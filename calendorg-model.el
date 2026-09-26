;;; calendorg-model.el --- Schedule parsing and persistence -*- lexical-binding: t; -*-

;; Parses the format described in schedule-format.org.  Three files share one
;; grammar.  The schedule is a template you write: weekday blocks repeat every
;; week, or on alternate weeks under Week 1 and Week 2.  The plan is ours:
;; dated allocations, one-off events and skipped occurrences for this week and
;; later.  The log is ours too and only grows: each week is frozen into it in
;; full once it ends, so history never shifts when the template changes, along
;; with the carry-over at each boundary that week.
;;
;; Dates are absolute day numbers, as in calendar.el.  A block's date is the
;; column it sits in, which after midnight is the previous day's.  A moment is
;; a date and a grid-space minute folded into one number of minutes.

;;; Code:

(require 'cl-lib)
(require 'calendar)

(defgroup calendorg nil
  "Weekly schedule viewer."
  :group 'calendar
  :prefix "calendorg-")

(defcustom calendorg-schedule-dir
  (or (getenv "SCHEDULE_DIR") "~/nextcloud-sync/todo/")
  "Directory holding the schedule, plan and log.
Defaults to the SCHEDULE_DIR environment variable when it is set."
  :type 'directory :group 'calendorg)

(defcustom calendorg-schedule-file "schedule.org"
  "Hand-authored weekly template.  Never written to.
Resolved against `calendorg-schedule-dir' unless absolute."
  :type 'file :group 'calendorg)

(defcustom calendorg-blocks-file "calendorg-blocks.org"
  "The plan: dated allocations, events and skips.  Rewritten by Calendorg.
Resolved against `calendorg-schedule-dir' unless absolute."
  :type 'file :group 'calendorg)

(defcustom calendorg-log-file "calendorg-log.org"
  "Past weeks frozen in full, and carry-over.  Appended by Calendorg.
Resolved against `calendorg-schedule-dir' unless absolute."
  :type 'file :group 'calendorg)

(defun calendorg--path (file)
  (expand-file-name file (expand-file-name calendorg-schedule-dir)))

(defun calendorg-schedule-path ()
  "Absolute path of the hand-authored schedule."
  (calendorg--path calendorg-schedule-file))

(defun calendorg-blocks-path ()
  "Absolute path of the plan Calendorg owns."
  (calendorg--path calendorg-blocks-file))

(defun calendorg-log-path ()
  "Absolute path of the log Calendorg appends to."
  (calendorg--path calendorg-log-file))

(defconst calendorg-days ["MON" "TUE" "WED" "THU" "FRI" "SAT" "SUN"])

(defconst calendorg-grid-start 480
  "First minute shown, 08:00.")

(defconst calendorg-grid-end 1500
  "Last minute shown, 01:00 the following day.")

(defconst calendorg-slot 15
  "Smallest allocatable unit, in minutes.")

(defconst calendorg-allocated-type "allocated"
  "Type token Calendorg writes.  Blocks of this type are mutable.")

(defconst calendorg--week 10080
  "Minutes in a week, which is how long every commitment's week lasts.")

(defconst calendorg--re-date "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")

(defconst calendorg--re-block
  (concat "^[ \t]*-[ \t]+\\([A-Z]\\{3\\}\\|" calendorg--re-date "\\)"
          "[ \t]+\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)-\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)"
          "[ \t]+\\([a-z][a-z-]*\\)"
          "\\(?:[ \t]+@\\([^ \t!]+\\)\\(?7:!\\)?\\)?"
          ;; Numbered explicitly: after the explicit group 7 above, an
          ;; implicit group would be numbered 8 and every label would vanish.
          "\\(?:[ \t]+\\(?6:.*?\\)\\)?[ \t]*$")
  "A block: weekday or date, span, type, then optional @COMMITMENT[!] and label.")

(defconst calendorg--re-skip
  (concat "^[ \t]*-[ \t]+\\(" calendorg--re-date "\\)"
          "[ \t]+\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)[ \t]+skip[ \t]*$")
  "A template occurrence left out, named by its date and start.")

(defconst calendorg--re-carry
  (concat "^[ \t]*-[ \t]+\\(" calendorg--re-date "\\)[ \t]+carry"
          "[ \t]+\\([^ \t]+\\)[ \t]+\\([-+]?[0-9]+\\(?:\\.[0-9]+\\)?\\)[ \t]*$")
  "Carry-over for a commitment as of a date.")

(defconst calendorg--re-comment    "^[ \t]+:[ \t]?\\(.*\\)$")
(defconst calendorg--re-commitment
  "^[ \t]*-[ \t]+\\([^ \t]+\\)[ \t]+\\([0-9]+\\(?:\\.[0-9]+\\)?\\)\\(?:[ \t]+\\(#[0-9a-fA-F]\\{6\\}\\)\\)?[ \t]*$")
(defconst calendorg--re-sleep      "^[ \t]*-[ \t]+\\(wake\\|sleep\\)[ \t]+\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)-\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)[ \t]*$")
(defconst calendorg--re-type       "^[ \t]*-[ \t]+\\([a-z][a-z-]*\\)[ \t]+\\(#[0-9a-fA-F]\\{6\\}\\)[ \t]*$")
(defconst calendorg--re-heading    "^\\*+[ \t]+\\(.*?\\)[ \t]*$")

(cl-defstruct (calendorg-block (:copier calendorg-block-copy))
  date         ; absolute day of its column; nil in the template
  day          ; template: weekday, 0 = Monday.  On screen: days from the
               ; Monday shown, negative for last week
  start end    ; minutes, grid space: may exceed 1440
  week         ; template only: nil for every week, else 1 or 2
  type
  commitment   ; token or nil
  boundary     ; t when written @TOKEN!, opening that commitment's week
  label        ; display string or nil
  comment      ; string or nil
  source)      ; `schedule' (fixed), `blocks' (the plan, ours) or `log'

(cl-defstruct calendorg-data
  commitments  ; list of (token hours color-or-nil)
  types        ; alist (token . "#rrggbb")
  wake sleep   ; (start . end) in grid space, or nil
  template     ; weekday blocks from the schedule
  week1        ; absolute Monday of a Week 1, or nil
  dated        ; dated blocks from the schedule and the plan
  skips        ; list of (DATE . START): template occurrences left out
  log          ; dated blocks from the log
  frozen       ; absolute Mondays of the weeks in the log
  carries      ; list of (DATE TOKEN VALUE) from the log
  warnings)    ; list of unparsed lines

;;; Times and dates

(defun calendorg--hhmm->min (s)
  (+ (* 60 (string-to-number (substring s 0 2)))
     (string-to-number (substring s 3 5))))

(defun calendorg--min->hhmm (m &optional past-midnight)
  "Format M as HH:MM.  With PAST-MIDNIGHT, 1440 prints as 24:00 rather
than 00:00, so a block running from before midnight to after it stays a
single positive span on disk."
  (let ((m (if past-midnight m (mod m 1440))))
    (format "%02d:%02d" (/ m 60) (mod m 60))))

(defun calendorg--date (s)
  "Absolute day number for S, \"YYYY-MM-DD\"."
  (calendar-absolute-from-gregorian
   (list (string-to-number (substring s 5 7))
         (string-to-number (substring s 8 10))
         (string-to-number (substring s 0 4)))))

(defun calendorg--date-string (date)
  "\"YYYY-MM-DD\" for absolute DATE."
  (let ((g (calendar-gregorian-from-absolute date)))
    (format "%04d-%02d-%02d" (nth 2 g) (nth 0 g) (nth 1 g))))

(defun calendorg--weekday (date)
  "0 for Monday through 6 for Sunday."
  (mod (1- date) 7))

(defun calendorg--monday (date)
  "Absolute Monday of DATE's week."
  (- date (calendorg--weekday date)))

(defun calendorg--moment (date minute)
  "DATE and a grid-space MINUTE of its column, as minutes since the epoch.
Grid space lets MINUTE run past 1440; that is simply the next morning."
  (+ (* date 1440) minute))

(defun calendorg--now ()
  "Current (DATE . MINUTE) in grid space.
The small hours belong to the preceding day's column, the same roll
`calendorg--normalize' applies to parsed times: at 00:06 Monday you are
at the bottom of Sunday, not the top of Monday.  Between the grid end and
the grid start there is no column to roll onto, so the calendar day
stands and callers find MINUTE outside the grid."
  (let* ((now (decode-time))
         (date (calendar-absolute-from-gregorian
                (list (nth 4 now) (nth 3 now) (nth 5 now))))
         (m (+ (* 60 (nth 2 now)) (nth 1 now))))
    (if (and (< m calendorg-grid-start)
             (<= (+ m 1440) calendorg-grid-end))
        (cons (1- date) (+ m 1440))
      (cons date m))))

(defun calendorg--now-moment ()
  (let ((now (calendorg--now)))
    (calendorg--moment (car now) (cdr now))))

(defun calendorg--start-moment (block)
  (calendorg--moment (calendorg-block-date block) (calendorg-block-start block)))

(defun calendorg--end-moment (block)
  (calendorg--moment (calendorg-block-date block) (calendorg-block-end block)))

(defun calendorg--hours (block)
  (/ (- (calendorg-block-end block) (calendorg-block-start block)) 60.0))

(defun calendorg--block< (a b)
  (let ((da (calendorg-block-day a)) (db (calendorg-block-day b)))
    (cond ((/= da db) (< da db))
          ((/= (calendorg-block-start a) (calendorg-block-start b))
           (< (calendorg-block-start a) (calendorg-block-start b)))
          (t (< (calendorg-block-end a) (calendorg-block-end b))))))

(defun calendorg--normalize (day start end &optional dated)
  "Roll times before the grid start onto the preceding column.
DAY is a weekday, so Monday wraps to Sunday, or with DATED an absolute
day, which just steps back one.  Returns (DAY START END) in grid space,
or nil when the span is invalid."
  (when (< start calendorg-grid-start)
    (setq day (if dated (1- day) (mod (1- day) 7))
          start (+ start 1440)
          end (+ end 1440)))
  (and (> end start)
       (<= end calendorg-grid-end)
       (list day start end)))

;;; Parsing

(defun calendorg--parse-block (line source week)
  "Build a block from LINE, which must already have matched the block regexp.
A dated line gives a dated block; a weekday line gives a template block
for WEEK, nil meaning every week."
  (let* ((head (match-string 1 line))
         (start (calendorg--hhmm->min (match-string 2 line)))
         (end (calendorg--hhmm->min (match-string 3 line)))
         (type (match-string 4 line))
         (commitment (match-string 5 line))
         (label (match-string 6 line))
         (boundary (and (match-string 7 line) t))
         (dated (string-match-p "\\`[0-9]" head))
         (day (if dated (calendorg--date head)
                (cl-position head calendorg-days :test #'equal)))
         (norm (and day (calendorg--normalize day start end dated))))
    (when norm
      (make-calendorg-block
       :date (and dated (nth 0 norm)) :day (unless dated (nth 0 norm))
       :start (nth 1 norm) :end (nth 2 norm) :week (unless dated week)
       :type type :commitment commitment :boundary boundary
       :label (unless (or (null label) (string-empty-p label)) label)
       :source source))))

(defun calendorg--file-monday (file)
  "Monday of the week FILE was last written in."
  (let ((tm (decode-time (file-attribute-modification-time (file-attributes file)))))
    (calendorg--monday (calendar-absolute-from-gregorian
                        (list (nth 4 tm) (nth 3 tm) (nth 5 tm))))))

(defun calendorg--parse-file (file source acc)
  "Parse FILE into ACC, a `calendorg-data'.
SOURCE is `schedule', `blocks' for the plan, or `log', and tags the blocks."
  (let ((file (expand-file-name file)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((section nil) (week nil) (last nil) (legacy nil))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (cond
               ((string-match calendorg--re-heading line)
                (let ((h (downcase (match-string 1 line))))
                  (cond
                   ;; A logged week: its blocks follow, and it is now frozen.
                   ((string-match (concat "\\`week of \\(" calendorg--re-date "\\)") h)
                    (push (calendorg--monday (calendorg--date (match-string 1 h)))
                          (calendorg-data-frozen acc))
                    (setq section "blocks" week nil))
                   ;; Alternating weeks nest under Blocks.  Week 1 carries the
                   ;; date that fixes which calendar weeks are which.
                   ((and (equal section "blocks")
                         (string-match "\\`week \\([12]\\)\\b" h))
                    (setq week (string-to-number (match-string 1 h)))
                    (when (and (= week 1)
                               (string-match (concat "<\\(" calendorg--re-date "\\)") h))
                      (setf (calendorg-data-week1 acc)
                            (calendorg--monday (calendorg--date (match-string 1 h))))))
                   (t (setq section h week nil)))))
               ((string-match "\\`[ \t]*\\'" line) nil)
               ((string-prefix-p "#" line) nil)

               ;; A comment attaches to the block above it.
               ((and (equal section "blocks")
                     (string-match calendorg--re-comment line))
                (when last
                  (setf (calendorg-block-comment last) (match-string 1 line))))

               ((and (equal section "blocks")
                     (string-match calendorg--re-skip line))
                (let ((date (calendorg--date (match-string 1 line)))
                      (start (calendorg--hhmm->min (match-string 2 line))))
                  (push (if (< start calendorg-grid-start)
                            (cons (1- date) (+ start 1440))
                          (cons date start))
                        (calendorg-data-skips acc))))

               ((and (equal section "blocks")
                     (string-match calendorg--re-carry line))
                (push (list (calendorg--date (match-string 1 line))
                            (match-string 2 line)
                            (string-to-number (match-string 3 line)))
                      (calendorg-data-carries acc)))

               ((equal section "blocks")
                (let ((b (and (string-match calendorg--re-block line)
                              (calendorg--parse-block line source week))))
                  (cond
                   ((null b) (push line (calendorg-data-warnings acc)))
                   ((calendorg-block-date b)
                    (if (eq source 'log)
                        (push b (calendorg-data-log acc))
                      (push b (calendorg-data-dated acc)))
                    (setq last b))
                   ((eq source 'schedule)
                    (push b (calendorg-data-template acc))
                    (setq last b))
                   ((eq source 'blocks)
                    ;; A weekday line from before blocks were dated.  It was
                    ;; planned for the week the file was last written in, so
                    ;; it lands there and is written back dated.
                    (unless legacy (setq legacy (calendorg--file-monday file)))
                    (setf (calendorg-block-date b) (+ legacy (calendorg-block-day b)))
                    (push b (calendorg-data-dated acc))
                    (setq last b))
                   (t (push line (calendorg-data-warnings acc))))))

               ((equal section "commitments")
                (if (string-match calendorg--re-commitment line)
                    (push (list (match-string 1 line)
                                (string-to-number (match-string 2 line))
                                (match-string 3 line))
                          (calendorg-data-commitments acc))
                  (push line (calendorg-data-warnings acc))))

               ((equal section "sleep")
                (if (string-match calendorg--re-sleep line)
                    (let* ((kind (match-string 1 line))
                           (a (calendorg--hhmm->min (match-string 2 line)))
                           (b (calendorg--hhmm->min (match-string 3 line)))
                           (span (if (< a calendorg-grid-start)
                                     (cons (+ a 1440) (+ b 1440))
                                   (cons a b))))
                      (if (equal kind "wake")
                          (setf (calendorg-data-wake acc) span)
                        (setf (calendorg-data-sleep acc) span)))
                  (push line (calendorg-data-warnings acc))))

               ((equal section "types")
                (if (string-match calendorg--re-type line)
                    (push (cons (match-string 1 line) (match-string 2 line))
                          (calendorg-data-types acc))
                  (push line (calendorg-data-warnings acc))))))
            (forward-line 1))))))
  acc)

(defun calendorg-load ()
  "Read the schedule, the plan and the log into a `calendorg-data'."
  (let ((acc (make-calendorg-data)))
    (calendorg--parse-file (calendorg-schedule-path) 'schedule acc)
    (calendorg--parse-file (calendorg-blocks-path) 'blocks acc)
    (calendorg--parse-file (calendorg-log-path) 'log acc)
    (setf (calendorg-data-commitments acc) (nreverse (calendorg-data-commitments acc))
          (calendorg-data-types acc) (nreverse (calendorg-data-types acc))
          (calendorg-data-warnings acc) (nreverse (calendorg-data-warnings acc)))
    acc))

;;; Weeks

(defun calendorg--parity (data monday)
  "1 or 2 for the week starting MONDAY, or nil without a Week 1 date."
  (let ((w1 (calendorg-data-week1 data)))
    (and w1 (1+ (mod (/ (- monday w1) 7) 2)))))

(defun calendorg-week (data monday)
  "Blocks for the week starting MONDAY, dated, in no particular order.
A week in the log comes from there alone, so history stays as it was when
the template changes.  Otherwise it is the template, with the Week 1 or
Week 2 blocks for its parity and without skipped occurrences, plus the
dated blocks that fall in it.  Dated blocks are returned as themselves,
so callers can change them in place; template ones are fresh copies."
  (let* ((end (+ monday 7))
         (in (lambda (b) (and (>= (calendorg-block-date b) monday)
                              (< (calendorg-block-date b) end)))))
    (if (memql monday (calendorg-data-frozen data))
        (cl-remove-if-not in (calendorg-data-log data))
      (let ((parity (calendorg--parity data monday))
            (skips (calendorg-data-skips data))
            (out nil))
        (dolist (b (calendorg-data-template data))
          (let ((date (+ monday (calendorg-block-day b))))
            (when (and (memql (calendorg-block-week b) (list nil parity))
                       (not (member (cons date (calendorg-block-start b)) skips)))
              (let ((i (calendorg-block-copy b)))
                (setf (calendorg-block-date i) date
                      (calendorg-block-week i) nil)
                (push i out)))))
        (dolist (b (calendorg-data-dated data))
          (when (and (funcall in b)
                     (not (and (eq (calendorg-block-source b) 'schedule)
                               (member (cons (calendorg-block-date b)
                                             (calendorg-block-start b))
                                       skips))))
            (push b out)))
        out))))

(defun calendorg-range (data from to)
  "Blocks whose column dates fall in [FROM, TO), across as many weeks."
  (let ((out nil))
    (cl-loop for monday from (calendorg--monday from) below to by 7
             do (dolist (b (calendorg-week data monday))
                  (when (and (>= (calendorg-block-date b) from)
                             (< (calendorg-block-date b) to))
                    (push b out))))
    out))

(defun calendorg--epoch (data)
  "Monday history starts from, or nil when nothing is dated yet.
The earliest logged week, dated block, or skip: the first week actually
planned here.  Weeks before it are never frozen, and carry-over never
charges for them.  The Week 1 date is deliberately not a candidate: it
only fixes which weeks alternate, and a Monday months back would
otherwise freeze and charge every week since."
  (let ((dates (append (calendorg-data-frozen data)
                       (mapcar #'calendorg-block-date (calendorg-data-dated data))
                       (mapcar #'car (calendorg-data-skips data)))))
    (and dates (calendorg--monday (apply #'min dates)))))

;;; Commitments

(defun calendorg-commitment-color (data token)
  "Colour declared for TOKEN, or nil when it did not name one."
  (nth 2 (assoc token (calendorg-data-commitments data))))

(defun calendorg--boundaries (data token from to)
  "Start moments of TOKEN's boundaries in columns dated [FROM, TO), ascending.
A commitment with no boundary anywhere starts each week on Monday at the
grid start instead."
  (if (cl-some (lambda (b) (and (calendorg-block-boundary b)
                                (equal (calendorg-block-commitment b) token)))
               (append (calendorg-data-template data) (calendorg-data-dated data)
                       (calendorg-data-log data)))
      (sort (cl-loop for b in (calendorg-range data from to)
                     when (and (calendorg-block-boundary b)
                               (equal (calendorg-block-commitment b) token))
                     collect (calendorg--start-moment b))
            #'<)
    (cl-loop for monday from (calendorg--monday (+ from 6)) below to by 7
             collect (calendorg--moment monday calendorg-grid-start))))

(defun calendorg-period (data token now)
  "TOKEN's current week as (START . END) moments around NOW, a moment.
It runs from TOKEN's most recent boundary at or before NOW to its next
one: seven days, unless a boundary was skipped, when it runs on to the
next that happens.  The target stays a single week's either way."
  (let* ((date (/ now 1440))
         (bs (calendorg--boundaries data token (- date 21) (+ date 22)))
         (start (or (car (last (cl-remove-if (lambda (m) (> m now)) bs)))
                    (calendorg--moment (calendorg--monday date) calendorg-grid-start))))
    (cons start (or (cl-find-if (lambda (m) (> m now)) bs)
                    (+ start calendorg--week)))))

(defun calendorg--sums (data token start end now)
  "(DONE ALLOCATED SPOKEN-FOR) hours for TOKEN over moments [START, END).
A block belongs where it starts.  DONE is the allocated blocks that have
ended by NOW: a block in progress counts nothing yet.  SPOKEN-FOR is every
other block naming TOKEN, meetings and the like, whose hours come off
the target."
  (let ((done 0.0) (allocated 0.0) (spoken 0.0))
    (dolist (b (calendorg-range data (1- (/ start 1440)) (+ 2 (/ end 1440))))
      (let ((s (calendorg--start-moment b)))
        (when (and (equal (calendorg-block-commitment b) token)
                   (>= s start) (< s end))
          (let ((h (calendorg--hours b)))
            (if (equal (calendorg-block-type b) calendorg-allocated-type)
                (progn (cl-incf allocated h)
                       (when (<= (calendorg--end-moment b) now)
                         (cl-incf done h)))
              (cl-incf spoken h))))))
    (list done allocated spoken)))

(defun calendorg--target (data token spoken)
  "TOKEN's declared weekly hours less SPOKEN, the hours already spoken for."
  (- (nth 1 (assoc token (calendorg-data-commitments data))) spoken))

(defun calendorg--delta (data token start end)
  "Target minus done for TOKEN's week of moments [START, END).
Zero when that week began before history did."
  (let ((epoch (calendorg--epoch data)))
    (if (or (null epoch) (< start (calendorg--moment epoch 0)))
        0.0
      (let ((sums (calendorg--sums data token start end end)))
        (- (calendorg--target data token (nth 2 sums)) (nth 0 sums))))))

(defun calendorg-carry (data token now)
  "TOKEN's carry-over at NOW, a moment: hours owed from closed weeks.
Negative when ahead.  It starts from the latest carry line in the log for
TOKEN, or zero, and adds the shortfall of each week closed since that
line's date, a week running from one boundary to the next.  Editing that
line is how to reset it."
  (let* ((today (/ now 1440))
         (line (car (sort (cl-remove-if-not
                           (lambda (c) (and (equal (nth 1 c) token)
                                            (<= (nth 0 c) today)))
                           (calendorg-data-carries data))
                          (lambda (a b) (> (car a) (car b))))))
         (value (if line (float (nth 2 line)) 0.0))
         (from (if line (1+ (nth 0 line)) (calendorg--epoch data))))
    (when from
      (let ((prev nil))
        (dolist (b (calendorg--boundaries data token (- from 21) (1+ today)))
          (when (and (>= (/ b 1440) from) (<= b now))
            (setq value (+ value (calendorg--delta data token
                                                   (or prev (- b calendorg--week)) b))))
          (setq prev b))))
    value))

(defun calendorg-stats (data &optional now)
  "Return a list of (TOKEN DONE ALLOCATED TARGET CARRY) in declaration order.
Each commitment is measured over its own week (`calendorg-period') at
NOW, a moment, defaulting to the present.

ALLOCATED is the allocated hours in that week.  DONE is those whose blocks
have ended, on the honour system: nothing is logged, and you trim a block
you did not finish.  TARGET is the declared weekly budget less the hours
of every other block that week naming the commitment.  CARRY is what is
owed from earlier weeks (`calendorg-carry')."
  (let ((now (or now (calendorg--now-moment))))
    (mapcar
     (lambda (c)
       (let* ((token (car c))
              (period (calendorg-period data token now))
              (sums (calendorg--sums data token (car period) (cdr period) now)))
         (list token (nth 0 sums) (nth 1 sums)
               (calendorg--target data token (nth 2 sums))
               (calendorg-carry data token now))))
     (calendorg-data-commitments data))))

;;; Writing

(defun calendorg--block->line (b)
  "Render B back to source syntax, undoing the grid-space roll: a block
starting past midnight is written on the following date or weekday."
  (let* ((date (calendorg-block-date b))
         (day (or date (calendorg-block-day b)))
         (start (calendorg-block-start b))
         (end (calendorg-block-end b)))
    (when (>= start 1440)
      (setq day (if date (1+ day) (mod (1+ day) 7))))
    (concat "- " (if date (calendorg--date-string day) (aref calendorg-days day))
            " " (calendorg--min->hhmm start)
            "-" (calendorg--min->hhmm end (and (< start 1440) (>= end 1440)))
            " " (calendorg-block-type b)
            (if (calendorg-block-commitment b)
                (concat " @" (calendorg-block-commitment b)
                        (if (calendorg-block-boundary b) "!" ""))
              "")
            (if (calendorg-block-label b)
                (concat " " (calendorg-block-label b)) ""))))

(defun calendorg--skip->line (skip)
  "Render SKIP, a (DATE . START), undoing the grid-space roll."
  (let ((date (car skip)) (start (cdr skip)))
    (when (>= start 1440) (setq date (1+ date)))
    (format "- %s %s skip" (calendorg--date-string date) (calendorg--min->hhmm start))))

(defun calendorg--insert-block (b)
  (insert (calendorg--block->line b) "\n")
  (when (calendorg-block-comment b)
    (insert "  : " (calendorg-block-comment b) "\n")))

(defun calendorg-save (data)
  "Rewrite the plan from DATA's own dated blocks and its skips."
  (let ((file (calendorg-blocks-path)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert "#+title: Calendorg blocks\n"
              "# Written by Calendorg.  Edits here are kept, but formatting is not.\n\n"
              "* Blocks\n")
      (dolist (b (sort (cl-remove-if-not (lambda (b) (eq (calendorg-block-source b) 'blocks))
                                         (calendorg-data-dated data))
                       (lambda (a b) (< (calendorg--start-moment a)
                                        (calendorg--start-moment b)))))
        (calendorg--insert-block b))
      (dolist (s (sort (copy-sequence (calendorg-data-skips data))
                       (lambda (a b) (< (calendorg--moment (car a) (cdr a))
                                        (calendorg--moment (car b) (cdr b))))))
        (insert (calendorg--skip->line s) "\n")))))

(defun calendorg--append-week (monday parity blocks carries)
  "Append the week starting MONDAY to the log: BLOCKS, then CARRIES."
  (let ((file (calendorg-log-path)))
    (make-directory (file-name-directory file) t)
    (with-temp-buffer
      (unless (file-exists-p file)
        (insert "#+title: Calendorg log\n"
                "# Past weeks, frozen by Calendorg when they end.  Edit freely.\n"))
      (insert "\n* Week of " (calendorg--date-string monday)
              (if parity (format " (week %d)" parity) "") "\n")
      (dolist (b blocks) (calendorg--insert-block b))
      (dolist (c carries)
        (insert (format "- %s carry %s %+g\n" (calendorg--date-string (nth 0 c))
                        (nth 1 c) (/ (round (* 100 (nth 2 c))) 100.0))))
      (write-region (point-min) (point-max) file t 'quiet))))

(defun calendorg-freeze (data)
  "Freeze every week that has ended into the log.  Non-nil when any was.
Each is written in full, template and all, followed by a carry line for
every boundary in it, and then its dated blocks and skips leave the plan.
Carry lines wait for the week to freeze rather than the boundary to pass,
so a block trimmed after the boundary still counts.  Weeks before
`calendorg--epoch' are left alone: nothing was planned in them here.
DATA is updated to match what was written."
  (let* ((epoch (calendorg--epoch data))
         (this (calendorg--monday (car (calendorg--now))))
         (weeks (and epoch
                     (cl-loop for m from epoch below this by 7
                              unless (memql m (calendorg-data-frozen data))
                              collect m))))
    (dolist (m weeks)
      (let ((blocks (sort (calendorg-week data m)
                          (lambda (a b) (< (calendorg--start-moment a)
                                           (calendorg--start-moment b)))))
            (carries nil))
        ;; While the week still resolves live, carry at each boundary in it.
        ;; Each line joins DATA at once, so the next builds on it.
        (dolist (c (calendorg-data-commitments data))
          (dolist (b (calendorg--boundaries data (car c) m (+ m 7)))
            (let ((entry (list (/ b 1440) (car c) (calendorg-carry data (car c) b))))
              (push entry carries)
              (push entry (calendorg-data-carries data)))))
        (calendorg--append-week m (calendorg--parity data m) blocks (nreverse carries))
        (push m (calendorg-data-frozen data))
        (dolist (b blocks)
          (let ((copy (calendorg-block-copy b)))
            (setf (calendorg-block-source copy) 'log)
            (push copy (calendorg-data-log data))))
        (let ((in (lambda (date) (and (>= date m) (< date (+ m 7))))))
          (setf (calendorg-data-dated data)
                (cl-remove-if (lambda (b) (and (eq (calendorg-block-source b) 'blocks)
                                               (funcall in (calendorg-block-date b))))
                              (calendorg-data-dated data))
                (calendorg-data-skips data)
                (cl-remove-if (lambda (s) (funcall in (car s)))
                              (calendorg-data-skips data))))))
    (when weeks
      (calendorg-save data)
      t)))

(provide 'calendorg-model)
;;; calendorg-model.el ends here
