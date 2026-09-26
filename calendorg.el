;;; calendorg.el --- Weekly schedule viewer -*- lexical-binding: t; -*-

;; Author: Lorenzo Hess
;; Package-Requires: ((emacs "29.1"))

;; Renders the schedule in `calendorg-schedule-dir' as an SVG week and allocates
;; blocks into the gaps.  This week and next are on screen, Monday to Sunday,
;; `w' hiding next week and `[' and `]' moving a week at a time.  The view also
;; reaches back to where the week closing at the next boundary (@TOKEN!)
;; began.  Each commitment's hours are counted between its boundaries, with any
;; shortfall carried over.  Selection always sits on a
;; block; `V' places a time cursor and `v' selects a range.  See
;; schedule-format.org for the data format.

;;; Code:

(require 'cl-lib)
(require 'calendorg-model)
(require 'calendorg-nav)
(require 'calendorg-render)

(defconst calendorg-buffer-name "*calendorg*")

(defvar-local calendorg--data nil)
(defvar-local calendorg--week-start nil "Absolute Monday of the week on screen.")
(defvar-local calendorg--blocks nil
  "Vector of the week's blocks, sorted, with days counted from its Monday.")
(defvar-local calendorg--weeks 2 "Weeks on screen: 2, or 1 with next week hidden.")
(defvar-local calendorg--back 0 "How many of last week's columns are drawn first.")
(defvar-local calendorg--back-blocks nil "Last week's blocks in those columns.")
(defvar-local calendorg--ranges nil)
(defvar-local calendorg--nav nil)
(defvar-local calendorg--sel nil "Index into `calendorg--blocks'.")
(defvar-local calendorg--vsel nil
  "Plist (:day :anchor :point [:cursor t]) while picking a time.
With :cursor the two ends are held together and drawn as a single line;
`v' drops the flag to start extending a range.")
(defvar-local calendorg--vsel-exit nil
  "Thunk that removes the time-select transient map.
Kept so prompts can drop the map before reading the minibuffer, where
`calendorg--vsel' is not bound and its motion keys would error.")
(defvar-local calendorg--geom nil "Geometry of the last render, for hit-testing.")
(defvar-local calendorg--last-size nil)
(defvar-local calendorg--last-image nil
  "Previous image, flushed on redraw.
Every redraw builds a new spec, so without this each one leaves its
rasterised bitmap in the image cache until the eviction delay expires.")

;;; State

(defun calendorg--selected ()
  (when (and calendorg--sel (< calendorg--sel (length calendorg--blocks)))
    (aref calendorg--blocks calendorg--sel)))

(defun calendorg--mutable ()
  "Selected block, erroring unless we own it."
  (let ((b (calendorg--selected)))
    (unless b (user-error "No block selected"))
    (pcase (calendorg-block-source b)
      ('log (user-error "%s is in the log; edit %s by hand"
                        (calendorg-block-title b) (calendorg-log-path)))
      ('schedule (user-error "%s is fixed; edit %s by hand"
                             (calendorg-block-title b) (calendorg-schedule-path))))
    b))

(defun calendorg--now-index ()
  "Index of the block happening now, else the next one, else the first."
  (let ((now (calendorg--now-moment))
        (n (length calendorg--blocks)))
    (or (cl-loop for i below n
                 for b = (aref calendorg--blocks i)
                 when (and (<= (calendorg--start-moment b) now)
                           (< now (calendorg--end-moment b)))
                 return i)
        (cl-loop for i below n
                 when (>= (calendorg--start-moment (aref calendorg--blocks i)) now)
                 return i)
        0)))

(defun calendorg--lay-out (blocks)
  "BLOCKS with days counted from the week on screen, sorted."
  (dolist (b blocks)
    (setf (calendorg-block-day b) (- (calendorg-block-date b) calendorg--week-start)))
  (sort blocks #'calendorg--block<))

(defun calendorg--view ()
  "What `calendorg-render' draws."
  (list :week-start calendorg--week-start :days (* 7 calendorg--weeks)
        :blocks calendorg--blocks
        :back calendorg--back :back-blocks calendorg--back-blocks))

(defun calendorg--rebuild (&optional keep)
  "Reload from disk, freezing any week that has ended, and lay out the weeks
on screen.  KEEP is a (DATE . START) to reselect if still present.
With this week on screen, the view also reaches back to where the week
closing at the next boundary began, so all of it is visible.  Once a
day's boundaries have passed, the next is later on and it reaches back
less, or not at all."
  (setq calendorg--data (calendorg-load))
  (when (calendorg-freeze calendorg--data)
    (setq calendorg--data (calendorg-load)))
  (let ((today (car (calendorg--now))))
    (unless calendorg--week-start
      (setq calendorg--week-start (calendorg--monday today)))
    (setq calendorg--blocks
          (vconcat (calendorg--lay-out
                    (cl-loop for w below calendorg--weeks
                             append (calendorg-week calendorg--data
                                                    (+ calendorg--week-start (* 7 w)))))))
    (let* ((now (calendorg--now-moment))
           (next (car (sort (cl-loop for b in (calendorg-range calendorg--data today (+ today 8))
                                     when (and (calendorg-block-boundary b)
                                               (> (calendorg--start-moment b) now))
                                     collect b)
                            (lambda (a b) (< (calendorg--start-moment a)
                                             (calendorg--start-moment b))))))
           (reach (and next
                       (= calendorg--week-start (calendorg--monday today))
                       (< (- (calendorg-block-date next) 7) calendorg--week-start)
                       (- (calendorg-block-date next) 7))))
      (setq calendorg--back (if reach (- calendorg--week-start reach) 0)
            calendorg--back-blocks
            (and reach (calendorg--lay-out
                        (calendorg-range calendorg--data reach calendorg--week-start))))))
  (setq calendorg--ranges (calendorg-day-ranges calendorg--blocks (* 7 calendorg--weeks))
        calendorg--nav (calendorg-build-nav calendorg--blocks calendorg--ranges))
  (setq calendorg--sel
        (cond
         ((zerop (length calendorg--blocks)) nil)
         (keep (or (cl-loop for i below (length calendorg--blocks)
                            for b = (aref calendorg--blocks i)
                            when (and (= (calendorg-block-date b) (car keep))
                                      (= (calendorg-block-start b) (cdr keep)))
                            return i)
                   (calendorg--now-index)))
         ((and calendorg--sel (< calendorg--sel (length calendorg--blocks)))
          calendorg--sel)
         (t (calendorg--now-index))))
  (when (calendorg-data-warnings calendorg--data)
    (message "calendorg: %d unparsed line(s); see calendorg-report-warnings"
             (length (calendorg-data-warnings calendorg--data)))))

(defun calendorg-report-warnings ()
  "List lines the parser could not read."
  (interactive)
  (let ((w (and calendorg--data (calendorg-data-warnings calendorg--data))))
    (if (null w)
        (message "calendorg: everything parsed")
      (with-current-buffer (get-buffer-create "*calendorg warnings*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (l w) (insert l "\n"))
          (special-mode))
        (display-buffer (current-buffer))))))

;;; Text below the calendar

(defun calendorg--insert-centered (lines)
  "Insert LINES centred as a block, aligned on the widest one."
  (when lines
    (let* ((wide (apply #'max (mapcar (lambda (l) (string-width (substring-no-properties l)))
                                      lines)))
           (pad (make-string (max 0 (/ (- (window-width) wide) 2)) ?\s)))
      (dolist (l lines) (insert pad l "\n")))))

(defun calendorg--insert-detail ()
  (let ((b (calendorg--selected)))
    (if (null b)
        (calendorg--insert-centered
         (list (propertize
                (if (file-readable-p (calendorg-schedule-path))
                    "No blocks parsed. Does the file match schedule-format.org?"
                  (format "%s not found." (calendorg-schedule-path)))
                'face 'shadow)))
      (calendorg--insert-centered
       (list (propertize
              (format "%s · %s %s–%s · %s%s"
                      (calendorg-block-title b)
                      (calendorg-date-label (calendorg-block-date b))
                      (calendorg--min->hhmm (calendorg-block-start b))
                      (calendorg--min->hhmm (calendorg-block-end b))
                      (calendorg-block-type b)
                      (if (calendorg-block-commitment b)
                          (concat " @" (calendorg-block-commitment b)) ""))
              'face `(:foreground ,(calendorg--tint
                                    (calendorg-block-color calendorg--data b)
                                    0.5)))))
      (calendorg--insert-centered
       (list (propertize (or (calendorg-block-comment b) "") 'face 'shadow))))))

(defun calendorg--faded (hex)
  "HEX pushed most of the way toward the background, for hours still ahead.
Faces have no alpha, so transparency is faked by mixing."
  (let ((bg (face-background 'default nil t)))
    (calendorg--mix hex (if (and bg (string-prefix-p "#" bg)) bg calendorg-bg)
                    0.6)))

(defun calendorg--insert-stats ()
  "One line per commitment, measured over its own week (`calendorg-stats').
What is owed is the target plus carry-over.  The bar is scaled to it:
solid for blocks already finished, faded for hours allocated but still
ahead, hollow for what is left.  Green once the allocation covers it,
amber until then.  Carry-over shows as \"+5 unmet\", or negative when ahead."
  (let ((stats (calendorg-stats calendorg--data))
        (width 18))
    (when stats
      (let ((pad (apply #'max (mapcar (lambda (s) (length (car s))) stats))))
        (calendorg--insert-centered
         (mapcar
          (lambda (s)
            (cl-destructuring-bind (token done allocated target carry) s
              (let* ((owed (+ target carry))
                     (cells (lambda (h) (if (> owed 0)
                                            (min width (max 0 (round (* width (/ h owed)))))
                                          width)))
                     (solid (funcall cells done))
                     (planned (max solid (funcall cells allocated)))
                     (met (>= allocated owed))
                     (color (if met "#98be65" "#ECBE7B")))
                (concat (format (format "%%-%ds  " pad) token)
                        (propertize (make-string solid ?█) 'face `(:foreground ,color))
                        (propertize (make-string (- planned solid) ?█)
                                    'face `(:foreground ,(calendorg--faded color)))
                        (propertize (make-string (- width planned) ?░) 'face 'shadow)
                        (propertize (format "  %.4g / %.4g h" allocated target)
                                    'face `(:foreground ,color))
                        (propertize (format "  %.4g done" done) 'face 'shadow)
                        (if (zerop (/ (round (* 100 carry)) 100.0)) ""
                          (propertize (format "  %+.4g unmet" carry)
                                      'face `(:foreground ,color)))))))
          stats))))))

;;; Redisplay

(defun calendorg--canvas-size ()
  (let* ((w (max 420 (- (window-body-width nil t) 4)))
         (lines (+ 4 (length (calendorg-data-commitments calendorg--data))))
         (h (max 260 (- (window-body-height nil t)
                        (* lines (default-line-height)) 8))))
    (cons w h)))

(defun calendorg--redisplay ()
  (let* ((inhibit-read-only t)
         (size (calendorg--canvas-size))
         (w (car size)) (h (cdr size)))
    ;; Same columns `calendorg-render' draws, or clicks land in the wrong one.
    (setq calendorg--geom (calendorg-geometry w h calendorg--back calendorg--week-start
                                              (* 7 calendorg--weeks))
          calendorg--last-size (cons (window-body-width nil t)
                                     (window-body-height nil t)))
    (erase-buffer)
    (when (and calendorg--last-image (display-graphic-p))
      (image-flush calendorg--last-image))
    (let ((start (point))
          (img (create-image (calendorg-render calendorg--data (calendorg--view)
                                               calendorg--sel calendorg--vsel w h)
                             'svg t :scale 1)))
      (setq calendorg--last-image img)
      (insert-image img)
      ;; `insert-image' hangs `image-map' off the image as a text property, and a
      ;; text-property keymap outranks every other map -- including our own and
      ;; evil's.  That is what swallows `i' (the image-transform prefix).
      (remove-text-properties start (point) '(keymap nil)))
    (insert "\n\n")
    (calendorg--insert-detail)
    (insert "\n")
    (calendorg--insert-stats)
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun calendorg--on-resize (_frame)
  (when-let* ((win (get-buffer-window calendorg-buffer-name)))
    (with-current-buffer calendorg-buffer-name
      (let ((now (cons (window-body-width nil t) (window-body-height nil t))))
        (unless (equal now calendorg--last-size)
          (calendorg--redisplay))))))

;;; Navigation

(defun calendorg--move (direction)
  (when calendorg--sel
    (setq calendorg--sel (calendorg-nav-move calendorg--nav calendorg--sel direction))
    (calendorg--redisplay)))

(defun calendorg-left ()  (interactive) (calendorg--move calendorg-nav-h))
(defun calendorg-down ()  (interactive) (calendorg--move calendorg-nav-j))
(defun calendorg-up ()    (interactive) (calendorg--move calendorg-nav-k))
(defun calendorg-right () (interactive) (calendorg--move calendorg-nav-l))

(defun calendorg-next ()
  "Next block chronologically, wrapping."
  (interactive)
  (when (> (length calendorg--blocks) 0)
    (setq calendorg--sel (mod (1+ (or calendorg--sel -1)) (length calendorg--blocks)))
    (calendorg--redisplay)))

(defun calendorg-previous ()
  (interactive)
  (when (> (length calendorg--blocks) 0)
    (setq calendorg--sel (mod (1- (or calendorg--sel 1)) (length calendorg--blocks)))
    (calendorg--redisplay)))

(defun calendorg-first ()
  "Select the first block of the week."
  (interactive)
  (when (> (length calendorg--blocks) 0)
    (setq calendorg--sel 0) (calendorg--redisplay)))

(defun calendorg-last ()
  "Select the last block of the week."
  (interactive)
  (when (> (length calendorg--blocks) 0)
    (setq calendorg--sel (1- (length calendorg--blocks))) (calendorg--redisplay)))

(defun calendorg--shift-week (weeks)
  (setq calendorg--week-start (+ calendorg--week-start (* 7 weeks))
        calendorg--sel nil)
  (calendorg--rebuild)
  (calendorg--redisplay))

(defun calendorg-toggle-next-week ()
  "Hide or show next week beside this one."
  (interactive)
  (let ((b (calendorg--selected)))
    (setq calendorg--weeks (if (= calendorg--weeks 2) 1 2))
    (calendorg--rebuild (and b (cons (calendorg-block-date b) (calendorg-block-start b))))
    (calendorg--redisplay)))

(defun calendorg-next-week ()
  "Show the following week."
  (interactive)
  (calendorg--shift-week 1))

(defun calendorg-previous-week ()
  "Show the preceding week."
  (interactive)
  (calendorg--shift-week -1))

(defun calendorg-day-start ()
  (interactive)
  (when-let* ((b (calendorg--selected))
              (r (aref calendorg--ranges (calendorg-block-day b))))
    (setq calendorg--sel (car r)) (calendorg--redisplay)))

(defun calendorg-day-end ()
  (interactive)
  (when-let* ((b (calendorg--selected))
              (r (aref calendorg--ranges (calendorg-block-day b))))
    (setq calendorg--sel (cdr r)) (calendorg--redisplay)))

(defun calendorg-mouse-select (event)
  "Select the block under the click."
  (interactive "e")
  (when-let* ((posn (event-start event))
              (xy (posn-object-x-y posn))
              (geom calendorg--geom))
    ;; Emacs may display the SVG at a size other than the one we drew it at,
    ;; so map click pixels back into geometry space before inverting.
    (let* ((shown (ignore-errors (image-size (posn-image posn) t)))
           (sx (if (and shown (> (car shown) 0))
                   (/ (float (plist-get geom :width)) (car shown)) 1.0))
           (sy (if (and shown (> (cdr shown) 0))
                   (/ (float (plist-get geom :height)) (cdr shown)) 1.0))
           (day (calendorg-day-at geom (* sx (car xy))))
           (m (and day (calendorg-minute-at geom (* sy (cdr xy))))))
      (when day
        (cl-loop for i below (length calendorg--blocks)
                 for b = (aref calendorg--blocks i)
                 when (and (= (calendorg-block-day b) day)
                           (>= m (calendorg-block-start b))
                           (< m (calendorg-block-end b)))
                 do (setq calendorg--sel i)
                 and return nil)
        (calendorg--redisplay)))))

;;; Editing

(defconst calendorg--re-spec
  "\\`[ \t]*\\([A-Za-z]\\{3\\}\\)[ \t]+\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)-\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)[ \t]*\\'"
  "What the user types when giving a day and span directly.")

(defun calendorg--spec-string (b)
  "B as \"DAY HH:MM-HH:MM\" on its own column, past midnight as 24:30, so
reading it back gives the same block."
  (format "%s %s-%s"
          (aref calendorg-days (calendorg--weekday (calendorg-block-date b)))
          (calendorg--min->hhmm (calendorg-block-start b) t)
          (calendorg--min->hhmm (calendorg-block-end b) t)))

(defun calendorg--read-spec (prompt initial)
  "Read \"DAY HH:MM-HH:MM\" and return (DAY START END) in grid space.
DAY counts from a Monday, so a time before the grid start on Monday is
-1, the previous Sunday's column."
  (let* ((s (read-string prompt initial))
         (ok (string-match calendorg--re-spec s)))
    (unless ok (user-error "Expected DAY HH:MM-HH:MM, got %S" s))
    (let* ((day (cl-position (upcase (match-string 1 s)) calendorg-days :test #'equal))
           (start (calendorg--hhmm->min (format "%05s" (match-string 2 s))))
           (end (calendorg--hhmm->min (format "%05s" (match-string 3 s))))
           (norm (and day (calendorg--normalize day start end t))))
      (unless day (user-error "Unknown day %S" (match-string 1 s)))
      (unless norm (user-error "Span must be positive and end by %s"
                               (calendorg--min->hhmm calendorg-grid-end)))
      norm)))

(defun calendorg--read-commitment (&optional initial)
  (let ((tokens (mapcar #'car (calendorg-data-commitments calendorg--data))))
    (unless tokens (user-error "No commitments declared in %s" (calendorg-schedule-path)))
    (completing-read "Commitment: " tokens nil t initial)))

(defun calendorg--commit (keep)
  "Save the plan from `calendorg--data', reload, and reselect KEEP."
  (calendorg-save calendorg--data)
  (calendorg--rebuild keep)
  (calendorg--redisplay))

(defun calendorg-edit ()
  "Edit the selected block's day, span, and commitment."
  (interactive)
  (let* ((b (calendorg--mutable))
         (spec (calendorg--read-spec "Block: " (calendorg--spec-string b)))
         (commitment (if (equal (calendorg-block-type b) calendorg-allocated-type)
                         (calendorg--read-commitment (calendorg-block-commitment b))
                       (calendorg-block-commitment b))))
    ;; B is the plan's own object (`calendorg-week'), so this edits the plan.
    ;; The day is read within B's own week.
    (setf (calendorg-block-date b) (+ (calendorg--monday (calendorg-block-date b))
                                      (nth 0 spec))
          (calendorg-block-start b) (nth 1 spec)
          (calendorg-block-end b) (nth 2 spec)
          (calendorg-block-commitment b) commitment)
    (calendorg--commit (cons (calendorg-block-date b) (calendorg-block-start b)))))

(defun calendorg-delete ()
  "Delete the selected block, or skip this occurrence of a schedule block.
Skipping writes a dated skip line to the plan; the template is untouched,
and deleting the line brings the occurrence back."
  (interactive)
  (let ((b (calendorg--selected)))
    (unless b (user-error "No block selected"))
    (if (eq (calendorg-block-source b) 'schedule)
        (when (y-or-n-p (format "Skip %s on %s? " (calendorg-block-title b)
                                (calendorg-date-label (calendorg-block-date b))))
          (push (cons (calendorg-block-date b) (calendorg-block-start b))
                (calendorg-data-skips calendorg--data))
          (calendorg--commit nil))
      (setq b (calendorg--mutable))
      (when (y-or-n-p (format "Delete %s %s–%s? "
                              (calendorg-block-title b)
                              (calendorg--min->hhmm (calendorg-block-start b))
                              (calendorg--min->hhmm (calendorg-block-end b))))
        (setf (calendorg-data-dated calendorg--data)
              (delq b (calendorg-data-dated calendorg--data)))
        (calendorg--commit nil)))))

(defun calendorg-comment ()
  "Set the comment on the selected block."
  (interactive)
  (let* ((b (calendorg--mutable))
         (text (string-trim (read-string "Comment: " (calendorg-block-comment b)))))
    (setf (calendorg-block-comment b) (unless (string-empty-p text) text))
    (calendorg--commit (cons (calendorg-block-date b) (calendorg-block-start b)))))

;;; Time select

(defvar calendorg-time-select-map
  (let ((m (make-sparse-keymap)))
    (define-key m "j" #'calendorg-ts-grow)
    (define-key m "k" #'calendorg-ts-shrink)
    (define-key m "h" #'calendorg-ts-prev-day)
    (define-key m "l" #'calendorg-ts-next-day)
    (define-key m [down] #'calendorg-ts-grow)
    (define-key m [up] #'calendorg-ts-shrink)
    (define-key m [left] #'calendorg-ts-prev-day)
    (define-key m [right] #'calendorg-ts-next-day)
    (define-key m "J" #'calendorg-ts-later)
    (define-key m "K" #'calendorg-ts-earlier)
    (define-key m "v" #'calendorg-ts-promote)
    (define-key m "o" #'calendorg-ts-swap)
    (define-key m (kbd "RET") #'calendorg-ts-allocate)
    (define-key m "t" #'calendorg-ts-event)
    (define-key m (kbd "C-g") #'calendorg-ts-cancel)
    (define-key m [escape] #'calendorg-ts-cancel)
    (define-key m "q" #'calendorg-ts-cancel)
    (dotimes (i 10) (define-key m (number-to-string i) #'digit-argument))
    m)
  "Transient map active during time select.  Overrides evil.")

(defun calendorg--vsel-span ()
  "Selected range as (LO . HI), never empty."
  (let* ((a (plist-get calendorg--vsel :anchor))
         (p (plist-get calendorg--vsel :point))
         (lo (min a p)) (hi (max a p)))
    (if (= lo hi)
        (if (< (+ lo calendorg-slot) calendorg-grid-end)
            (cons lo (+ lo calendorg-slot))
          (cons (- hi calendorg-slot) hi))
      (cons lo hi))))

(defun calendorg-time-cursor ()
  "Place a movable line on a 15 minute boundary.  `v' selects a range from it."
  (interactive)
  (calendorg-time-select)
  (plist-put calendorg--vsel :cursor t)
  (calendorg--redisplay)
  (message "%s" (calendorg--vsel-echo)))

(defun calendorg-ts-promote ()
  "Turn the cursor into a range selection starting where it sits."
  (interactive)
  (when (plist-get calendorg--vsel :cursor)
    (calendorg--vsel-update
     (lambda () (plist-put calendorg--vsel :cursor nil)))))

(defun calendorg-time-select ()
  "Pick a time range in 15 minute slots, anchored at the selection's end."
  (interactive)
  (let* ((b (calendorg--selected))
         (today (- (car (calendorg--now)) calendorg--week-start))
         (day (cond (b (calendorg-block-day b))
                    ((<= 0 today (1- (* 7 calendorg--weeks))) today)
                    (t 0)))
         (anchor (if b
                     (min (calendorg-block-end b)
                          (- calendorg-grid-end calendorg-slot))
                   calendorg-grid-start)))
    (setq calendorg--vsel (list :day day :anchor anchor :point anchor))
    (calendorg--redisplay)
    (setq calendorg--vsel-exit
          (set-transient-map calendorg-time-select-map (lambda () calendorg--vsel)))
    (message "%s" (calendorg--vsel-echo))))

(defun calendorg--ts-quit ()
  "Leave time select: drop the transient map and clear the selection."
  (when calendorg--vsel-exit
    (funcall calendorg--vsel-exit)
    (setq calendorg--vsel-exit nil))
  (setq calendorg--vsel nil))

(defun calendorg--vsel-echo ()
  (if (plist-get calendorg--vsel :cursor)
      (format "TIME CURSOR %s %s  j/k ±15m · h/l day · v select · RET allocate · t event · ESC"
              (calendorg-date-label (+ calendorg--week-start (plist-get calendorg--vsel :day)))
              (calendorg--min->hhmm (plist-get calendorg--vsel :point)))
    (let ((span (calendorg--vsel-span)))
      (format "TIME SELECT %s %s–%s (%.2gh)  j/k grow · J/K move · h/l day · o swap · RET allocate · t event · ESC"
              (calendorg-date-label (+ calendorg--week-start (plist-get calendorg--vsel :day)))
              (calendorg--min->hhmm (car span)) (calendorg--min->hhmm (cdr span))
              (/ (- (cdr span) (car span)) 60.0)))))

(defun calendorg--vsel-update (fn)
  (funcall fn)
  (calendorg--redisplay)
  (message "%s" (calendorg--vsel-echo)))

(defun calendorg-ts-grow (&optional n)
  "Move the selection's moving end N slots later.
While the cursor is a bare line there is nothing to grow, so move it."
  (interactive "p")
  (calendorg--vsel-update
   (lambda ()
     (if (plist-get calendorg--vsel :cursor)
         (calendorg-ts-slide (or n 1))
       (plist-put calendorg--vsel :point
                  (min calendorg-grid-end
                       (+ (plist-get calendorg--vsel :point)
                          (* calendorg-slot (or n 1)))))))))

(defun calendorg-ts-shrink (&optional n)
  (interactive "p")
  (calendorg--vsel-update
   (lambda ()
     (if (plist-get calendorg--vsel :cursor)
         (calendorg-ts-slide (- (or n 1)))
       (plist-put calendorg--vsel :point
                  (max calendorg-grid-start
                       (- (plist-get calendorg--vsel :point)
                          (* calendorg-slot (or n 1)))))))))

(defun calendorg-ts-slide (n)
  "Move both ends N slots later, keeping the span, clamped to the grid."
  (let* ((a (plist-get calendorg--vsel :anchor))
         (p (plist-get calendorg--vsel :point))
         (d (* calendorg-slot n))
         (d (max (- calendorg-grid-start (min a p))
                 (min d (- calendorg-grid-end (max a p))))))
    (plist-put calendorg--vsel :anchor (+ a d))
    (plist-put calendorg--vsel :point (+ p d))))

(defun calendorg-ts-later (&optional n)
  "Slide the selection N slots later."
  (interactive "p")
  (calendorg--vsel-update (lambda () (calendorg-ts-slide (or n 1)))))

(defun calendorg-ts-earlier (&optional n)
  "Slide the selection N slots earlier."
  (interactive "p")
  (calendorg--vsel-update (lambda () (calendorg-ts-slide (- (or n 1))))))

(defun calendorg-ts-prev-day ()
  "Shift the whole range to the previous day, wrapping within the view."
  (interactive)
  (calendorg--vsel-update
   (lambda () (plist-put calendorg--vsel :day
                         (mod (1- (plist-get calendorg--vsel :day)) (* 7 calendorg--weeks))))))

(defun calendorg-ts-next-day ()
  (interactive)
  (calendorg--vsel-update
   (lambda () (plist-put calendorg--vsel :day
                         (mod (1+ (plist-get calendorg--vsel :day)) (* 7 calendorg--weeks))))))

(defun calendorg-ts-swap ()
  "Swap the fixed and moving ends."
  (interactive)
  (calendorg--vsel-update
   (lambda ()
     (let ((a (plist-get calendorg--vsel :anchor))
           (p (plist-get calendorg--vsel :point)))
       (plist-put calendorg--vsel :anchor p)
       (plist-put calendorg--vsel :point a)))))

(defun calendorg--ts-finish (make)
  "Leave time select, then commit the block MAKE returns for (DATE SPAN).
The map has to come off before any prompt: it lives in
`overriding-terminal-local-map', so its motion keys would otherwise fire
inside the minibuffer against a nil `calendorg--vsel'.  Aborting a prompt
unwinds to a clean calendar rather than stranding the overlay."
  (let ((day (plist-get calendorg--vsel :day))
        (span (calendorg--vsel-span))
        (done nil))
    (calendorg--ts-quit)
    (unwind-protect
        (let ((block (funcall make (+ calendorg--week-start day) span)))
          (setq done t)
          (push block (calendorg-data-dated calendorg--data))
          (calendorg--commit (cons (calendorg-block-date block)
                                   (calendorg-block-start block))))
      (unless done (calendorg--redisplay)))))

(defun calendorg-ts-allocate ()
  "Create an allocated block over the selection."
  (interactive)
  (calendorg--ts-finish
   (lambda (date span)
     (make-calendorg-block :date date :start (car span) :end (cdr span)
                           :type calendorg-allocated-type
                           :commitment (calendorg--read-commitment)
                           :source 'blocks))))

(defun calendorg-ts-event ()
  "Create an ad-hoc typed event over the selection."
  (interactive)
  (calendorg--ts-finish
   (lambda (date span)
     (let* ((types (cl-remove calendorg-allocated-type
                              (mapcar #'car (calendorg-data-types calendorg--data))
                              :test #'equal))
            (type (completing-read "Type: " types nil t nil nil "other"))
            (label (read-string "Label: "))
            (comment (read-string "Comment: ")))
       (make-calendorg-block
        :date date :start (car span) :end (cdr span) :type type
        :label (unless (string-empty-p (string-trim label)) (string-trim label))
        :comment (unless (string-empty-p (string-trim comment)) (string-trim comment))
        :source 'blocks)))))

(defun calendorg-ts-cancel ()
  (interactive)
  (calendorg--ts-quit)
  (calendorg--redisplay)
  (message nil))

;;; Mode

(defun calendorg-refresh ()
  "Reload from disk and redraw, staying on the same week and block."
  (interactive)
  (let ((b (calendorg--selected)))
    (calendorg--rebuild (and b (cons (calendorg-block-date b) (calendorg-block-start b)))))
  (calendorg--redisplay))

(defvar calendorg-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "h" #'calendorg-left)
    (define-key m "j" #'calendorg-down)
    (define-key m "k" #'calendorg-up)
    (define-key m "l" #'calendorg-right)
    (define-key m [left] #'calendorg-left)
    (define-key m [down] #'calendorg-down)
    (define-key m [up] #'calendorg-up)
    (define-key m [right] #'calendorg-right)
    (define-key m (kbd "TAB") #'calendorg-next)
    (define-key m [backtab] #'calendorg-previous)
    (define-key m "G" #'calendorg-last)
    (define-key m "gg" #'calendorg-first)
    (define-key m "gr" #'calendorg-refresh)
    (define-key m "[" #'calendorg-previous-week)
    (define-key m "]" #'calendorg-next-week)
    (define-key m "w" #'calendorg-toggle-next-week)
    (define-key m "0" #'calendorg-day-start)
    (define-key m "$" #'calendorg-day-end)
    (define-key m "v" #'calendorg-time-select)
    (define-key m "V" #'calendorg-time-cursor)
    (define-key m "i" #'calendorg-edit)
    (define-key m "x" #'calendorg-delete)
    (define-key m "c" #'calendorg-comment)
    (define-key m "q" #'quit-window)
    (define-key m [mouse-1] #'calendorg-mouse-select)
    m))

(define-derived-mode calendorg-mode special-mode "Calendorg"
  "Weekly schedule viewer."
  (setq-local cursor-type nil
              truncate-lines t
              buffer-read-only t
              mode-line-format nil)
  (when (boundp 'display-line-numbers) (setq-local display-line-numbers nil))
  ;; `evil-refresh-cursor' overwrites `cursor-type' on every command, and point
  ;; sits on the image, so the box cursor draws as a border around the whole
  ;; calendar.  Suppressing it per state is the only thing that sticks.
  (dolist (state '(normal insert visual motion emacs operator replace))
    (let ((var (intern (format "evil-%s-state-cursor" state))))
      (when (boundp var) (set (make-local-variable var) (list nil)))))
  (buffer-disable-undo)
  (add-hook 'window-size-change-functions #'calendorg--on-resize nil t))

;; An auxiliary map still loses `i', `v', `x' and `c' to evil's own normal-state
;; commands.  An intercept map outranks every evil state map, so the mode map
;; above stays the single source of truth for bindings.
(with-eval-after-load 'evil
  (evil-set-initial-state 'calendorg-mode 'normal)
  (evil-make-intercept-map calendorg-mode-map 'normal))

;;;###autoload
(defun calendorg ()
  "Open the weekly schedule on this week."
  (interactive)
  (unless (image-type-available-p 'svg)
    (user-error "This Emacs has no SVG support; rebuild with librsvg"))
  (let ((buf (get-buffer-create calendorg-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'calendorg-mode) (calendorg-mode))
      (setq calendorg--week-start nil calendorg--sel nil))
    (switch-to-buffer buf)
    (calendorg-refresh)))

(provide 'calendorg)
;;; calendorg.el ends here
