;;; calendorg-model.el --- Schedule parsing and persistence -*- lexical-binding: t; -*-

;; Parses the format described in schedule-format.org.  Two files, one grammar:
;; the schedule is read-only, the blocks file is ours to rewrite.

;;; Code:

(require 'cl-lib)

(defgroup calendorg nil
  "Weekly schedule viewer."
  :group 'calendar
  :prefix "calendorg-")

(defcustom calendorg-schedule-dir
  (or (getenv "SCHEDULE_DIR") "~/nextcloud-sync/todo/")
  "Directory holding both schedule files.
Defaults to the SCHEDULE_DIR environment variable when it is set."
  :type 'directory :group 'calendorg)

(defcustom calendorg-schedule-file "schedule.org"
  "Hand-authored schedule.  Never written to.
Resolved against `calendorg-schedule-dir' unless absolute."
  :type 'file :group 'calendorg)

(defcustom calendorg-blocks-file "calendorg-blocks.org"
  "Allocated blocks.  Owned and rewritten by Calendorg.
Resolved against `calendorg-schedule-dir' unless absolute."
  :type 'file :group 'calendorg)

(defun calendorg-schedule-path ()
  "Absolute path of the hand-authored schedule."
  (expand-file-name calendorg-schedule-file
                    (expand-file-name calendorg-schedule-dir)))

(defun calendorg-blocks-path ()
  "Absolute path of the blocks file Calendorg owns."
  (expand-file-name calendorg-blocks-file
                    (expand-file-name calendorg-schedule-dir)))

(defconst calendorg-days ["MON" "TUE" "WED" "THU" "FRI" "SAT" "SUN"])

(defconst calendorg-grid-start 480
  "First minute shown, 08:00.")

(defconst calendorg-grid-end 1500
  "Last minute shown, 01:00 the following day.")

(defconst calendorg-slot 15
  "Smallest allocatable unit, in minutes.")

(defconst calendorg-allocated-type "allocated"
  "Type token Calendorg writes.  Blocks of this type are mutable.")

(defconst calendorg--re-block
  (concat "^[ \t]*-[ \t]+\\([A-Z]\\{3\\}\\)"
          "[ \t]+\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)-\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)"
          "[ \t]+\\([a-z][a-z-]*\\)"
          "\\(?:[ \t]+@\\([^ \t]+\\)\\)?"
          "\\(?:[ \t]+\\(.*?\\)\\)?[ \t]*$"))

(defconst calendorg--re-comment    "^[ \t]+:[ \t]?\\(.*\\)$")
(defconst calendorg--re-commitment
  "^[ \t]*-[ \t]+\\([^ \t]+\\)[ \t]+\\([0-9]+\\(?:\\.[0-9]+\\)?\\)\\(?:[ \t]+\\(#[0-9a-fA-F]\\{6\\}\\)\\)?[ \t]*$")
(defconst calendorg--re-sleep      "^[ \t]*-[ \t]+\\(wake\\|sleep\\)[ \t]+\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)-\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)[ \t]*$")
(defconst calendorg--re-type       "^[ \t]*-[ \t]+\\([a-z][a-z-]*\\)[ \t]+\\(#[0-9a-fA-F]\\{6\\}\\)[ \t]*$")
(defconst calendorg--re-heading    "^\\*+[ \t]+\\(.*?\\)[ \t]*$")

(cl-defstruct (calendorg-block (:copier calendorg-block-copy))
  day          ; 0 = Monday
  start end    ; minutes, grid space: may exceed 1440
  type
  commitment   ; token or nil
  label        ; display string or nil
  comment      ; string or nil
  source)      ; `schedule' (immutable) or `blocks' (ours)

(cl-defstruct calendorg-data
  commitments  ; list of (token hours color-or-nil)
  types        ; alist (token . "#rrggbb")
  wake sleep   ; (start . end) in grid space, or nil
  blocks       ; list
  warnings)    ; list of unparsed lines

;;; Times

(defun calendorg--hhmm->min (s)
  (+ (* 60 (string-to-number (substring s 0 2)))
     (string-to-number (substring s 3 5))))

(defun calendorg--min->hhmm (m &optional past-midnight)
  "Format M as HH:MM.  With PAST-MIDNIGHT, 1440 prints as 24:00 rather
than 00:00, so a block running from before midnight to after it stays a
single positive span on disk."
  (let ((m (if past-midnight m (mod m 1440))))
    (format "%02d:%02d" (/ m 60) (mod m 60))))

(defun calendorg--hours (block)
  (/ (- (calendorg-block-end block) (calendorg-block-start block)) 60.0))

(defun calendorg--block< (a b)
  (let ((da (calendorg-block-day a)) (db (calendorg-block-day b)))
    (cond ((/= da db) (< da db))
          ((/= (calendorg-block-start a) (calendorg-block-start b))
           (< (calendorg-block-start a) (calendorg-block-start b)))
          (t (< (calendorg-block-end a) (calendorg-block-end b))))))

(defun calendorg--normalize (day start end)
  "Roll times before the grid start onto the preceding day's column.
Returns (DAY START END) in grid space, or nil when the span is invalid."
  (when (< start calendorg-grid-start)
    (setq day (mod (1- day) 7)
          start (+ start 1440)
          end (+ end 1440)))
  (and (> end start)
       (<= end calendorg-grid-end)
       (list day start end)))

;;; Parsing

(defun calendorg--parse-block (line source)
  "Build a block from LINE, which must already have matched the block regexp."
  (let* ((day (cl-position (match-string 1 line) calendorg-days :test #'equal))
         (start (calendorg--hhmm->min (match-string 2 line)))
         (end (calendorg--hhmm->min (match-string 3 line)))
         (type (match-string 4 line))
         (commitment (match-string 5 line))
         (label (match-string 6 line))
         (norm (and day (calendorg--normalize day start end))))
    (when norm
      (make-calendorg-block
       :day (nth 0 norm) :start (nth 1 norm) :end (nth 2 norm)
       :type type :commitment commitment
       :label (unless (or (null label) (string-empty-p label)) label)
       :source source))))

(defun calendorg--parse-file (file source acc)
  "Parse FILE into ACC, a `calendorg-data'.  SOURCE tags the blocks."
  (when (file-readable-p (expand-file-name file))
    (with-temp-buffer
      (insert-file-contents (expand-file-name file))
      (goto-char (point-min))
      (let ((section nil))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (cond
             ((string-match calendorg--re-heading line)
              (setq section (downcase (match-string 1 line))))
             ((string-match "\\`[ \t]*\\'" line) nil)
             ((string-prefix-p "#" line) nil)

             ;; A comment attaches to the block above it.
             ((and (equal section "blocks")
                   (string-match calendorg--re-comment line))
              (when (calendorg-data-blocks acc)
                (setf (calendorg-block-comment (car (calendorg-data-blocks acc)))
                      (match-string 1 line))))

             ((equal section "blocks")
              (let ((b (and (string-match calendorg--re-block line)
                            (calendorg--parse-block line source))))
                (if b
                    (push b (calendorg-data-blocks acc))
                  (push line (calendorg-data-warnings acc)))))

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
          (forward-line 1)))))
  acc)

(defun calendorg-load ()
  "Read both files.  Returns a `calendorg-data' with blocks sorted."
  (let ((acc (make-calendorg-data)))
    (calendorg--parse-file (calendorg-schedule-path) 'schedule acc)
    (calendorg--parse-file (calendorg-blocks-path) 'blocks acc)
    (setf (calendorg-data-commitments acc) (nreverse (calendorg-data-commitments acc))
          (calendorg-data-types acc) (nreverse (calendorg-data-types acc))
          (calendorg-data-warnings acc) (nreverse (calendorg-data-warnings acc))
          (calendorg-data-blocks acc) (sort (calendorg-data-blocks acc) #'calendorg--block<))
    acc))

;;; Writing

(defun calendorg--block->line (b)
  "Render B back to source syntax, undoing the grid-space day roll."
  (let ((day (calendorg-block-day b))
        (start (calendorg-block-start b))
        (end (calendorg-block-end b)))
    (when (>= start 1440)
      (setq day (mod (1+ day) 7)))
    (concat "- " (aref calendorg-days day)
            " " (calendorg--min->hhmm start)
            "-" (calendorg--min->hhmm end (and (< start 1440) (>= end 1440)))
            " " (calendorg-block-type b)
            (if (calendorg-block-commitment b)
                (concat " @" (calendorg-block-commitment b)) "")
            (if (calendorg-block-label b)
                (concat " " (calendorg-block-label b)) ""))))

(defun calendorg-save (blocks)
  "Rewrite the blocks file from BLOCKS, which must all be ours."
  (let ((file (calendorg-blocks-path)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert "#+title: Calendorg blocks\n"
              "# Written by Calendorg.  Edits here are kept, but formatting is not.\n\n"
              "* Blocks\n")
      (dolist (b (sort (copy-sequence blocks) #'calendorg--block<))
        (insert (calendorg--block->line b) "\n")
        (when (calendorg-block-comment b)
          (insert "  : " (calendorg-block-comment b) "\n"))))))

;;; Stats

(defun calendorg-commitment-color (data token)
  "Colour declared for TOKEN, or nil when it did not name one."
  (nth 2 (assoc token (calendorg-data-commitments data))))

(defun calendorg-stats (data)
  "Return a list of (TOKEN ALLOCATED TARGET) in declaration order.
TARGET is the declared budget less every block in the schedule file that
references the commitment, whatever its type: those hours are already
spoken for, so only the remainder is yours to allocate."
  (let ((blocks (calendorg-data-blocks data)))
    (mapcar
     (lambda (c)
       (let ((token (car c)) (spoken-for 0.0) (allocated 0.0))
         (dolist (b blocks)
           (when (equal (calendorg-block-commitment b) token)
             (cond
              ((equal (calendorg-block-type b) calendorg-allocated-type)
               (cl-incf allocated (calendorg--hours b)))
              ((eq (calendorg-block-source b) 'schedule)
               (cl-incf spoken-for (calendorg--hours b))))))
         (list token allocated (- (nth 1 c) spoken-for))))
     (calendorg-data-commitments data))))

(provide 'calendorg-model)
;;; calendorg-model.el ends here
