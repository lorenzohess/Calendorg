;;; calendorg.el --- Weekly schedule viewer -*- lexical-binding: t; -*-

;; Author: Lorenzo Hess
;; Package-Requires: ((emacs "29.1"))

;; Renders the schedule in `calendorg-schedule-dir' as an SVG week and allocates
;; blocks into the
;; gaps.  Selection always sits on a block; `v' drops into a slot cursor for
;; picking a time range.  See schedule-format.org for the data format.

;;; Code:

(require 'cl-lib)
(require 'calendorg-model)
(require 'calendorg-nav)
(require 'calendorg-render)

(defconst calendorg-buffer-name "*calendorg*")

(defvar-local calendorg--data nil)
(defvar-local calendorg--blocks nil "Vector of blocks, sorted.")
(defvar-local calendorg--ranges nil)
(defvar-local calendorg--nav nil)
(defvar-local calendorg--sel nil "Index into `calendorg--blocks'.")
(defvar-local calendorg--vsel nil "Plist (:day :anchor :point) while selecting time.")
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

(defun calendorg--own-blocks ()
  "Our blocks only, as a list."
  (cl-loop for b across calendorg--blocks
           when (eq (calendorg-block-source b) 'blocks) collect b))

(defun calendorg--mutable ()
  "Selected block, erroring unless we own it."
  (let ((b (calendorg--selected)))
    (unless b (user-error "No block selected"))
    (unless (eq (calendorg-block-source b) 'blocks)
      (user-error "%s is fixed; edit %s by hand"
                  (calendorg-block-title b) (calendorg-schedule-path)))
    b))

(defun calendorg--now ()
  "Current (DAY . MINUTE) in grid space.
The small hours belong to the previous day's column, the same roll
`calendorg--normalize' applies to parsed times."
  (let* ((now (decode-time))
         (day (mod (+ 6 (nth 6 now)) 7))
         (m (+ (* 60 (nth 2 now)) (nth 1 now))))
    (if (< m calendorg-grid-start)
        (cons (mod (1- day) 7) (+ m 1440))
      (cons day m))))

(defun calendorg--now-index ()
  "Index of the block happening now, else the next one, else the first."
  (let* ((now (calendorg--now))
         (day (car now))
         (m (cdr now))
         ;; start tops out at 1500, so this key sorts exactly as the blocks do.
         (key (+ (* day 1600) m))
         (n (length calendorg--blocks)))
    (or (cl-loop for i below n
                 for b = (aref calendorg--blocks i)
                 when (and (= (calendorg-block-day b) day)
                           (>= m (calendorg-block-start b))
                           (< m (calendorg-block-end b)))
                 return i)
        (cl-loop for i below n
                 for b = (aref calendorg--blocks i)
                 when (>= (+ (* (calendorg-block-day b) 1600)
                             (calendorg-block-start b))
                          key)
                 return i)
        0)))

(defun calendorg--rebuild (&optional keep)
  "Reload from disk.  KEEP is a (DAY . START) to reselect if still present."
  (setq calendorg--data (calendorg-load)
        calendorg--blocks (vconcat (calendorg-data-blocks calendorg--data))
        calendorg--ranges (calendorg-day-ranges calendorg--blocks)
        calendorg--nav (calendorg-build-nav calendorg--blocks calendorg--ranges))
  (setq calendorg--sel
        (cond
         ((zerop (length calendorg--blocks)) nil)
         (keep (or (cl-loop for i below (length calendorg--blocks)
                            for b = (aref calendorg--blocks i)
                            when (and (= (calendorg-block-day b) (car keep))
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
                      (aref calendorg--day-labels (calendorg-block-day b))
                      (calendorg--min->hhmm (calendorg-block-start b))
                      (calendorg--min->hhmm (calendorg-block-end b))
                      (calendorg-block-type b)
                      (if (calendorg-block-commitment b)
                          (concat " @" (calendorg-block-commitment b)) ""))
              'face `(:foreground ,(calendorg--tint
                                    (calendorg--type-color
                                     calendorg--data (calendorg-block-type b))
                                    0.5)))))
      (calendorg--insert-centered
       (list (propertize (or (calendorg-block-comment b) "") 'face 'shadow))))))

(defun calendorg--insert-stats ()
  "One line per commitment: allocated against the budget less its meetings."
  (let ((stats (calendorg-stats calendorg--data))
        (width 18))
    (when stats
      (let ((pad (apply #'max (mapcar (lambda (s) (length (car s))) stats))))
        (calendorg--insert-centered
         (mapcar
          (lambda (s)
            (cl-destructuring-bind (token allocated target) s
              (let* ((ratio (if (> target 0) (/ allocated target) 0))
                     (filled (min width (max 0 (round (* width ratio)))))
                     (met (>= allocated target))
                     (color (if met "#98be65" "#ECBE7B")))
                (concat (format (format "%%-%ds  " pad) token)
                        (propertize (make-string filled ?█) 'face `(:foreground ,color))
                        (propertize (make-string (- width filled) ?░) 'face 'shadow)
                        (propertize (format "  %.4g / %.4g h" allocated target)
                                    'face `(:foreground ,color))))))
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
    (setq calendorg--geom (calendorg-geometry w h)
          calendorg--last-size (cons (window-body-width nil t)
                                     (window-body-height nil t)))
    (erase-buffer)
    (when (and calendorg--last-image (display-graphic-p))
      (image-flush calendorg--last-image))
    (let ((start (point))
          (img (create-image (calendorg-render calendorg--data calendorg--blocks
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
  (interactive)
  (when (> (length calendorg--blocks) 0)
    (setq calendorg--sel 0) (calendorg--redisplay)))

(defun calendorg-last ()
  (interactive)
  (when (> (length calendorg--blocks) 0)
    (setq calendorg--sel (1- (length calendorg--blocks))) (calendorg--redisplay)))

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

(defun calendorg--read-spec (prompt initial)
  "Read \"DAY HH:MM-HH:MM\" and return (DAY START END) in grid space."
  (let* ((s (read-string prompt initial))
         (ok (string-match calendorg--re-spec s)))
    (unless ok (user-error "Expected DAY HH:MM-HH:MM, got %S" s))
    (let* ((day (cl-position (upcase (match-string 1 s)) calendorg-days :test #'equal))
           (start (calendorg--hhmm->min (format "%05s" (match-string 2 s))))
           (end (calendorg--hhmm->min (format "%05s" (match-string 3 s))))
           (norm (and day (calendorg--normalize day start end))))
      (unless day (user-error "Unknown day %S" (match-string 1 s)))
      (unless norm (user-error "Span must be positive and end by %s"
                               (calendorg--min->hhmm calendorg-grid-end)))
      norm)))

(defun calendorg--read-commitment (&optional initial)
  (let ((tokens (mapcar #'car (calendorg-data-commitments calendorg--data))))
    (unless tokens (user-error "No commitments declared in %s" (calendorg-schedule-path)))
    (completing-read "Commitment: " tokens nil t initial)))

(defun calendorg--commit (blocks keep)
  "Persist BLOCKS, reload, and reselect KEEP."
  (calendorg-save blocks)
  (calendorg--rebuild keep)
  (calendorg--redisplay))

(defun calendorg-edit ()
  "Edit the selected block's day, span, and commitment."
  (interactive)
  (let* ((b (calendorg--mutable))
         (spec (calendorg--read-spec
                "Block: " (format "%s %s-%s"
                                  (aref calendorg-days (calendorg-block-day b))
                                  (calendorg--min->hhmm (calendorg-block-start b))
                                  (calendorg--min->hhmm (calendorg-block-end b)))))
         (commitment (if (equal (calendorg-block-type b) calendorg-allocated-type)
                         (calendorg--read-commitment (calendorg-block-commitment b))
                       (calendorg-block-commitment b)))
         (others (cl-remove b (calendorg--own-blocks)))
         (new (calendorg-block-copy b)))
    (setf (calendorg-block-day new) (nth 0 spec)
          (calendorg-block-start new) (nth 1 spec)
          (calendorg-block-end new) (nth 2 spec)
          (calendorg-block-commitment new) commitment)
    (calendorg--commit (cons new others)
                       (cons (calendorg-block-day new) (calendorg-block-start new)))))

(defun calendorg-delete ()
  "Delete the selected block."
  (interactive)
  (let ((b (calendorg--mutable)))
    (when (y-or-n-p (format "Delete %s %s–%s? "
                            (calendorg-block-title b)
                            (calendorg--min->hhmm (calendorg-block-start b))
                            (calendorg--min->hhmm (calendorg-block-end b))))
      (calendorg--commit (cl-remove b (calendorg--own-blocks)) nil))))

(defun calendorg-comment ()
  "Set the comment on the selected block."
  (interactive)
  (let* ((b (calendorg--mutable))
         (text (read-string "Comment: " (calendorg-block-comment b)))
         (others (cl-remove b (calendorg--own-blocks)))
         (new (calendorg-block-copy b)))
    (setf (calendorg-block-comment new)
          (unless (string-empty-p (string-trim text)) (string-trim text)))
    (calendorg--commit (cons new others)
                       (cons (calendorg-block-day new) (calendorg-block-start new)))))

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
         (day (if b (calendorg-block-day b)
                (mod (+ 6 (nth 6 (decode-time))) 7)))
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
              (aref calendorg--day-labels (plist-get calendorg--vsel :day))
              (calendorg--min->hhmm (plist-get calendorg--vsel :point)))
    (let ((span (calendorg--vsel-span)))
      (format "TIME SELECT %s %s–%s (%.2gh)  j/k grow · J/K move · h/l day · o swap · RET allocate · t event · ESC"
              (aref calendorg--day-labels (plist-get calendorg--vsel :day))
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
  "Shift the whole range to the previous day, wrapping."
  (interactive)
  (calendorg--vsel-update
   (lambda () (plist-put calendorg--vsel :day
                         (mod (1- (plist-get calendorg--vsel :day)) 7)))))

(defun calendorg-ts-next-day ()
  (interactive)
  (calendorg--vsel-update
   (lambda () (plist-put calendorg--vsel :day
                         (mod (1+ (plist-get calendorg--vsel :day)) 7)))))

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
  "Leave time select, then commit the block MAKE returns for (DAY SPAN).
The map has to come off before any prompt: it lives in
`overriding-terminal-local-map', so its motion keys would otherwise fire
inside the minibuffer against a nil `calendorg--vsel'.  Aborting a prompt
unwinds to a clean calendar rather than stranding the overlay."
  (let ((day (plist-get calendorg--vsel :day))
        (span (calendorg--vsel-span))
        (done nil))
    (calendorg--ts-quit)
    (unwind-protect
        (let ((block (funcall make day span)))
          (setq done t)
          (calendorg--commit (cons block (calendorg--own-blocks))
                             (cons (calendorg-block-day block)
                                   (calendorg-block-start block))))
      (unless done (calendorg--redisplay)))))

(defun calendorg-ts-allocate ()
  "Create an allocated block over the selection."
  (interactive)
  (calendorg--ts-finish
   (lambda (day span)
     (make-calendorg-block :day day :start (car span) :end (cdr span)
                           :type calendorg-allocated-type
                           :commitment (calendorg--read-commitment)
                           :source 'blocks))))

(defun calendorg-ts-event ()
  "Create an ad-hoc typed event over the selection."
  (interactive)
  (calendorg--ts-finish
   (lambda (day span)
     (let* ((types (cl-remove calendorg-allocated-type
                              (mapcar #'car (calendorg-data-types calendorg--data))
                              :test #'equal))
            (type (completing-read "Type: " types nil t nil nil "other"))
            (label (read-string "Label: "))
            (comment (read-string "Comment: ")))
       (make-calendorg-block
        :day day :start (car span) :end (cdr span) :type type
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
  "Reload both files and redraw."
  (interactive)
  (let ((b (calendorg--selected)))
    (calendorg--rebuild (and b (cons (calendorg-block-day b) (calendorg-block-start b)))))
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
  "Open the weekly schedule."
  (interactive)
  (unless (image-type-available-p 'svg)
    (user-error "This Emacs has no SVG support; rebuild with librsvg"))
  (let ((buf (get-buffer-create calendorg-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'calendorg-mode) (calendorg-mode)))
    (switch-to-buffer buf)
    (calendorg-refresh)))

(provide 'calendorg)
;;; calendorg.el ends here
