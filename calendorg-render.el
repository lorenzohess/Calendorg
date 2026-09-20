;;; calendorg-render.el --- SVG calendar -*- lexical-binding: t; -*-

;; Builds the whole week as one SVG string.  Nothing here touches buffers or
;; state; callers pass in the data and get back the markup plus the geometry
;; used, so hit-testing can invert exactly the same numbers.

;;; Code:

(require 'cl-lib)
(require 'calendorg-model)

(defcustom calendorg-bg "#21242b"
  "Canvas background." :type 'string :group 'calendorg)

(defcustom calendorg-grid-color "#2a2e35"
  "Hour and day rules." :type 'string :group 'calendorg)

(defcustom calendorg-grid-color-faint "#23262d"
  "Half-hour rules." :type 'string :group 'calendorg)

(defcustom calendorg-muted "#5B6268"
  "Axis labels." :type 'string :group 'calendorg)

(defcustom calendorg-today-color "#51afef"
  ;; Also washes the column background since the highlight was too faint.
  "Today's column header and marker." :type 'string :group 'calendorg)

(defcustom calendorg-now-color "#ff6c6b"
  "Current-time rule." :type 'string :group 'calendorg)

(defcustom calendorg-select-color "#c678dd"
  "Time-select overlay.  Deliberately not one of the block types."
  :type 'string :group 'calendorg)

(defcustom calendorg-sleep-color "#31363f"
  "Sleep hatching." :type 'string :group 'calendorg)

(defcustom calendorg-hour-height 24
  "Fallback pixels per hour when the window height is unknown."
  :type 'integer :group 'calendorg)

(defconst calendorg--gutter 34)
(defconst calendorg--header 18)
(defconst calendorg--font 11)
(defconst calendorg--day-labels ["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"])

;;; Colour helpers

(defun calendorg--rgb (hex)
  (list (string-to-number (substring hex 1 3) 16)
        (string-to-number (substring hex 3 5) 16)
        (string-to-number (substring hex 5 7) 16)))

(defun calendorg--tint (hex amount)
  "Blend HEX toward white by AMOUNT, 0.0 to 1.0."
  (apply #'format "#%02x%02x%02x"
         (mapcar (lambda (c) (min 255 (round (+ c (* amount (- 255 c))))))
                 (calendorg--rgb hex))))

(defun calendorg--saturation (hex)
  (let* ((rgb (calendorg--rgb hex))
         (hi (apply #'max rgb))
         (lo (apply #'min rgb)))
    (if (zerop hi) 0.0 (/ (float (- hi lo)) hi))))

(defun calendorg--type-color (data type)
  (or (cdr (assoc type (calendorg-data-types data))) calendorg-muted))

(defun calendorg-block-color (data block)
  "Colour for BLOCK.
An allocated block takes its commitment's colour when one is declared, so
two budgets read apart at a glance instead of both being generic green.
Everything else is coloured by type."
  (or (and (equal (calendorg-block-type block) calendorg-allocated-type)
           (calendorg-block-commitment block)
           (calendorg-commitment-color data (calendorg-block-commitment block)))
      (calendorg--type-color data (calendorg-block-type block))))

(defun calendorg--fill-opacity (type color)
  "Allocated blocks recede; fixed ones read as solid.
Low-chroma colours need more alpha to carry the same weight."
  (cond ((equal type calendorg-allocated-type) 0.14)
        ((< (calendorg--saturation color) 0.25) 0.50)
        (t 0.34)))

;;; Text

(defun calendorg--esc (s)
  (let ((s (or s "")))
    (setq s (replace-regexp-in-string "&" "&amp;" s t t))
    (setq s (replace-regexp-in-string "<" "&lt;" s t t))
    (replace-regexp-in-string ">" "&gt;" s t t)))

(defun calendorg--fit (s width)
  "Truncate S to roughly WIDTH pixels at the label font size."
  (let ((max (max 1 (floor (/ width (* calendorg--font 0.55))))))
    (if (<= (length s) max) s (concat (substring s 0 (max 1 (1- max))) "…"))))

(defun calendorg-block-title (block)
  "Display name: the label, else the commitment, else the type.
Falling back to the commitment is what makes an unlabelled meeting read as
\"GVSC11\" rather than \"Meeting\"."
  (or (calendorg-block-label block)
      (calendorg-block-commitment block)
      (let ((s (replace-regexp-in-string "-" " " (calendorg-block-type block))))
        (concat (upcase (substring s 0 1)) (substring s 1)))))

;;; Geometry

(defun calendorg-geometry (width height)
  "Layout plist for a canvas of WIDTH by HEIGHT pixels."
  (let* ((x0 calendorg--gutter)
         (y0 calendorg--header)
         (col-w (/ (- width x0) 7.0))
         (grid-h (- height y0)))
    (list :width width :height height :x0 x0 :y0 y0
          :col-w col-w :grid-h grid-h
          :span (float (- calendorg-grid-end calendorg-grid-start)))))

(defun calendorg-y-of (geom minute)
  (+ (plist-get geom :y0)
     (* (plist-get geom :grid-h)
        (/ (- minute calendorg-grid-start) (plist-get geom :span)))))

(defun calendorg-x-of (geom day)
  (+ (plist-get geom :x0) (* day (plist-get geom :col-w))))

(defun calendorg-day-at (geom x)
  "Column index at pixel X, or nil outside the grid."
  (let ((d (floor (/ (- x (plist-get geom :x0)) (plist-get geom :col-w)))))
    (and (>= d 0) (<= d 6) d)))

(defun calendorg-minute-at (geom y)
  "Minute at pixel Y, snapped to the slot size."
  (let* ((raw (+ calendorg-grid-start
                 (* (plist-get geom :span)
                    (/ (- y (plist-get geom :y0)) (plist-get geom :grid-h)))))
         (snapped (* calendorg-slot (round (/ raw (float calendorg-slot))))))
    (max calendorg-grid-start (min calendorg-grid-end snapped))))

;;; Pieces

(defun calendorg--svg-defs ()
  (concat
   "<defs>"
   ;; The diagonal is baked into the tile rather than applied with
   ;; patternTransform='rotate(45)', which costs librsvg ~85ms a frame.
   (format "<pattern id='hatch' width='8' height='8' patternUnits='userSpaceOnUse'>
<rect width='8' height='8' fill='#101318'/><path d='M0,8 L8,0 M-2,2 L2,-2 M6,10 L10,6' stroke='%s' stroke-width='1' fill='none'/></pattern>"
           calendorg-sleep-color)
   "<linearGradient id='fadeDown' x1='0' y1='0' x2='0' y2='1'>
<stop offset='0' stop-color='#fff' stop-opacity='1'/><stop offset='1' stop-color='#fff' stop-opacity='0'/></linearGradient>
<linearGradient id='fadeUp' x1='0' y1='0' x2='0' y2='1'>
<stop offset='0' stop-color='#fff' stop-opacity='0'/><stop offset='1' stop-color='#fff' stop-opacity='1'/></linearGradient>"
   "</defs>"))

(defun calendorg--svg-sleep (geom data)
  "Fade the wake and sleep uncertainty windows across every day."
  (let ((x (plist-get geom :x0))
        (w (- (plist-get geom :width) (plist-get geom :x0)))
        (out ""))
    (dolist (spec (list (cons (calendorg-data-wake data) "fadeDown")
                        (cons (calendorg-data-sleep data) "fadeUp")))
      (when-let* ((span (car spec)))
        (let* ((id (cdr spec))
               (y (calendorg-y-of geom (car span)))
               (h (- (calendorg-y-of geom (cdr span)) y))
               (mask (format "m%s" id)))
          (setq out
                (concat out
                        (format "<mask id='%s'><rect x='%.1f' y='%.1f' width='%.1f' height='%.1f' fill='url(#%s)'/></mask>"
                                mask x y w h id)
                        (format "<rect x='%.1f' y='%.1f' width='%.1f' height='%.1f' fill='url(#hatch)' mask='url(#%s)'/>"
                                x y w h mask))))))
    out))

(defun calendorg--svg-grid (geom today)
  (let* ((x0 (plist-get geom :x0))
         (w (plist-get geom :width))
         (col-w (plist-get geom :col-w))
         (out ""))
    ;; Today's column, faintly lifted.
    (when today
      (setq out (format "<rect x='%.1f' y='0' width='%.1f' height='%.1f' fill='%s' fill-opacity='0.09'/>"
                        (calendorg-x-of geom today) col-w (plist-get geom :height)
                        calendorg-today-color)))
    ;; Rules on the hour, fainter on the half.
    (cl-loop for m from calendorg-grid-start to calendorg-grid-end by 30
             do (let ((y (calendorg-y-of geom m))
                      (hourp (zerop (mod m 60))))
                  (setq out (concat out
                                    (format "<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' stroke='%s'/>"
                                            x0 y w y
                                            (if hourp calendorg-grid-color
                                              calendorg-grid-color-faint))))))
    ;; Day separators.
    (cl-loop for d from 1 to 6
             do (let ((x (calendorg-x-of geom d)))
                  (setq out (concat out
                                    (format "<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' stroke='%s'/>"
                                            x (plist-get geom :y0) x (plist-get geom :height)
                                            calendorg-grid-color)))))
    ;; Hour labels, every two hours, 24-hour clock.
    (cl-loop for m from calendorg-grid-start to calendorg-grid-end by 120
             do (setq out (concat out
                                  (format "<text x='%d' y='%.1f' text-anchor='end' font-family='monospace' font-size='%d' fill='%s'>%02d</text>"
                                          (- x0 6) (+ (calendorg-y-of geom m) 4)
                                          calendorg--font calendorg-muted
                                          (/ (mod m 1440) 60)))))
    ;; Day headers.
    (dotimes (d 7)
      (setq out (concat out
                        (format "<text x='%.1f' y='13' text-anchor='middle' font-family='sans-serif' font-size='%d' fill='%s'%s>%s</text>"
                                (+ (calendorg-x-of geom d) (/ col-w 2)) calendorg--font
                                (if (eq d today) calendorg-today-color calendorg-muted)
                                (if (eq d today) " font-weight='500'" "")
                                (aref calendorg--day-labels d)))))
    out))

(defun calendorg--svg-block (geom data block selectedp)
  (let* ((color (calendorg-block-color data block))
         (allocp (equal (calendorg-block-type block) calendorg-allocated-type))
         (x (+ (calendorg-x-of geom (calendorg-block-day block)) 1))
         (w (- (plist-get geom :col-w) 2))
         (y (calendorg-y-of geom (calendorg-block-start block)))
         (h (max 8 (- (calendorg-y-of geom (calendorg-block-end block)) y)))
         (opacity (calendorg--fill-opacity (calendorg-block-type block) color))
         (title (calendorg-block-title block))
         (out ""))
    ;; Allocated blocks carry a dashed outline and no accent bar: provisional,
    ;; rather than a wall.
    (setq out
          (format "<rect x='%.1f' y='%.1f' width='%.1f' height='%.1f' rx='3' fill='%s' fill-opacity='%.2f'%s/>"
                  x y w h color
                  (if selectedp (+ opacity 0.10) opacity)
                  (cond (selectedp (format " stroke='%s' stroke-width='1.5'%s"
                                           (calendorg--tint color 0.55)
                                           (if allocp " stroke-dasharray='3 2.5'" "")))
                        (allocp (format " stroke='%s' stroke-width='1' stroke-dasharray='3 2.5'" color))
                        (t ""))))
    (unless allocp
      (setq out (concat out (format "<rect x='%.1f' y='%.1f' width='2.5' height='%.1f' rx='1' fill='%s'/>"
                                    x y h color))))
    (setq out (concat out
                      (format "<text x='%.1f' y='%.1f' font-family='sans-serif' font-size='%d' fill='%s'>%s</text>"
                              (+ x (if allocp 8 9)) (+ y 14) calendorg--font
                              (calendorg--tint color (if selectedp 0.65 0.45))
                              (calendorg--esc (calendorg--fit title (- w 14))))))
    (when (>= h 40)
      (setq out (concat out
                        (format "<text x='%.1f' y='%.1f' font-family='monospace' font-size='%d' fill='%s'>%s–%s</text>"
                                (+ x (if allocp 8 9)) (+ y 27) calendorg--font
                                (calendorg--tint color 0.15)
                                (calendorg--min->hhmm (calendorg-block-start block))
                                (calendorg--min->hhmm (calendorg-block-end block))))))
    out))

(defun calendorg--svg-now (geom today)
  (when today
    (let* ((now (decode-time))
           (m (+ (* 60 (nth 2 now)) (nth 1 now)))
           (m (if (< m calendorg-grid-start) (+ m 1440) m)))
      (when (and (>= m calendorg-grid-start) (<= m calendorg-grid-end))
        (let ((y (calendorg-y-of geom m))
              (x (calendorg-x-of geom today)))
          (format "<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' stroke='%s' stroke-opacity='0.55'/>
<circle cx='%.1f' cy='%.1f' r='3' fill='%s'/>"
                  x y (+ x (plist-get geom :col-w)) y calendorg-now-color
                  x y calendorg-now-color))))))

(defun calendorg--svg-cursor (geom vsel)
  "Draw VSEL as a single line on a slot boundary."
  (let* ((day (plist-get vsel :day))
         (x (+ (calendorg-x-of geom day) 1))
         (w (- (plist-get geom :col-w) 2))
         (y (calendorg-y-of geom (plist-get vsel :point)))
         (tint (calendorg--tint calendorg-select-color 0.45)))
    (concat
     (format "<rect x='%.1f' y='%.1f' width='%.1f' height='2.5' fill='%s'/>"
             x (- y 1.25) w calendorg-select-color)
     ;; Caret on the leading edge, so the line reads as a cursor.
     (format "<path d='M %.1f %.1f L %.1f %.1f L %.1f %.1f Z' fill='%s'/>"
             x (- y 4.5) (+ x 6) y x (+ y 4.5) calendorg-select-color)
     (format "<text x='%.1f' y='%.1f' font-family='monospace' font-size='%d' fill='%s'>%s</text>"
             (+ x 10) (- y 4) calendorg--font tint
             (calendorg--min->hhmm (plist-get vsel :point))))))

(defun calendorg--svg-selection (geom vsel)
  "Draw VSEL, as a cursor line when it carries :cursor, else as a range."
  (when vsel
   (if (plist-get vsel :cursor)
       (calendorg--svg-cursor geom vsel)
    (let* ((day (plist-get vsel :day))
           (lo (min (plist-get vsel :anchor) (plist-get vsel :point)))
           (hi (max (plist-get vsel :anchor) (plist-get vsel :point)))
           (x (+ (calendorg-x-of geom day) 1))
           (w (- (plist-get geom :col-w) 2))
           (y (calendorg-y-of geom lo))
           (h (- (calendorg-y-of geom hi) y))
           (tint (calendorg--tint calendorg-select-color 0.45))
           (out (format "<rect x='%.1f' y='%.1f' width='%.1f' height='%.1f' rx='3' fill='%s' fill-opacity='0.22' stroke='%s' stroke-width='1.5'/>"
                        x y w h calendorg-select-color calendorg-select-color)))
      ;; Slot ticks, so the increment is legible.
      (cl-loop for m from (+ lo calendorg-slot) below hi by calendorg-slot
               do (let ((ty (calendorg-y-of geom m)))
                    (setq out (concat out (format "<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' stroke='%s'/>"
                                                  x ty (+ x 7) ty tint)))))
      ;; Thicker edge on the moving end.
      (let ((py (calendorg-y-of geom (plist-get vsel :point))))
        (setq out (concat out (format "<rect x='%.1f' y='%.1f' width='%.1f' height='2.5' fill='%s'/>"
                                      x (- py 1.25) w tint))))
      (concat out
              (format "<text x='%.1f' y='%.1f' font-family='monospace' font-size='%d' fill='%s'>%s–%s</text>"
                      (+ x 9) (+ y 14) calendorg--font tint
                      (calendorg--min->hhmm lo) (calendorg--min->hhmm hi)))))))

;;; Entry point

(defun calendorg-render (data blocks sel vsel width height)
  "Return the SVG string for the week.
BLOCKS is a vector, SEL an index or nil, VSEL a time-select plist or nil."
  (let* ((geom (calendorg-geometry width height))
         (today (mod (+ 6 (nth 6 (decode-time))) 7)))
    (with-temp-buffer
      (insert (format "<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d' viewBox='0 0 %d %d'>"
                      width height width height))
      (insert (calendorg--svg-defs))
      (insert (format "<rect width='%d' height='%d' fill='%s'/>" width height calendorg-bg))
      (insert (calendorg--svg-grid geom today))
      (insert (calendorg--svg-sleep geom data))
      ;; Fixed blocks first, allocated over them, selection last.
      (dotimes (pass 2)
        (dotimes (i (length blocks))
          (let ((b (aref blocks i)))
            (when (and (eq (equal (calendorg-block-type b) calendorg-allocated-type)
                           (= pass 1))
                       (not (eq i sel)))
              (insert (calendorg--svg-block geom data b nil))))))
      (when (and sel (< sel (length blocks)))
        (insert (calendorg--svg-block geom data (aref blocks sel) t)))
      (insert (or (calendorg--svg-now geom today) ""))
      (insert (or (calendorg--svg-selection geom vsel) ""))
      (insert "</svg>")
      (buffer-string))))

(provide 'calendorg-render)
;;; calendorg-render.el ends here
