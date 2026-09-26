;;; calendorg-nav.el --- Selection graph -*- lexical-binding: t; -*-

;; Each block gets a four-vector of neighbour indices, rebuilt whenever the
;; block set changes.  Because blocks are kept sorted by (day, start, end),
;; j and k are just i+/-1 modulo the block count, which runs chronologically
;; across midnight and wraps at the end of the view; only h and l need to search.

;;; Code:

(require 'cl-lib)
(require 'calendorg-model)

(defun calendorg-day-ranges (blocks &optional days)
  "Return a DAYS-vector, 7 by default, of (LO . HI) index ranges into BLOCKS.
Each is inclusive, or nil for a day with no blocks."
  (let ((ranges (make-vector (or days 7) nil)))
    (dotimes (i (length blocks))
      (let* ((day (calendorg-block-day (aref blocks i)))
             (cur (aref ranges day)))
        (if cur
            (setcdr cur i)
          (aset ranges day (cons i i)))))
    ranges))

(defun calendorg--midpoint (block)
  (/ (+ (calendorg-block-start block) (calendorg-block-end block)) 2.0))

(defun calendorg--nearest-in-day (blocks range mid)
  "Index within RANGE whose midpoint is closest to MID.  Ties go to the earlier."
  (let ((best nil) (best-d nil))
    (cl-loop for k from (car range) to (cdr range)
             for d = (abs (- (calendorg--midpoint (aref blocks k)) mid))
             do (when (or (null best-d) (< d best-d))
                  (setq best k best-d d)))
    best))

(defun calendorg--scan (blocks ranges day mid dir)
  "First block on the nearest populated day from DAY in DIR, wrapping.
Stops once the scan returns to DAY, so a lone populated day yields itself."
  (let* ((n (length ranges)) (d (mod (+ day dir) n)) (result nil))
    (while (and (null result) (/= d day))
      (let ((range (aref ranges d)))
        (if range
            (setq result (calendorg--nearest-in-day blocks range mid))
          (setq d (mod (+ d dir) n)))))
    (or result
        ;; Only this day has blocks: stay put rather than returning nil.
        (let ((range (aref ranges day)))
          (and range (calendorg--nearest-in-day blocks range mid))))))

(defun calendorg-build-nav (blocks ranges)
  "Return a vector of [H J K L] index vectors, parallel to BLOCKS."
  (let* ((n (length blocks))
         (nav (make-vector n nil)))
    (dotimes (i n)
      (let* ((b (aref blocks i))
             (day (calendorg-block-day b))
             (mid (calendorg--midpoint b))
             ;; Blocks are sorted by (day, start), so stepping the index runs
             ;; chronologically and rolls into the next day on its own.
             (j (mod (1+ i) n))
             (k (mod (1- i) n)))
        (aset nav i (vector (calendorg--scan blocks ranges day mid -1)
                            j k
                            (calendorg--scan blocks ranges day mid 1)))))
    nav))

(defconst calendorg-nav-h 0)
(defconst calendorg-nav-j 1)
(defconst calendorg-nav-k 2)
(defconst calendorg-nav-l 3)

(defun calendorg-nav-move (nav index direction)
  "Neighbour of INDEX in DIRECTION, or INDEX when there is nowhere to go."
  (or (and nav index
           (< index (length nav))
           (aref (aref nav index) direction))
      index))

(provide 'calendorg-nav)
;;; calendorg-nav.el ends here
