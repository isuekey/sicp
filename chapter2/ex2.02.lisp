

(defun make-segment (p1 p2)
  (cons p1 p2))
(defun start-segment (seg)
  (car seg))
(defun end-segment (seg)
  (cdr seg))
(defun make-point (x y)
  (cons x y))
(defun x-point (p)
  (car p))
(defun y-point (p)
  (cdr p))

(defun print-point (p)
  (princ #\Newline)
  (princ "(")
  (princ (x-point p))
  (princ ",")
  (princ (y-point p))
  (princ ")"))

(defvar mseg (make-segment (make-point 1.0 2.0) (make-point 5.0 6.8)))

(print-point (start-segment mseg))
(print-point (end-segment mseg))

(defun midpoint-segment (seg)
  (let ((p1 (start-segment seg))
        (p2 (end-segment seg)))
    (make-point (/ (+ (x-point p1) (x-point p2)) 2.0d0)
                (/ (+ (y-point p1) (y-point p2)) 2.0d0))))
(print-point (midpoint-segment mseg))

