

(defvar tolerance 0.000001)
(defun fixed-point (f fguess)
  (defun close? (v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (defun try (guess)
    (print guess)
    (let ((next (funcall f guess)))
      (if (close? guess next)
          next
        (try next))))
  (try fguess))

(defun xx (y)
  (fixed-point (lambda (x) (/ (log y) (log x))) 2.0))

(print (xx 1000))
(defun average (x y) (/ (+ x y) 2))

(print " use average ")
(defun xxa (y)
  (fixed-point (lambda (x) (average x (/ (log y) (log x)))) 2.0))

(print (xxa 1000))
