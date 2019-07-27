

(defvar tolerance 0.00001)
(defun fixed-point (f first-guess)
  (defun close-enough? (v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (defun try (guess)
    (let ((next (funcall f guess)))
      (if (close-enough? guess next)
          next
        (try next))))
  (try first-guess))

(print (fixed-point 'cos 1.0))
(print (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0))

(defun average (x y) (/ (+ x y) 2))

(defun average-damp (f)
  (lambda (x) (/ (+ x (funcall f x)) 2)))

(defun msqrt (x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(print (msqrt 4))

