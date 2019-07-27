
(defvar dx 0.000001d0)

(defun deriv (g)
  (lambda (x) (/ (- (funcall g (+ x dx)) (funcall g x)) dx)))
  

(defun cube (x) (* x x x))

(print (funcall (deriv 'cube) 5))

(defun newton-transform (g)
  (lambda (x) (- x (/ (funcall g x) (funcall (deriv g) x)))))

(defvar tolerance 0.00001d0)
(defun fixed-point (f first-guess)
  (defun close-enough? (v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (defun try (guess)
    (let ((next (funcall f guess)))
      (if (close-enough? guess next)
          next
        (try next))))
  (try first-guess))

(defun newton-guess (g guess)
  (fixed-point (newton-transform g) guess))

(defun msqrt (x)
  (newton-guess (lambda (y) (- (* y y) x)) 1.0))

(print (msqrt 5))
(print (msqrt 9))


(defun fixed-transform (g transform  guess)
  (fixed-point (funcall transform g) guess))

(defun average-damp (f)
  (lambda (x) (/ (+ x (funcall f x)) 2)))

(defun fmasqrt (x)
  (fixed-transform (lambda (y) (/ x y))
                   'average-damp
                   1.0d0))
(print (fmasqrt 5))
(print (fmasqrt 9))

(defun fmnsqrt (x)
  (fixed-transform (lambda (y) (- (* y y) x))
                   'newton-transform
                   1.0d0))

(print (fmnsqrt 5))
(print (fmnsqrt 9))

