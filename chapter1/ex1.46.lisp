

(defun iterative-improve (good? improve)
  (lambda (x)
    (if (funcall good? x)
        x
      (funcall (iterative-improve good? improve) (funcall improve x)))))


(defvar dx 0.0000001d0)

(defun msqrt (x)
  (funcall (iterative-improve
            (lambda (y) (< (abs (- (* y y 1.0d0) x)) dx))
            (lambda (y) (/ (+ y (/ x y)) 2.0d0))) 1.0d0))

(print (msqrt 8))

(defun fixed-point (fx first-guess)
  (funcall (iterative-improve
            (lambda (y) (< (abs (- (funcall fx y) y)) dx))
            (lambda (y) (funcall fx y))) first-guess))


(defun fmsqrt (x)
  (fixed-point (lambda (y) (/ ( + y (/ x y)) 2)) 1.0d0))

(print (fmsqrt 10))




