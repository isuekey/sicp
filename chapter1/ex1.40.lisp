

;;; g(x)= x^3 + ax^2 + bx + c

(defun cubic (a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(defvar dx 0.000001d0)
(defun deriv (g x)
  (/ (- (funcall g (+ x dx)) (funcall g x)) dx))

(defun newton-trans (g)
  (lambda (x) (- x (/ (funcall g x) (deriv g x)))))

(defun fix-point (f first-guess)
  (defun close? (v1 v2)
    (< (abs (- v1 v2)) dx))
  (defun try (guess)
    (let ((next (funcall f guess)))
      (if (close? guess next)
          next
        (try next))))
  (try first-guess))


(defun newton-method (g guess)
  (fix-point (newton-trans g) guess))

(print (newton-method (cubic 1 1 1) 1))

