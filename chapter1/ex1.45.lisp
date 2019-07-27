
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

(defun average-damp (f)
  (lambda (x) (/ (+ x (funcall f x)) 2)))


(defun repeat (f n)
  (lambda (x)
    (if (= n 1)
        (funcall f x)
      (funcall f (funcall (repeat f (- n 1)) x)))))

(defun mm (x)
  (lambda (y) (* y x)))

(defun nm (x n)
  (funcall (repeat (mm x) n) 1))


;;; f(x) = y / x^(n - 1) = x
;;; avg-damp (x + f(x))/2
;;; y= x^n
;;; n > 1


(defun rootfn (y n)
  (defun fx (x)
    (/ y (nm x (- n 1))))
  (defun avg (f)
    (funcall (repeat 'average-damp (- n 1)) f))
  (fixed-point (avg 'fx) 1.0d0))


(print (rootfn 1024 10))
  
