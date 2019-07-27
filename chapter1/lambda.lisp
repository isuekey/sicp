
(defun sum (term a next b)
  (if (> a b)
      0
    (+ (funcall term a) (sum term (funcall next a) next b))))

;;; (print ((lambda (x) (+ x 4)) 4))

(defun pisum (a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2)))) a
       (lambda (x) (+ x 4)) b))

(print (* 8 (pisum 1 1000)))

(defun p4 (x) ((lambda (x) (+ x 4)) x))
(print (p4 1))

;;; f(x,y) = x (1 + xy)^2 + y(1-y) + (1+xy)(1-y)
;;; let a=1+xy,b=1-6; f(x,y) = xa^2 + yb + ab

(defun f (x y)
  (defun f-h (a b)
    (+ (* x a a) (* y b) (* a b)))
  (f-h (+ 1 (* x y)) (- 1 y)))

(print (f 3 4))

(defun ff (x y)
  ((lambda (a b)
     (+ (* x a a) (* y b) (* a b)))
   (+ 1 (* x y)) (- 1 y)))

(print (ff 3 4))

(defun fl (x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x a a) (* y b) (* a b))))

(print (fl 3 4))

