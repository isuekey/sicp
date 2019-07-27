
(defvar dx 0.00000001d0)

(defun smooth (f)
  (lambda (x)
    (/ (+ (funcall f (+ x dx)) (funcall f x) (funcall f (- x dx))) 3)))

(defun repeat (f n)
  (lambda (x)
    (if (= n 1)
        (funcall f x)
      (funcall f (funcall (repeat f (- n 1)) x)))))


(defun square (x) (* x x))

(defun sm2sq (x)
  (funcall (funcall (repeat 'smooth 2) 'square) x))

(print (sm2sq 5))
