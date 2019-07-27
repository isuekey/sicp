
(defun repeat (f n)
  (lambda (x)
    (if (= n 1)
        (funcall f x)
      (funcall f (funcall (repeat f (- n 1)) x)))))

(defun square (x) (* x x))

(print (funcall (repeat 'square 2) 5))


