
(defun f (g)
  (funcall g 2))

(defun square (x) (* x x))

(print (f 'square))

(print (f (lambda (z) (* z (+ z 1)))))

(print (f 'f))
