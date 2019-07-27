
;; f(x) g(x) f在g之后复合 f(g(x))


(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

(defun sq (x) (* x x))
(defun inc (x) (1+ x))

(print (funcall (compose 'sq 'inc) 6))
