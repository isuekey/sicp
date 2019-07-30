

(defun mcons (x y)
  (lambda (m) (funcall m x y)))

(defun mcar (z)
  (funcall z (lambda (p q) p)))
(defun mcdr (z)
  (funcall z (lambda (p q) q)))

(defvar mm (mcons 2 3))
(print (mcar mm))
(print (mcdr mm))
