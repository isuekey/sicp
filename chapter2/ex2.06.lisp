
(defvar zero (lambda (f) (lambda (x) x)))
(defun add-1 (n)
  (lambda (f) (lambda (x) (funcall f (funcall (funcall n f) x)))))

(print zero)
(print (add-1 zero))
(print (add-1 (add-1 zero)))
;;;(print (add-1 (add-1 (add-1 zero))))

;;; add-1 zero
;;; (lambda (f) (lambda (x) (funcall f (funcall (funcall zero f) x))))
;;; (lambda (f) (lambda (x) (funcall f (funcall (lambda (x) x) x))))
;;; (lambda (f) (lambda (x) (funcall f x)))

(defvar one
  (lambda (f) (lambda (x) (funcall f x))))
;;; (add-1 (add-1 zero)) => (add-1 one)
;;; (lambda (f) (lambda (x) (funcall f (funcall (funcall one f) x))))
;;; (lambda (f) (lambda (x) (funcall f (funcall (lambda (x) (funcall f x)) x))))
;;; (lambda (f) (lambda (x) (funcall f (funcall f x))))
(defvar two
  (lambda (f) (lambda (x) (funcall f (funcall f x)))))

(defun church-add (nn mm)
  (lambda (f) (lambda (x)
                (funcall (funcall nn f) (funcall (funcall mm f) x)))))
(print (add-1 one))
(print (church-add one zero))
