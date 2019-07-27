

(defun dd (p)
  (lambda (x) (funcall p (funcall p x))))

(defun inc (x)
  (1+ x))

(print (funcall (funcall (dd (dd 'dd)) 'inc) 5))

;;; 21 = 2^2^2 + 5
