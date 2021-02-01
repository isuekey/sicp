

(defun mforeach (proc list)
  (if (null (car list))
      nil
    (funcall proc (car list)))
  (if (null (cdr list))
      nil
    (mforeach proc (cdr list))))


(mforeach (lambda (x) (print x)) (list 57 321 88))
