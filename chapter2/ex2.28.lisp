

(defvar x (list (list 1 2) (list 3 4)))

(defun fringe (alist)
  (if (null alist)
      nil
    (let ((ele (car alist))
          (subl (cdr alist)))
      (if (consp ele)
          (append (fringe ele) (fringe subl))
        (cons ele (fringe subl))))))
(print (fringe x))
(print (fringe (list  x x)))
