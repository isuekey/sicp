
(defun mmap (proc items)
  (if (null items)
      items
    (cons (funcall proc (car items)) (mmap proc (cdr items)))))

(defun square-list-1 (items)
  (if (null items)
      nil
    (cons (* (car items) (car items)) (square-list-1 (cdr items)))))

(defun square-list-2 (items)
  (mmap (lambda (x) (* x x)) items))







(print (square-list-1 (list 1 2 3 4)))
;;; (1 4 9 16)
(print (square-list-2 (list 1 2 3 4)))
