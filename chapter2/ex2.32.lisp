
;;;(1 2 3)
;;;(() (1) (2) (3) (1 2) (2 3) (3 1) (1 2 3))

(defun mmap (proc items)
  (if (null items)
      items
    (cons (funcall proc (car items)) (mmap proc (cdr items)))))

(defun subsets (s)
  (if (null s)
      (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (mmap (lambda (x) (cons (car s) x)) rest)))))

(print (subsets (list 1 2 3)))
