

(defun mmap (proc items)
  (if (null items)
      items
    (cons (funcall proc (car items)) (mmap proc (cdr items)))))

(defun abslist (alist)
  (mmap 'abs alist))


(print (abslist (list -1 5 7 -10 20 -33)))
