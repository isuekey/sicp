
(defun mmap (proc items)
  (if (null items)
      items
    (cons (funcall proc (car items)) (mmap proc (cdr items)))))


(defun square-tree (tree)
  (mmap (lambda (subtree)
          (cond ((null subtree) nil)
                ((not (consp subtree)) (* subtree subtree))
                (t (square-tree subtree)))) tree))

(print (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))
;;; (1 4 (9 16) 25) (36 49))


