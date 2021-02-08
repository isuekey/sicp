
;; 预期
(define (square-tree tree) (tree-map square tree))
;; 要正常工作，需要

(define (tree-map proccess tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (proccess tree))
        (else
         (cons (tree-map proccess (car tree))
               (tree-map proccess (cdr tree)))))
  )
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;; 工作顺利

