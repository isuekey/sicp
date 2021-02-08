
;; 预期
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;; (1 (4 (9 16) 25) (36 49))

;; 直接定义
(define (square-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (square tree))
        (else
         (cons (square-tree (car tree))
               (square-tree (cdr tree)))))
  )
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;; map定义
(define (square-tree tree)
  (map (lambda (x)
         (if (pair? x)
             (square-tree x)
             (square x)))
       tree))
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;; 很顺利
