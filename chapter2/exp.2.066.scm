;; 二叉树表示集合
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))


;; 二叉树表示记录的集合

(define less? <)
;; 修改 less?谓词的过程实现具体的效果

(define (look-up given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (entry set-of-records))) (entry set-of-records))
        ((less? given-key (key (entry set-of-records))) (look-up given-key (left-branch set-of-records)))
        (else (look-up given-key (right-branch set-of-records)))))


