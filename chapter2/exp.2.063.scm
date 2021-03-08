(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entiry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))


;; 结果应该是相同的
;; 结果都是
;; (1 3 5 7 9)

;; 转换时，计算步骤两者类似，但是空间list-2可能少些。
;; 都采用了分治的方式处理，然后拼接
;; 不清楚空间的使用效果，直觉上也没多少差别
;; 
