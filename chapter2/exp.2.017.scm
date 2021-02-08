
(define (last-pair items)
  (define (iter last subs)
    (if (null? subs)
        last
        (iter subs (cdr subs))))
  (iter items (cdr items)))
(define squares (list 1 4 9 16 25 36 49))
(define odds (list 1 3 5 7 9 11))

(last-pair odds)
(last-pair squares)

;; 上面是一个迭代计算
;; 我需要一个递归计算

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))
(last-pair odds)
(last-pair squares)
      
;; 这不是找别扭，重复造轮子，而是一种强化训练
