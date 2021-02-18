
(define squares (list 1 4 9 16 25 36 49))
(define odds (list 1 3 5 7 9 11))

;; 递归计算很容易
(define (reverse items)
  (if (null? items)
      items
      (cons (reverse (cdr items)) (car items)))
  )

(reverse odds)
(reverse squares)
;; 啪啪打脸
;; (list 1 2 3 4) 与 (cons 1 (cons 2 (cons 3 (cons 4 nil)))) 等价
;; 所以结果是不对的

(define (reverse items)
  (define (iter subs reversed)
    (if (null? subs)
        reversed
        (iter (cdr subs) (cons (car subs) reversed))))
  (iter items ()))
(reverse odds)
(reverse squares)
;; 上述过程是个迭代计算过程，再考虑一个递归计算过程
;; 这是比较少见的迭代计算更容易实现的案例。

