
;; 尝试重新写找零过程

(define (cc amount coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (null? coins)) 0)
        (+ (cc amount (cdr coins))
           (cc (- amount (car coins)) coins))))

(define cny-coins (list 1 2 5 10 50))
(cc 1 cny-coins)
(cc 2 cny-coins)
(cc 5 cny-coins)
(cc 10 cny-coins)

;; 结果不正确，因为过程抽象的不对
;; 两个变量 总额，零钱种类与面额
;; 当前总额 其他币种找零， 与 当前总额-面额 的币种找零
;;
;; 上面对找零过程的理解实现有问题 没有遍历零钱
;; 
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
;; 但是如果按照 list-ref 方式写的话有老调重弹的感觉
;; 还是先体会一下教材的碾压

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(cc 100 us-coins)

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else )))
;; 等等我知道错那了，我理解的没有问题，代码写的有问题
(define (cc amount coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (null? coins)) 0)
        (else (+ (cc amount (cdr coins))
           (cc (- amount (car coins)) coins)))))
(cc 1 cny-coins)
(cc 2 cny-coins)
(cc 5 cny-coins)
(cc 10 cny-coins)
(cc 100 us-coins)
292
;; 顺序是没有影响的
(define us-coins (list 25 10 50 5 1))
(cc 100 us-coins)
292
