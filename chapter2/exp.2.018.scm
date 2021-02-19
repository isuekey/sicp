
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
;; 更有可能是我没有想清楚

;; 就是没有想清楚
;; 考察上面的递归计算结果
((((((() . 11) . 9) . 7) . 5) . 3) . 1)
;; 跟正确的结果的差异在于多了一个括号，
;; 其实应该是向后，而不是先向前
(() . (11 . (9 . (7 . (5 . (3 . (1)))))))
;; 所以要在递归过程中建立序对cons
;; 原始数据
(1 3 5 7 9 11)
;; 1 要 放到 递归的末尾来执行，这就要求从尾部向前递归
;; 所以之前写的一定是有问题
;; 这就需要逆序 便利与读取
;; 使用last-ref获取尾部元素
;; 使用length 获取长度
(define (length items)
  (define (iter subs count)
    (if (null? subs)
        count
        (iter (cdr subs) (+ count 1))))
  (iter items 0))
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define (reverse items)
  (let ((item-length (length items)))
    (define (iter idx)
      (if (< idx 0)
          ()
          (cons (list-ref items idx) (iter (- idx 1)))))
    (iter (- item-length 1))))
odds
squares
(reverse odds)
(reverse squares)

