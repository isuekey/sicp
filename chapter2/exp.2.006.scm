
;; 没有数字的 有理数操作
;; Church 计数
;; 有理数是描述步长1有多少的概念
;; 1 = +1;
;; 2 = +1+1;
;; 3 = +1+1+1;
;; ……

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))

;; one = (add-1 zero); two = (add-1 one);
;; 计算出来
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(add two one)
;; (lambda (f) (lambda (x) ((lambda (x) (f (f x))) (f x))))
;; (lambda (f) (lambda (x) (f (f (f x)))))
;; 成立的
;; 开始的时候：蒙的成分比较大，只是感觉形式上是对的。
;; 在最后验证的时候才大约明白了一些
;;
;; 这里需要知道什么是有理数。有理数的定义
;; 
