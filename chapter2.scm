
;; 构造数据抽象
;; 数据抽象将逻辑的处理过程与处理内容隔离开，是有力的设计方法

;; 数据抽象将帮助我们在程序的不同部分之间建立抽象屏障。

;; 复合数据的关键思想是闭包的概念
;;



(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (/ (* (numer x) (numer y))
               (* (denom x) (denom y)))))
(define (div-rat x y)
  (make-rat (/ (* (numer x) (denom y))
               (* (denom x) (numer y)))))
(define (equal-rat x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x)) (display "/") (display (denom x)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;; 使用过程来表示序对，以说明序对是个抽象的概念过程
;; 而不是具体的实现过程
(define (cons a b)
  (define (dispatch m)
    (cond ((= m 0) a)
          ((= m 1) b)
          (else (error "参数只能是0与1 -- 序对" m))))
  dispatch)
(define (car z) (z 0))
(define (cdr z) (z 1))
;; 从而表明数据与过程实现之间没有清晰的边界，其实是一致的

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (maker-interval (min p1 p2 p3 p4)
                    (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
(define (make-interval a b) (cons a b))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; 电阻并联计算总电阻误差的时候
