
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                    (max p1 p2 p3 p4))))
(define (div-interval x y)
  (if (and (<= 0 (lower-bound y))
           (>= 0 (upper-bound y)))
      (error "区间越界了")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))
(define upper-bound cdr)
(define lower-bound car)

(define (sub-interval x y)
  (add-interval x
                (make-interval (- 0.0 (upper-bound y))
                               (- 0.0 (lower-bound y)))))
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

;; 区间 (2,3), (5,7)
(define interval-a (make-interval 2 3))
(define interval-b (make-interval 5 7))
(display (width interval-a))
(display (width interval-b))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                    (max p1 p2 p3 p4))))
;; 这个首先要抽象出来形式
;; p1 = ac
;; p2 = ad
;; p3 = bc
;; p4 = bd
;; a >= 0
;;   c >=0 (p1 p4)
;;   else d >=0 (p3 p4)
;;        else (p3 p4)
;; a < 0 且 b >=0
;;   c >= 0 (p2 p4)
;;   else d >=0 ((min p2 p3), p4) ;;这是最复杂情况 待定
;;        else (p3 p1)
;; b < 0
;;   c >= 0 (p2, p3)
;;   else d >= 0 (p2, p1)
;;        else (p4, p1)
;; 剩下的问题是解决 a<0, c<0, b>0, d>0时，p1,p2,p3,p4的极值问题
;; m1,w1,m2,w2分别为x,y的中点与宽, w1.w2大于0
;; 而且m1的绝对值小于w1，m2的绝对值小于w2
;; p1 = a*c = (m1-w1)(m2-w2) = m1m2 - m1w2 - w1m2 + w1w2;
;; p2 = a*d = (m1-w1)(m2+w2) = m1m2 + m1w2 - w1m2 - w1w2;
;; p3 = b*c = (m1+w1)(m2-w2) = m1m2 - m1w2 + w1m2 - w1w2;
;; p4 = b*d = (m1+w1)(m2+w2) = m1m2 + w1m2 + m1w2 + w1w2;
;; m1 >= 0
;;   m2 >= 0 ((min (p2,p3)), p4) ;; 这里没想到啥好办法
;;   else (p3, p4)
;; else m2 >= 0 (p2, p4)
;       else ((min (p2, p3)), p4) ;; 这里没想到啥好办法
;; p2 - p3 = 2m1w2 - 2w1m2
;; 分析到这里 我没想到啥好办法
;; 无论怎样都需要三次乘法计算
;; 考虑 (-1, 5)*(-2, 7) 范围 (-10, 35)
;; (-2, 4)*(-2, 7) 的范围 (-14, 28)
;; 想达到最多两次的计算需要我在数学上有所突破
;;
;; 剩下的实现就不搞了
;; 用cond写写就成了
;; 心灰意冷啊 :( :( :(
;; 


