
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

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((width (/ (* c p) 100)))
    (make-interval (- c width)
                   (+ c width))))
(define (percent i)
  (* (/ (width i) (center i)) 100))
;; 使用make-center-width进行定义
;; (define (make-center-percent c p)
;;   (make-center-width c (/ (* c p) 100)))

;; 使用 make-center-width 定义 c > 0, w > 0, w/c < 0.01
;; x (xc, xw), y (yc, yw)
;; x * y = ((xc - xw)*(yc - yw), ((xc + xw)*(yc + yw)))
;; = (xcyc-xcyw-xwyc+xwyw, xcyc+xcyw+xwyc+xwyw)
;; c(xy) = xcyc + xwyw => xcyc
;; w(xy) = xcyw + xwyc
;; p(xy) = w(xy)/c(xy) => yw/yc + xw/xc = p(x) + p(y)
;; 如果相对误差很小的时候，
;; 乘积区间误差 近似等于 因子误差之和
;; 其实应该用 LaTex 来写推导的，但是不同语言杂凑一起也不合适。
;;

(define (mul-interval-percent x y)
  (make-interval (* (center x) (center y)) (+ (percent x) (percent y))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2) (add-interval r1 r2)))
;;1/(1/R1+1/R2)
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define r1 (make-center-percent 40 1))
(define r2 (make-center-percent 40 0.5))
(div-interval r1 r1)
(percent (div-interval r1 r1))
;; 1 + 1
(div-interval r1 r2)
(percent (div-interval r1 r2))
;; 1 + 0.5

(define pr1 (par1 r1 r2))
(define pr2 (par2 r1 r2))
(display pr1)
(display (percent pr1))
(display pr2)
(display (percent pr2))

;; pr2 显然比 pr1要好的多
;; 
