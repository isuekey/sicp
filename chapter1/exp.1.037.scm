
;;无穷连分式
;;递归计算过程
(define (cont-frac-r n d k)
  (define (cont-iter i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (cont-iter (+ i 1))))))
  (cont-iter 1))

;; 1/fa = (5^(1/2) - 1)/2 = 0.6180339887498949

(define (ff n)
  (cont-frac-r (lambda (i) 1.0)
               (lambda (i) 1.0)
               n))

(ff 10)
.6179775280898876
(ff 20)
.6180339850173578
(ff 15)
.6180344478216819
(ff 13)
.6180371352785146
(ff 11)
.6180555555555556
;; 11 就够了

;; 迭代计算过程
(define (cont-frac-l n d k)
  (define (cont-iter i r)
    (if (< i 1) r
        (cont-iter (- i 1) (/ (n i) (+ (d i) r)))))
  (cont-iter k 0))
(define (ff n)
  (cont-frac-l (lambda (i) 1.0)
               (lambda (i) 1.0)
               n))
(ff 10)
.6179775280898876
(ff 20)
.6180339850173578
(ff 15)
.6180344478216819
(ff 13)
.6180371352785146
(ff 11)
.6180555555555556




