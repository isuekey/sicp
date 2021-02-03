;; Ni = 1
;; Di = 1,2,1, 1,4,1, 1,6,1, 1,8,1, ...

(define (cont-frac-l n d k)
  (define (cont-iter i r)
    (if (< i 1) r
        (cont-iter (- i 1) (/ (n i) (+ (d i) r)))))
  (cont-iter k 0))

(define (ola n)
  (cont-frac-l
   (lambda (i) 1)
   (lambda (i) (if (= (remainder i 3) 2)
                   (* (/ (+ i 1) 3) 2)
                   1.0))
   n))
(+ (ola 10) 2)
(+ (ola 20) 2)
(+ (ola 30) 2)

