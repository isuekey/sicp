;; 快速幂乘 迭代过程算法

(define (even? n)
  (= (remainder n 2) 0))

(define (loop-expt b n)
  (loop-expt-iter b n 1))

(define (loop-expt-iter b c a)
  (cond ((= c 0) a)
        ((even? c) (loop-expt-iter (square b) (/ c 2) a))
        (else (loop-expt-iter b (- c 1) (* a b)))))

;; (b^n/2)^2 = (b^2)^n/2
;; 2^5 = 2*2^4 = 2 * (2^2)^2 = 2 * (4^2)^1
