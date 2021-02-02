(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))
;; 这种方式计算出base^exp的结果再求余，
;; 在素数检查的时候exp都很大，导致base^exp的结果非常大
;; 超出程序处理的范围了
;; 如果用于fast-prime?，通常会导致系统崩溃
