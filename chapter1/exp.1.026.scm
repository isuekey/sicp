
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))
;; log(2,n)次求余操作

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (* (expmod base (/ exp 2) m)
             (expmod base (/ exp 2) m))
          m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
;;log(2,n)个2连乘的求余操作 2*2……2 = 2^(log(2,n))=n次求余操作。

