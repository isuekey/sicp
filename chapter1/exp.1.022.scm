(define (prime? n)
  (= (smallest-divisor n) n))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divisor? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divisor? a b)
  (= (remainder b a) 0))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time) )
      0))

(define (report-prime elapsed-time 0)
  (display " *** ")
  (display elapsed-time) 0)

(define (check n)
  (timed-prime-test n)
  n)
(define (search-for-primes from to)
  (cond ((> from to) 0)
        ((= (remainder from 2) 0) (search-for-primes (+ from 1) to))
        (else (search-for-primes (+ (check from) 1) to))))

(search-for-primes 1000 1100)
(search-for-primes 10000 10100)
(search-for-primes 100000 100100)
(search-for-primes 1000000 1000100)
(search-for-primes 10000000 10000100)
(search-for-primes 100000000 100000100)
(search-for-primes 1000000000 1000000100)
(search-for-primes 10000000000 10000000100)
(search-for-primes 100000000000 100000000100)

;; 按照现代计算机性能来说 1000000000以后的才有有效输出
;; 对比下列结果
(search-for-primes 10000000000 10000000100) ;; 0.08秒左右
(search-for-primes 100000000000 100000000100) ;; 0.25秒左右
;; 符合预期
