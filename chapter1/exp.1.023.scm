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

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time) 0)

(define (check n)
  (timed-prime-test n)
  n)
(define (search-for-primes from to)
  (cond ((> from to) 0)
        ((= (remainder from 2) 0) (search-for-primes (+ from 1) to))
        (else (search-for-primes (+ (check from) 1) to))))

;; (search-for-primes 1000 1100)
;; (search-for-primes 10000 10100)
;; (search-for-primes 100000 100100)
;; (search-for-primes 1000000 1000100)
;; (search-for-primes 10000000 10000100)
;; (search-for-primes 100000000 100000100)
;; (search-for-primes 1000000000 1000000100)
;; (search-for-primes 10000000000 10000000100)
;; (search-for-primes 100000000000 100000000100)

;; 按照现代计算机性能来说 1000000000以后的才有有效输出
;; 对比下列结果
(search-for-primes 10000000000 10000000100)
;; 原始0.08秒左右
(search-for-primes 100000000000 100000000100)
;; 原始0.25秒左右
;; 符合预期

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divisor? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (next a)
  (if (= a 2)
      3
      (+ a 2)))
(search-for-primes 10000000000 10000000100)
;; next 0.05秒
(search-for-primes 100000000000 100000000100)
;; next 0.16秒
;; 达不到2的增效 因为引入了检查判断

(define (smallest-divisor n)
  (if (divisor? n 2)
      2
      (find-divisor n 3)))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divisor? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (next a)
  (+ a 2))
(search-for-primes 10000000000 10000000100)
;; 使用初始化检查后 0.04秒
(search-for-primes 100000000000 100000000100)
;; 使用初始化检查后 0.13秒
;; 符合2的增效，因为只作了一次检查判断

;; 如果追求性能的话，很多防御性的工作需要在外层处理，
;; 内层应该尽量减少防御代码


