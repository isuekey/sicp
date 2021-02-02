;; 过滤器的概念

(define (filter a check null-value term)
  (if (check a)
      (term a)
      null-value))
(define (filtered-accumulate combine null-value term a next b check)
  (if (> a b)
      null-value
      (combine (filter a check null-value term) (filtered-accumulate combine null-value term (next a) next b check))))

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

(define (identity a) a)
(define (inc a) (+ a 1))

(define (sum-prime a b)
  (filtered-accumulate + 0 identity a inc b prime?))
(sum-prime 2 10)
(sum-prime 99 101)

(define (gcd a b)
  (if (= (remainder a b) 0)
      b
      (gcd b (remainder a b))))

(define (prod-r-prime n)
  (define (r-prime a)
    (= (gcd a n) 1))
  (filtered-accumulate * 1 identity 1 inc n r-prime))
(prod-r-prime 10)
