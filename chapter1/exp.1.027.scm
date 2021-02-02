(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
(define (even? n)
  (= (remainder n 2) 0))
(define (square n)
  (* n n))
(define (ceillog n base)
  (define (c-iter nn b exp)
    (if (= nn 0)
        exp
        (c-iter (/ (- nn (remainder nn b)) b) b (+ exp 1))))
  (c-iter (- n 1) base 0))
(define (prime? n)
  (fast-prime? n (+ (square (ceillog n 2)) 64)))

(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)
