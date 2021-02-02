;; 求平方根
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (good-enough? res x)
  (< (abs (- x (square res))) 0.001))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (square x) (* x x))
(define (sqrt x)
  (sqrt-iter 1.0 x))

;; 指数运算
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
(define (expt b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b (- counter 1) (* b product))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))

;; 找素数
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

;; 费马小定理：如果n是素数，a是小于n的正整数，那么 (a^n)%n与a%n相同。
;; 推论是 (a^(n-1))%n = 1
;; a = 1时 显然成立
;; 若 a > 0且 a < n-1时，成立，那么 a+1时 ( a + 1)^n，其中(a+1)<n
;; C(n,0)a^n + C(n,1)a^(n-1)+ ... + C(n, n-1)a + C(n, n)
;; 因为n是素数，那么k>1且k<n时，根据组合的计算方式，
;; C(n, k)必然是n的倍数, 上式简化为 a^n + mn + 1
;; 于是有 ( a + 1)^n % n = (a^n + mn + 1)%n = (a^n + 1) % n
;; 因为 a >0 , a< n-1, 故 (a^n + 1) = p n + a + 1，p为正整数，a + 1 < n
;; 所以 ( a + 1 )^n % n = ( pn + a + 1) %n = a + 1
;; 根据费马小定理进行的检查

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))
;; 上述过程的逆过程可以有效理解为啥这个过程是成立。

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; Miller-Rabin检查
;; 考虑到是概率检测, t的值应该是ceil(log(2,n))+m。
;; 失败的概率小于 (1/2)^m。我们可以取m=64，失败概率就足够小。
;; 那么时间复杂度应该是 log(2,n)^2+64
(define (ceillog n base)
  (define (c-iter nn b exp)
    (if (= nn 0)
        exp
        (c-iter (/ (- nn (remainder nn b)) b) b (+ exp 1))))
  (c-iter (- n 1) base 0))
(define (prime? n)
  (fast-prime? n (+ (square (ceillog n 2)) 64)))
(search-for-primes 10000000000 10000000100)
;; 0.01秒
(search-for-primes 100000000000 100000000100)
;; 0.01秒
(search-for-primes 1000000 1000100)
;; 0.01秒
;; 变化不大

;;1.3.1
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))
(define (cube a)
  (* a a a))
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (pi-sum a b)
  (define (term x)
    (/ 1.0 (* x (+ x 2))))
  (define (next a)
    (+ a 4))
  (* 8 (sum term a next b)))

(pi-sum 1 10000)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b) dx))

(integral cube 0 1 0.001)
(integral cube 0 1 0.0001)
(integral cube 0 1 0.00001)
(integral cube 0 1 0.000001)

