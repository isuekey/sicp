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
;; 若 a > 0且 a < n-1时，成立，那么 a+1时 ( a + 1)^n， (a+1)需要<n
;; C(n,0)a^n + C(n,1)a^(n-1)+ ... + C(n, n-1)a + C(n, n)
;; 因为n是素数，那么k>1且k<n时，根据组合的计算方式，
;; C(n, k)必然是n的倍数, 上式简化为 a^n + mn + 1
;; ( a + 1)^n % n = (a^n + 1)%n
;; 因为 a >0 , a< n-1, 故 (a^n + 1) = p n + a + 1
;; 所以 ( a + 1)^n % n = ( pn + a + 1) %n = a + 1

;; 根据费马小定理进行的检查
