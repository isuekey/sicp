;; Miller-Rabin 检查

(define (prime? n)
  (fast-prime? n (+ (ceillog n 2) 64)))

(define (ceillog n b)
  (ceillog-iter (- n 1) b 0))
(define (ceillog-iter n b exp)
  (if (= n 0)
      exp
      (ceillog-iter (/ (- n (remainder n b)) b) b (+ exp 1))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((not (mr-check-prime n (+ (random (- n 1)) 1))) false)
        (else (fast-prime? n (- times 1)))))
(define (mr-check-prime n a)
  (= (mr-expmod a n (- n 1)) 1))

(define (mr-expmod a n exp)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (mr-expmod a n (/ exp 2))) n))
        (else (remainder (* a (mr-expmod a n (- exp 1))) n))))
(define (even? n)
  (= (remainder n 2) 0))

;; Miller-Rabin检查 上面就可以了。
;; 但是要求在中途检测到a的幂乘取模n为1的非平凡平方根解时退出求模过程返回0
;; 就是说除了1,n-1其他的数的平方对n取模结果不能为1。
;; 就需要修改expmod为迭代计算过程
(define (mr-expmod-lp a n target curExp totalExp cr tr)
  (cond ((= a 1) 1)
        ((= target totalExp) tr)
        ((and (= (* cr tr) 1) (< a target)) 0)
        ((< (+ totalExp (* curExp 2)) target)
         (mr-expmod-lp a n target (* curExp 2) totalExp (remainder (* cr cr) n) tr))
        ((= (+ totalExp (* curExp 2)) target)
         (mr-expmod-lp a n target 0 (+ totalExp curExp curExp) 1 (remainder (* cr cr tr) n)))
        (else
         (mr-expmod-lp a n target 1 (+ totalExp curExp) a (remainder (* tr cr) n)))
        ))
(define (mr-expmod a n exp)
  (mr-expmod-lp a n exp 1 0 a 1))

;; 非常不好意思，mr-expmod-lp这个过程写的稀烂
;; 举例说明 3^7 = 3^4 * 3^2 * 3^1 用了2log(2,n)次的求余操作。
;; 1直接略过
;; n-1 需要一直检测到结束，因为如果n-1是奇数的话，结果是n-1
;; 但是n-1就不应该是奇数，
;; (n-1)^(n-1)%n
;; even ((n-1)^(2m))%n = (n^2 - 2n + 1)^m%n = 1;
;; odd ((n-1)^(2m)(n-1))%n = (n-1)%n = n-1

(prime? 3)
(prime? 199)
(prime? 1999)
(prime? 19999)
(prime? 199999)
(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)
(prime? 1000081)

;; 回顾代码 有个变换 没有用上 (b^2)(n/2) = (b^(n/2))^2
(define (mr-expmod-lp2 a n exp i r)
  (cond ((= i 0) (remainder r n))
        ((even? i) (mr-expmod-lp2 (remainder (square a) n) n exp (/ i 2) r))
        (else (mr-expmod-lp2 a n exp (- i 1) (remainder (* r a) n)))))
(define (even? a) (= (remainder a 2) 0))
(define (square a) (* a a))
        
(define (mr-expmod a n exp)
  (mr-expmod-lp2 a n exp exp 1))
(prime? 3)
(prime? 199)
(prime? 1999)
(prime? 19999)
(prime? 199999)
(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)
(prime? 1000081)
;; 算法优化更多的是数学上证明过程的优化
;; 而不是实现过程上的优化


