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

;; lambda 与 let
;; 在scheme里let是lambda的语法糖衣
;; f(x,y) = x(1+xy)^2+y(1-y)+(1+xy)(1-y)
;; 如果 a = 1+xy, b=1-y
;; f(x,y) = x*a^2 + y*b + ab，表达就会简单很多
;; 程序过程也有类似的需求
;;
;; lambda
(define (f x y)
  ((lambda (a b)
          (+ (* x a a)
             (* y b)
             (* a b)))
   (+ 1 (* x y))
   (- 1 y)))
;; let
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x a a)
       (* y b)
       (* a b))))
;;

;; 折半求方程的解

(define (search f neg-point pos-point)
  (let ((middle (/ (+ neg-point pos-point) 2.0)))
    (if (close-enough? neg-point pos-point)
        middle
        (let ((test-value (f middle)))
          (cond ((positive? test-value) (search f neg-point middle))
                ((negative? test-value) (search f middle pos-point))
                (else middle))))))

(define (close-enough? a b)
  (< (abs (- a b)) tolerance))
(define tolerance 0.0001)
(define (positive? a) (> a 0))
(define (negative? a) (< a 0))
(define (cube a) (* a a a))
(search cube -1 2)

;; 增强可用性
(define (half-interval-search f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
          ((and (positive? a-value) (negative? b-value)) (search f b a))
          (else (error "给出的值的正负号需要是相反的")))))

;; 函数不动点 x=f(x)就会有f(x)=f(f(x))=f(f(f(x)))...
(define tolerance 0.0001)
(define (fix-point f first-guess)
  (define (try-guess guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try-guess next))))
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (try-guess first-guess))
(fix-point cube 1)
(define (sqrt x)
  (fix-point (lambda (y) (/ x y))
             1.0))
(define (average x y) (/ (+ x y) 2))
(define (sqrt x)
  (fix-point (lambda (y) (average y (/ x y)))
             1.0))

;;平均阻尼技术
;; 1/2 x = 1/2 f(x) ;;不动点
;; x = 1/2 ( x + f(x)) ;;平均阻尼技术
;; 隐含的数学思想猜想跟微分方程有关。

;; 返回过程

(define (average-damp f)
  (define (average a b) (/ (+ a b) 2))
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fix-point (average-damp (lambda (y) (/ x y)))
             1.0))
(sqrt 4)

(define (cube-root x)
  (fix-point (average-damp (lambda (y) (/ x (* y y))))
             1.0))
(cube-root 1)
1
(cube-root 3)
1.4422317456294458
(cube-root 8)
1.999970920454376
(cube-root 18)
2.620722039185508
(cube-root 27)
3.000022143521597
(cube-root 40)
3.419976592519082


;; 牛顿法
;; x -> g(x) 是可微分的函数，那么g(x)=0的一个解就是 x->f(x)的一个不动点
;; f(x) = x - (g(x) / Dg(x)), Dg(x)是g(x)对x的导数
;; Dg(x) = (g(x + dx) - g(x))/dx

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.000001)
(define (cube x) (* x x x))
((deriv cube) 5)

(define (close-enough? a b)
  (< (abs (- a b)) dx))
;;去掉了教材中 try-guess 直接递归结果
(define (fix-point f guess)
  (let ((next (f guess)))
    (if (close-enough? guess next)
        next
        (fix-point f next))))
;;按照教材写的话
(define (newton-method g)
  (define (newton-transform f)
    (lambda (x)
      (- x (/ (f x) ((deriv f) x)))))
  (fix-point (newton-transform g) 1.0))

(newton-method cube)
(newton-method (lambda (x) (- (cube x) 27)))
;;如果写到一起
(define (newton-method g)
  (fix-point
   (lambda (x)
     (- x (/ (g x) ((deriv g) x))))
   1.0))
(newton-method cube)
(newton-method (lambda (x) (- (cube x) 27)))

(define (sqrt x)
  (newton-method (lambda (y) (- (* y y) x))))
(sqrt 2)
(sqrt 4)
(sqrt 10)

;; 抽象与第一级过程
(define (fixed-point f guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try guess))
(define tolerance 0.00001)
(define (close-enough? a b)
  (< (abs (- a b)) tolerance))
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
(define (average-damp f)
  (define (average a b) (/ (+ a b) 2))
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (/ x y))
   average-damp
   1.0))
(sqrt 2)
(sqrt 4)
(sqrt 10)
;; newton
(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (- (* y y) x))
   (lambda (g)
     (lambda (y) (- y (/ (g y) ((deriv g) y)))))
   1.0))
(sqrt 2)
(sqrt 4)
(sqrt 10)
;; guess 应该显示的使用
;; 程序抽象的过程应该理解为语义化算法的过程，而不是理解为屏蔽细节的过程。
;; 屏蔽细节仅仅是语义化的附属产物。
;; 所以上面装逼式的写到一起只能是个傻逼。还是按照教材的写，才有更好的发展。
;; 不要将算法优化的理念与算法实现的理念混淆到一起。

;; 第一级状态（带有最小限制的元素）
;;; 可以用变量命名
;;; 可以提供给过程作为参数
;;; 可以由过程作为结果返回
;;; 可以包含在数据结构中
;;;


