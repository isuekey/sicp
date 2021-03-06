
(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
;; (set! balance (- balance amount))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! (balance (+ balance amount)))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (elese (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

;; (define random-init <??>)
;; (define random-update <??>)
(define rand
  (let ((x random-init))
    (lambda ()
      (set1 x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trails))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
;; 如果只用 rand-update
(define (estimate-pi trails)
  (sqrt (/ 6 (random-gcd-test trials random-init))))
(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trails-remaining 1)
                     (+ trails-passed 1)
                     x2))
              (else
               (iter (- trails-remaining 1)
                     trails-passed
                     x2))))))
  (iter trails 0 initial-x))



;; 3.1.3 命令式程序设计的缺陷
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))


(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x ) (square y)))
(define (f a)
  (sum-of-square (+ a 1) (* a 2)))
#|
E0: 
  square: (* x x)
  sum-of-squares: (+ (square x) (square y))
  f: (sum-of-squares (+ a 1) (* a 2))
  +,*...
|#
(f 5)
#|
E1:
  P:E0
  a:5
  (sum-of-squares (+ a 1) (* a 2)) E1找不到sum-of-square然后到E0中找到
  (sum-of-squares 6 10)
  E2 返回 136
|#
#|
E2
  P:E1
  x:6
  y:10
  (+ (square x) (square y)) E2找不square-》E1没有-》E0中找到
  (+ (square 6) (square 10))
  E3 E4 依次返回响应（顺序不一定，看编译器的具体实现）
  (+ 36 100)
|#
#|
E3
  P:E2
  x:6
  (square x) E3找不square-》E2没有-》E1没有-》E0中找到
  (* 6 6)
|#
#|
E4
  P:E2
  x:10
  (square x) E4找不square-》E2没有-》E1没有-》E0中找到
  (* 10 10)
|#

;; 内部定义

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
