
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))
;; let 是 lambda 的语法糖衣
;; 等价如下
(define (make-withdraw initial-amount)
  ((lambda (balance)
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))) intial-amount))
#|
E0:
  make-withdraw:  ((lambda (balance) (lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds"))) intial-amount))
|#
(define W1 (make-withdraw 100))
#|
E0:
  make-withdraw:  ((lambda (balance) (lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds"))) intial-amount))
  W1: (make-withdraw 100) E0找到make-withdraw
E1:
  P:E0
  initial-amount:100
  call:((lambda (balance) (lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds"))) intial-amount))
E2:
  P:E1
  balance:100
  call:(lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds")))
逐步返回给W1:(lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds"))) with E2
|#
(W1 50);; E0找到W1定义
#|
E3:
  P:E2
  amount:50
  (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds"))
  balance in E2
|#
(define W2 (make-withdraw 100))
#|
E0:
  make-withdraw:  ((lambda (balance) (lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds"))) intial-amount))
  W1: (make-withdraw 100) E0找到make-withdraw -> E2.call
  W2: (mame-withdraw 100) E0找到make-withdraw -> E5.call
E4:
  P:E0
  initial-amount:100
  ((lambda (balance) (lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds"))) intial-amount))
E5:
  P:E4
  balance:100
  (lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds")))
逐步返回给W1:(lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds"))) with E5
|#
