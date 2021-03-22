
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

(define acc (make-account 50))
((acc 'deposit) 40)
((acc 'withdraw) 60)
(define acc2 (make-account 60))

#|
E0:
  make-account: ****
  acc:(make-account 50)在E0找到make-accont定义
  acc:(make-account 60)在E0找到make-accont定义
E1:
  P:E0
  balance:50
  withdraw:
  deposit:
  dispatch:
  返回dispatch
;; acc 操作的状态在 E1
E2:
  P:E0
  balance:60
  withdraw:
  deposit:
  dispatch:
  返回dispatch
;; acc2 操作的状态在E2
|#
