;; exp3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        (error "not enough balance")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch input action)
    (if (eq? input password)
        (cond ((eq? action 'withdraw) withdraw)
              ((eq? action 'deposit) deposit)
              (else (error "unknown action")))
        (error "Incorrect password")))
  dispatch)

(define acc (make-account 100 'onlyyou))
((acc 'onlyyou 'withdraw) 40)
;; ((acc 'onlyyous 'deposit) 50)


(define (make-joint public-acc acc-password new-password)
  (lambda (password action)
    (if (eq? password new-password) (public-acc acc-password action)
        (error "Incorrect password"))))
(define peter-acc (make-account 10000 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
(define woofy-acc (make-joint paul-acc 'rosebud 'lighting))
((peter-acc 'open-sesame 'withdraw) 40)
((paul-acc 'rosebud 'withdraw) 50)
((woofy-acc 'lighting 'withdraw) 60)
((woofy-acc 'lighting 'deposit) 1000)
((peter-acc 'open-sesame 'withdraw) 0)

;; 完全按照意愿进行了工作。
#| 
 但是存在一个问题，如果旧密码错误怎么处理。
 因为这里存在了两种账户，基础账户与共有账户，
 其中共有账户可以别共有账户的基础账户，
 对资金的操作，都可以追溯到基础账户上操作，
 但是密码检查确实每层账户自己问题。
 问题的根源在于：资金是共有的，密码是个人的。
 所以如果增加密码检查的功能，
 是无法仅通过修改上层就能够实现的
|#
