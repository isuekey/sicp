
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
((acc 'onlyyous 'deposit) 50)


