
(equal? '(this is a list) '(this is a list))
#t
(equal? '(this is a list) '(this (is a) list))
#f

(define (equal?54 x y)
  (cond ((and (null? x) (null? y)) #t)
        ((or (null? x) (null? y)) #f)
        (else
         (and (eq? (car x) (car y))
              (equal?54 (cdr x) (cdr y))))))
(equal?54 '(this is a list) '(this is a list))
#t
(equal?54 '(this is a list) '(this (is a) list))
#f

