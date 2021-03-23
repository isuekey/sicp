
(define (hasit pair array)
  (cond ((null? array) #f)
        ((equal? pair (car array)) #t)
        (else (hasit pair (cdr array)))))
      
(define (count-pairs x)
  (let ((counted (list )))
    (define (iter-count p)
      (cond ((not (pair? p)) 0)
            ((hasit (car p) counted) (iter-count (cdr p)))
            ((hasit (cdr p) counted) (iter-count (car p)))
            ((equal? (car p) (cdr p))
             (set! counted (cons p counted))
             (+ (iter-count (car p)) 1))
            (else
             (set! counted (cons p counted))
             (+ (iter-count (car p))
                (iter-count (cdr p))
                1))))
    (iter-count x)))
(define a (cons 'a 'b))
(define b (cons 'b 'c))
(define c (cons a b))
(count-pairs c)

(define d (cons a a))
(count-pairs d)
(define e (cons d 'd))
(count-pairs e)

(define f (cons d d))
(count-pairs f)

(define z (cons 'y 'z))
(define y (cons 'x z))
(define x (cons 'w y))
(set-cdr! z x)

(count-pairs x)

;; 不符合预期。明天再说

