
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))
(define a (cons 'a 'b))
(define b (cons 'b 'c))
(define c (cons a b))
(count-pairs c)

(define d (cons a a))
(define e (cons d 'd))
(count-pairs e)

(define f (cons d d))
(count-pairs f)

(define z (cons 'y 'z))
(define y (cons 'x z))
(define x (cons 'w y))
(set-cdr! z x)
(count-pairs x)
;; crash 

