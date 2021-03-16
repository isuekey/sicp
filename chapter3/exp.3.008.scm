
(define o -1)
(define (f x)
  (if (< o 0)
      (begin (set! o x) x)
      0))
(+ (f 0) (f 1))
