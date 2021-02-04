
;; smooth(f(x))
;; (f(x-dx) + f(x) + f(x+dx))/3

(define (smooth g)
  (lambda (x)
    (/ (+ (g (- x dx)) (g x) (g (+ x dx))) 3)))
(define dx 0.000001)

(define (repeat f n)
  (define (iter f i r)
    (if (not (< i n))
        r
        (iter f (+ i 1)
              (lambda (x) (f (r x))))))
  (iter f 1 f))
(define (square x) (* x x))
((repeat square 2) 5)

(define (repeat-smooth g n)
  ((repeat smooth n) g))
((repeat-smooth square 2) 5)

