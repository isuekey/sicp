
;; f(x), f^n(x) = f(f(f...(f(x))))

(define (repeat g n)
  (if (= n 1)
      g
      (lambda (x)
        (g ((repeat g (- n 1)) x)))
      ))

(define (square x) (* x x))
((repeat square 2) 5)
