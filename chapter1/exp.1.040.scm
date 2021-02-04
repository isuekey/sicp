
;; x^3+ax^2+bx+c
;;
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess)
  )
(define (fixed-point f guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next))))
  (try guess))
(define (close-enough? a b)
  (< (abs (- a b)) dx))
(define dx 0.000001)
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

(newton-method (cubic 1 1 1) 1)

