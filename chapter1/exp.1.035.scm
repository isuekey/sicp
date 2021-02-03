
;; 黄金分隔 f=(1+5^(1/2))/2 => f(f-1)=1 => f = 1+1/f

(define tolerance 0.0001)
(define (fix-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (try first-guess))

(define ff
  (fix-point (lambda (a) (+ 1 (/ 1 a))) 1.0))

