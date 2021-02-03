;; tan x
;; Ni = if i = 1, then x , else x^2
;; Di = 2i - 1

(define (tan-cf x k)
  (define (cf-iter i r n d)
    (if (< i 1) r
        (cf-iter (- i 1) (/ (n i) (- (d i) r)) n d)
        ))
  (cf-iter k 0.
           (lambda (i) (if (= i 1) x (* x x)))
           (lambda (i) (- (* 2 i) 1))))

