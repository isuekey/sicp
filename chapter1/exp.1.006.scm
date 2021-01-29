(define (new-if pre then-clause alt-clause)
  (cond (pre then-clause)
        (else alt-clause)))
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (good-enough? res x)
  (< (abs (- x (square res))) 0.001))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (square x) (* x x))
(define (sqrt x)
  (sqrt-iter 1.0 x))

;;; new-if 是普通式会求算所有参数式的值，导致递归展开
