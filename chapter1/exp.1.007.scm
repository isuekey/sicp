(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (square x) (* x x))


(define (sqrt-iter guess x oldGuess)
  (if (good-enough? guess x oldGuess)
      guess
      (sqrt-iter (improve guess x) x guess)))
;(define (good-enough? res x)
;  (= (improve res x) res))
(define (sqrt x)
  (sqrt-iter (improve 1. x) x 1.0))
(define (good-enough? guess x oldGuess)
  (<= (abs (- x (square guess)))
      (abs (- x (square oldGuess)))
      ))

;; the best precision
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (good-enough? guess x)
  (= (improve guess x) guess))
(define (sqrt x)
  (sqrt-iter 1.0 x))
;; (sqrt 0.) will Floating-point underflow


;; 使用梯度检查
(define (sqrt-iter guess x oldGuess)
  (if (good-enough? guess x oldGuess)
      guess
      (sqrt-iter (improve guess x) x guess)))
(define (good-enough? guess x oldGuess)
  (<= (abs (- guess oldGuess))
      (* guess 0.001)))
(define (sqrt x)
  (sqrt-iter 1.0 x 2.0))
