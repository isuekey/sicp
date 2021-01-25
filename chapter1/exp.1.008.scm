(define (trrt-iter guess x imp)
  (if (good-enough? guess x imp)
      guess
      (trrt-iter (improve guess x) x guess)))
(define (good-enough? guess x imp)
  (< (abs (/ (- guess imp) guess)) 0.0001))
(define (improve guess x)
  (/ (+ (/ x (* guess guess))
        (* guess 2))
     3))
(define (trrt x)
  (trrt-iter 1.0 x x))

;;; 注意要用浮点数，不然会给出个分式，要人命的长，还以为自己错了。
