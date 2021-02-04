;; 使用迭代改进过程重写 sqrt 与 fixed-point
;; 迭代改进过程

(define (interactive-improve good? improve)
  (define (test y)
    (let ((next (improve y)))
      (if (good? next y)
          next
          (test next))))
  test)
(define dx 0.000001)
(define (sqrt n)
  ((interactive-improve
    (lambda (x y) (< (abs (- x y)) dx))
    (lambda (y) (/ (+ y (/ n y)) 2))
    )
   1.0))
(sqrt 2)
(sqrt 4)
(sqrt 10003282920)

(define (fixed-point f)
  (interactive-improve
   (lambda (x y) (< (abs (- x y)) dx))
   f))
