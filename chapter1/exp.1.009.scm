(define (m+ a b)
  (if (= a 0)
      b
      (inc (m+ (dec a) b))))
(define (n+ a b)
  (if (= a 0)
      b
      (n+ (dec a) (inc b))))
(define (inc a) (+ 1 a))
(define (dec a) (- a 1))

;; m+ 递归计算过程
;; n+ 迭代递归过程
