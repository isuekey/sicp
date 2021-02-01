;; 用价法和特殊过程表示乘法 递归计算过程

(define (f a b)
  (if (= b 0)
      0
      (+ a (f a (- b 1)))))

(define (even? n)
  (= (remainder n 2) 0))
(define (halve n) (/ n 2))
(define (double n) (+ n n))

(define (ff a b)
  (cond ((= b 0) 0)
        ((even? b) (ff (double a) (halve b)))
        (else (+ a (ff a (- b 1))))))
