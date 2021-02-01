;; 用价法和特殊过程表示乘法 迭代计算过程

(define (even? n)
  (= (remainder n 2) 0))
(define (halve n) (/ n 2))
(define (double n) (+ n n))

(define (lf a b)
  (if (= b 0)
      0
      (lf-iter a b 0)))
(define (lf-iter a b r)
  (cond ((= b 1) (+ a r))
        ((even? b) (lf-iter (double a) (halve b) r))
        (else (lf-iter a (- b 1) (+ a r)))))
