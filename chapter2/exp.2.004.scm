
;; (car (cons x y)) => x

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

;; 定义 cdr
(define (cdr z)
  (z (lambda (p q) q)))

;; 重新看看了啥是代换模型。发现没有明确的定义。

(define cc (cons 1 2)) ;; cc = (lambda (m) (m 1 2))
(car cc);; (cc z) z=(lambda (p, q) p) => (z 1 2)
1
(cdr cc);; (cc z) z=(lambda (p, q) q) => (z 1 2)
2
;;因为用的是lambda，没有命名不好描述
