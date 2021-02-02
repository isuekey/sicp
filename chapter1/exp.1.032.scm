
;; accumulate
;; 线性递归计算过程
(define (accumulate combine null-value term a next b)
  (if (> a b)
      null-value
      (combine (term a) (accumulate combine null-value term (next a) next b))))


(define (identity a) a)
(define (inc a) (+ a 1))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(sum identity 1 inc 100)

(define (prod term a next b)
  (accumulate * 1 term a next b))
(prod identity 1 inc 6)

;; 迭代计算过程
(define (accumulate combine null-value term a next b)
  (define (accumulate-iter a result)
    (if (> a b)
        result
        (accumulate-iter (next a) (combine result (term a)))))
  (accumulate-iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(sum identity 1 inc 100)

(define (prod term a next b)
  (accumulate * 1 term a next b))
(prod identity 1 inc 6)



