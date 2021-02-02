;; 用于产生product方法

(define (prod-rur term a next b)
  (if (> a b)
      1
      (* (term a) (prod-rur term (next a) next b))))

(define (identity a) a)

(define (inc a) (+ a 1))

(define (fractorial-rur n)
  (prod-rur identity 1 inc n))
(fractorial-rur 4)
(fractorial-rur 5)
(fractorial-rur 6)
(fractorial-rur 7)

(define (even? n)
  (= (remainder n 2) 0))
(define (pi-prod n)
  (define (term a)


