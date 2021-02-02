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

;; pi/4 = 2/3 * 4/3 * 4/5 * 6/5 * 6/7 * 8/7
(define (even? n)
  (= (remainder n 2) 0))

(define (pi-term a)
  (if (even? a)
      (/ (+ a 2) (+ a 1))
      (/ (+ a 1) (+ a 2))
      ))

(define (pi-prod n)
  (* 4.0 (prod-rur pi-term 1 inc n)))

(pi-prod 100)
;; 3.1570301764551676

(define (prod-lp term a next b)
  (define (prod-lp-iter k r)
    (if (> k b)
        r
        (prod-lp-iter (next k) (* r (term k)))
        ))
  (prod-lp-iter a 1))
(define (fractorial-lp n)
  (prod-lp identity 1 inc n))
(fractorial-lp 4)
(fractorial-lp 5)
(fractorial-lp 6)
(fractorial-lp 7)

(define (pi-prod-lp n)
  (* 4.0 (prod-lp pi-term 1 inc n)))

(pi-prod-lp 100000)
