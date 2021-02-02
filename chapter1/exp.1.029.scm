(define (cube a)
  (* a a a))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

;; 辛普森规则f的在[a,b]上的定积分
;; n是偶数, h=(b-a)/n, y(k)=f(a + kh)
;; h/3[y(0)+4y(1)+2y(2)+4y(3)+2y(4)+……+2y(n-2)+4y(n-1)+yn]

(define (even? a)
  (= (remainder a 2) 0))

(define (ss f a b m)
  (define (n) (* m 2))
  (define (h)
    (/ (- b a) (n)))
  (define (y k)
    (cond ((= k 0) (f a))
          ((= k (n)) (f b))
          ((even? k) (* 2 (f (+ a (* k (h))))))
          (else (* 4 (f (+ a (* k (h))))))))
  (define (ss-iter nn k result)
    (if (> k nn)
        result
        (ss-iter nn (+ k 1) (+ result (y k)))))
  (/ (* (h) (ss-iter (n) 0 0)) 3.0)
  )

(ss cube 0 1 50)
(ss cube 0 1 500)
