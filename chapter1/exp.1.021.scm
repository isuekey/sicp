199
1999
19999

(define (smallest-divisor n)
  (find-iter n 2))
(define (find-iter n t)
  (if (= (remainder n t) 0)
      t
      (find-iter n (+ t 1))))

;; 199 199
;; 1999 1999
;; 19999 7

