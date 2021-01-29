;; recursive rf
(define (rf n)
  (cond
   ((<= n 3) n)
   (else
    (+ (rf (- n 1))
       (* 2 (rf (- n 2)))
       (* 3 (rf (- n 3)))
       )
    )
   )
  )

;; loop lf
(define (lf-iter n a b c)
  (if (= n 0)
      a
      (lf-iter (- n 1) (+ a (* 2 b) (* 3 c)) a b)))
(define (lf n)
  (if (<= n 3)
      n
      (lf-iter (- n 3) 3 2 1)
  ))
