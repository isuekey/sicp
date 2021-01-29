;; recursive pascal-triangle
(define (p a b)
  (cond
   ((or (= b 1) (= a b)) 1)
   (else (+ (p (- a 1) (- b 1)) (p (- a 1) b)))
   ))
