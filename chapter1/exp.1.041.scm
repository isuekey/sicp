
(define (double g)
  (lambda (x) (g (g x))))

(define (inc x) (+ x 1))

(((double (double double)) inc) 5)
;; 预期 13
;; 实际 21
((double inc) 5)
((inc (inc 5)))
;; 5+1*2 = 7
(((double double) inc) 5)
((double (double inc)) 5)
((double inc) ((double inc) 5))
(inc (inc (inc (inc 5))))
;; 5+2*2 = 9
(((double (double double)) inc) 5)
(((double double) ((double double) inc)) 5)
((double (double
          ((double double) inc))) 5)
((double
  (double
   (double
    (double inc))))
 5)
;; 5+2^4 = 21
;; 根据结果倒推很容易，
;; 但是正向判断有些问题。8与16倍很容易猜错。
;; 需要充分了解过程的解析才能得到正确的答案。
;; 




