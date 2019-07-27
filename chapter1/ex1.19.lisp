;;; a <- a + b; b <- a
;;; T_{p,q}(a, b) = (ap + aq + bq, bp + aq)
;;; T_{p,q}^2(a, b) = T_{p,q}(T_{p,q}(a, b)) = T_{p,q}(ap + aq + bq, bp + aq)
;;;   = (a(p^2+ q^2) + (2pq + q^2)(a + b), b(p^2 + q^2)+ a(2pq + q^2))
;;;   = T_{p^2 + q^2, 2pq + q^2}(a, b)
;;; p' = p^2 + q^2, q' = 2pq + q^2
;;; 对于斐波那契数列来讲 p = 0, q = 1


(defun fib (n)
  (fib-it 1 0 0 1 n))
(defun fib-it (a b p q count)
  (defun even? (m)
    (= (mod m 2) 0))
  (cond ((= count 0) b)
        ((even? count) (fib-it a b
                               (+ (* p p) (* q q))
                               (+ (* 2 p q) (* q q))
                               (/ count 2)))
        (t (fib-it
            (+ (* a p) (* b q) (* a q))
            (+ (* b p) (* a q))
            p q (- count 1)))))

(print (fib 11))
(print (fib 12))
(print (fib 13))
(print (fib 14))
(print (fib 15))
(print (fib 16))

