;;斐波那契数 f(n+1) = f(n) + f(n-1), f(0)=0, f(1)=1,
;;a <- a+b, b <- a
;;变换族T_pq是(a,b)成为(bq+aq+ap, bp+aq)的变换,
;;T_pq ^ 2 ((bp+aq)q+(bq+aq+ap)q+(bq+aq+ap)p, (bp+aq)p+(bq+aq+ap)q)
;;(bpq+aqq+bqq+aqq+apq+bpq+apq+app, bpp+apq+bqq+aqq+apq)
;;(b(pq+qq+pq)+a(qq+qq+pq+pq+pp), b(pp+qq)+a(pq+qq+pq))
;;(b(2pq+qq)+a(2pq+qq)+a(qq+pp), b(pp+qq)+a(2pq+qq))
;;T_pq^2 = T_(qq+pp)(2pq+qq)

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p q (- count 1)))))
(define (fib n)
  (fib-iter 1 0 0 1 n))
