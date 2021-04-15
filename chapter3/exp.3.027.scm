
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

#|
  递归进行 memo-fib 时，会从小到大逐渐加入到memory中
  导致后面的计算深度不会超过2
  然后整体的计算量是O(n)
|#

(define memo-fib2 (memoize fib))
#|
  递归调用时，没有使用memory内容
  所以不生效
|#
