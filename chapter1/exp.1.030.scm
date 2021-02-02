;;文中出现的线性递归计算过程的过程
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

;; 需要一个迭代计算过程的过程

(define (sum-lp term a next b)
  (define (sum-lp-iter k b result)
    (if (> k b)
        result
        (sum-lp-iter (next k) b (+ result (term k)))))
  (sum-lp-iter a b 0))

(define (cube a) (* a a a))
(define (inc a) (+ a 1))
(sum-lp cube 1 inc 10)
(sum cube 1 inc 10)

;;根据题中提示
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;;自行研究的代码中 sum-lp-iter 的参数 b 其实没啥用

