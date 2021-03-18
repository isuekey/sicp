

(define (factorial n)
  (if (= n 1) 1
      (* n (factorial (- n 1)))))
(factorial 6)
#|
E0:
  factorial: (if (= n 1) 1 (* n facotrial (- n 1)))
  (factorial 6)
E1:
  P:E0
  n: 6
  (* n (factorial (- n 1))) 从E0找到factorial
E2:
  P:E1
  n: 5
  (* n (factorial (- n 1))) 从E0找到factorial
E3:
  P:E2
  n: 4
  (* n (factorial (- n 1))) 从E0找到factorial
E4:
  P:E3
  n: 3
  (* n (factorial (- n 1))) 从E0找到factorial
E5:
  P:E4
  n: 2
  (* n (factorial (- n 1))) 从E0找到factorial
E6:
  P:E5
  返回 1 
再依次计算回去 最终得到 720
|#

(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-counter)
  (if (> counter max-counter) product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-counter)))
(factorial 6)
#|
E0:
  factorial: (fact-iter 1 1 n)
  fact-iter: (if (> counter max-counter) product (fact-iter (* counter product) (+ counter 1) max-counter)))
  (factorial 6)
E1:
  P:E0
  product:1
  counter:1
  n:6
  (fact-iter product counter n)从E0找到fact-iter
E2:
  P:E1
  product:1
  counter:2
  n:6
  (fact-iter product counter n)从E0找到fact-iter
E3:
  P:E2
  product:2
  counter:3
  n:6
  (fact-iter product counter n)从E0找到fact-iter
E4:
  P:E3
  product:6
  counter:4
  n:6
  (fact-iter product counter n)从E0找到fact-iter
E5:
  P:E4
  product:24
  counter:5
  n:6
  (fact-iter product counter n)从E0找到fact-iter
E6:
  P:E5
  product:120
  counter:6
  n:6
  (fact-iter product counter n)从E0找到fact-iter
E7:
  P:E6
  product:720
  counter:7
  n:6
  (fact-iter product counter n)从E0找到fact-iter
E8:
  P:E7
  720
再依次计算回去 最终得到 720
但是这是尾递归，E1-E8在执行过程只会保留一份而不是一个栈
据说会在第5.4章解释
|#
