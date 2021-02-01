;; gcd define 
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)

2

;; 特殊形式 都是先求值谓词部分，再继续处理
(gcd 206 40)
;; 正则序替换
(gcd 40 (remainder 206 40)) = (gcd 40 6)
(if (= (remainder 206 40) 0)
    (remainder 206 40)
    (gcd (remainder 206 40) (remainder 40 (remainder 206 20))) = (gcd 6 4)
    )
(if (= (remainder 40 (remainder 206 20)) 0)
    (remainder 40 (remainder 206 20))
    (gcd (remainder 40 (remainder 206 20)) (remainder (remainder 206 40) (remainder 40 (remainder 206 20)))) = (gcd 4 2)
    )
(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 20))) 0)
    (remainder (remainder 206 40) (remainder 40 (remainder 206 20)))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 20))) (remainder (remainder 40 (remainder 206 20)) (remainder (remainder 206 40) (remainder 40 (remainder 206 20))))) = (gcd 2 0)
    )
(if (= (remainder (remainder 40 (remainder 206 20)) (remainder (remainder 206 40) (remainder 40 (remainder 206 20)))) 0)
    (remainder (remainder 40 (remainder 206 20)) (remainder (remainder 206 40) (remainder 40 (remainder 206 20))))
    0
    )
;;; 正则序中，执行remainder是在谓词部分和最后的结果部分。
(+ 1 2 4 7 7)
22 ;;; 22次 remainder

;; 应用序
(if (= 40 0)
    40
    (gcd 40 (remainder 206 40)) = (gcd 40 6)
    )
(if (= 6 0)
    6
    (gcd 6 (remainder 40 6)) = (gcd 6 4)
    )
(if (= 4 0)
    4
    (gcd 4 (remainder 6 4)) = (gcd 4 2)
    )
(if (= 2 0)
    2
    (gcd 2 (remainder 4 2)) = (gcd 2 0)
    )
(if (= 0 0)
    2
    (gcd 0 (remainder 2 0))
    )
;;; 应用序中, 执行remainder在递归上。这是一个迭代计算过程。共计4次
