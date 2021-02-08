
(list 1 (list 2 (list 3 4)))
;; 根据形式应该是这样
(1 (2 (3 4)))
;; 转换成cons表示
(list 1 2)
;; 等价于
(cons 1 (cons 2 ()))
(list 1 (list 2 (list 3 4)))
;; 等价于
(list 1
      (list 2
            (list 3
                  4)
            )
      )
(list 1
      (list 2
            (cons 3 (cons
                     4 ()))
            )
      )
(list 1
      (cons 2 (cons
               (cons 3 (cons
                        4 ()))
               ()))
      )
(cons 1 (cons
         (cons 2 (cons
                  (cons 3 (cons
                           4 ()))
                  ()))
         ()))
;; 展开过程主要是强化一下如何用cons表示list
;; 如果过程理解的模糊，用cons思考list的时候很容易认为如下结果
;; (1 2 3 4)




