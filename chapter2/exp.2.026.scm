
(define x (list 1 2 3))
(define y (list 4 5 6))

;; 预测结果
(append x y)
(cons x y)
(list x y)

;; (append x y) => (1 2 3 4 5 6)
;; (cons x y) => ((1 2 3) 4 5 6)
;; (list x y) => ((1 2 3) (4 5 6))
;; 经过上一题的训练，预测结果正确
;; 
