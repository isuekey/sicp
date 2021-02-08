
;; 取出值 7

(define a (list 1 3 (list 5 7) 9))
a
(define b (list (list 7)))
b
(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
c
(cdr (cdr (car (cdr (cdr a)))))
;; 结果错了
(car (cdr (car (cdr (cdr a)))))
;; 最后一步错了

(car (car b))

(car (cdr (cdr (cdr (cdr (cdr (cdr c)))))))
;; 又错了
(list 1 2)
(cdr (list 1 (list 2))) ;; 得到是一个 (cons (list 2) ())的内容
;; 所以
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))

;; 所以提供了cadr这种过程
(cadr (cadr (cadr (cadr (cadr (cadr c))))))





