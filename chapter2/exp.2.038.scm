
;; accumulate 是  fold-right
;;
(define (accumulate op initial items)
  (if (null? items) initial
      (op (car items)
          (accumulate op initial (cdr items)))))

(define (fold-right op initial items)
  (accumulate op initial items))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list () (list 1 2 3))
(fold-left list () (list 1 2 3))

;; op 如果同时对fold-right与fold-left生效且结果一致，
;; 个人认为 op 需要满足交换律
;; (op a b) 与 (op b a)是一致的

