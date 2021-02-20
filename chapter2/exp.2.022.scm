
;;

(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items (list)))
(square-list (list 1 2 3 4))

;; 结果是反序的。其实在之前的习题中尝试使用迭代计算的时候
;; 就发现了，使用了结果再次反序的方式解决的。
;;

(define (square-list items)
  (define (iter things answer)
    (if (null? things) answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items (list)))
(square-list (list 1 2 3 4))
;; ((((() . 1) . 4) . 9) . 16)

;; 考虑反序结果

(define (reverse items)
  (define (iter subs answer)
    (if (null? subs) answer
        (iter (cdr subs) (cons (car subs) answer))))
  (iter items (list)))
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (reverse (iter items (list)))
  )
(square-list (list 1 2 3 4))

;; 用了两次迭代计算后变成正序
;; 怀疑跟对称性相关，但是没有证据。
;; 可能是由于cons这种数据结构导致的某种对称性引发的
;; 
