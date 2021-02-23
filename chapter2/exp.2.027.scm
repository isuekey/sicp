
;; 修改reverse过程，得到一个deep-reverse过程
;; 一个表为参数，返回一个表，同时让子树的内容也翻转
(define x (list (list 1 2) (list 3 4)))
x
;; (reverse x) => ((3 4) (1 2))
;; (deep-reverse x) => ((4 3) (2 1))

(define (reverse items)
  (define (iter subs answer)
    (if (null? subs) answer
        (iter (cdr subs) (cons (car subs) answer))))
  (iter items ()))
(define (deep-reverse items)
  (define (iter subs answer)
    (cond ((null? subs) answer)
          ((pair? (car subs))
           (iter (cdr subs) (cons (iter (car subs) ()) answer)))
          (else
           (iter (cdr subs) (cons (car subs) answer)))))
  (iter items ()))

(reverse x)
(deep-reverse x)

