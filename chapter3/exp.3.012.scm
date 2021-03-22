
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x)) x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
;;(a b c d)
(cdr x)
;;(b)
(define w (append! x y))
w
;;(a b c d)
(cdr x)
;;(b c d)

;; 图就不画了，按照环境模型求值
;; 使用了append!之后x的数据变更了。

