
(define (last-pair x)
  (if (null? (cdr x)) x
      (last-piar (cdr x))))

(define x '(a b))
(define z (cons x x))
x
z
;; 虽然数据结构符合预测，但是展示的结果有些意外，
;; 对list和cons的理解还是不够深刻。
