
(define (last-pair x)
  (if (null? (cdr x)) x
      (last-piar (cdr x))))

(define x '(a b))
(define z (cons x x))
x
z
;; 虽然数据结构符合预测，但是展示的结果有些意外，
;; 对list和cons的理解还是不够深刻。

(define (cons x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))


(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)
(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value) ((z 'set-x!) new-value) z)
(define (set-cdr! z new-value) ((z 'set-y!) new-value) z)




