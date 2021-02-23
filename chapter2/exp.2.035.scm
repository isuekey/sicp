
;; count-leaves 定义为一个累积

(define (accumulate op initial items)
  (if (null? items) initial
      (op (car items)
          (accumulate op initial (cdr items)))))

(define (count-leaves  x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (count-leaves atree)
  (accumulate (lambda (ele subs)
                (+ (if (pair? ele) (count-leaves ele)
                       1)
                   subs
                ))
              0
              atree)
  )

(define x (cons (list 1 2) (list 3 4)))
(define xx (list x x))

(count-leaves x)
(count-leaves xx)

