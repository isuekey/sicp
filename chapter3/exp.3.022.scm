;; 使用局部状态实现一个queue
#|
  创建队列
  make-queue
  队列的操作方法
  (insert-queue q1 'a)
  (delete-queue q1)
  (body-queue q1)
  (fornt-queue q1)
  (print-queue q1)
|#

(define (make-queue)
  (let ((front-ptr (list ))
        (rear-ptr (list )))
    (define (empty-q?) (null? front-ptr))
    (define (insert-q item)
      (let ((new-pair (cons item '())))
        (cond ((empty-q?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair))))
      dispatch)
    (define (delete-q)
      (cond ((empty-q?)
             (error "delete called with an empty queue" dispatch))
            (else
             (set! front-ptr (cdr front-ptr))
             dispatch)))
    (define (body-q) front-ptr)
    (define (front-q)
      (if (empty-q?)
          (error "front called with an empty queue" dispatch)
          (car front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'insert) insert-q)
            ((eq? m 'delete) delete-q)
            ((eq? m 'body) body-q)
            ((eq? m 'front) front-q)))
    dispatch))
(define (insert-queue! queue item)
  ((queue 'insert) item))
(define (delete-queue! queue)
  ((queue 'delete)))
(define (body-queue queue)
  ((queue 'body)))
(define (front-queue queue)
  ((queue 'front)))
(define (print-queue queue)
  (body-queue queue))

(define q2 (make-queue))
(insert-queue! q2 'a)
(print-queue q2)
(insert-queue! q2 'b)
(print-queue q2)
(delete-queue! q2)
(print-queue q2)
(delete-queue! q2)
(print-queue q2)

#|
有些拼写错误
对照着数据方式处理的
还做不到熟练的定义队列
核心原因是对队列的定义与工作内容不熟悉
见鬼吧。因为我们通常需要使用队列的地方，
一般都提供了完备的操作。我们通常对队列没有啥感知
|#


