
#|
 deque 双端队列
 deque (front . rear)
 front: (list item...)
 rear: last-item
|#

(define (make-deque) (cons '() '()))
(define (empty-deque? deq) (null? (caar deq)))
(define (front-deque deq)
  (if (empty-deque? deq)
      (error "front deque with empty deque" deq)
      (caar deq)))
(define (rear-deque deq)
  (if (empty-deque? deq)
      (error "rear deque with empty deque" deq)
      (cadr deq)))
(define (front-insert-deque! deq item)
  (let ((front-item (cons item (car deq))))
    (cond ((empty-deque? deq)
           (set-car! deq front-item)
           (set-cdr! deq front-item)
           deq)
          (else
           (set-car! deq front-item)
           deq))))
(define (rear-insert-deque! deq item)
  (let ((rear-item (cons item '())))
    (cond ((empty-deque? deq)
           (set-car! deq rear-item)
           (set-cdr! deq rear-item)
           deq)
          (else
           (set-cdr! (cdr deq) rear-item)
           (set-cdr! deq rear-item)
           deq))))
(define (front-delete-deque! deq)
  (cond ((empty-deque? deq) deq)
        (else
         (let ((new-deq (cdar deq)))
           (set-car! deq new-deq)
           deq))))
(define (rear-delete-deque! deq))
;; 处理不了了
      
