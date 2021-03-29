
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
;; 这里发现每个节点，不仅要知道后续节点，还要知道前置节点
;; 所以假定每个节点的结构为 ((item-val . pre-item-ptr) next-item-ptr)
;; 从这里看到，只需要调整item的构造即可
;; 

(define (make-node item next pre)
  (cons (cons item pre) next))
(define (next-node node)
  (cdr node))
(define (pre-node node)
  (cdar node))
(define (node-item node)
  (caar node))

(define (make-deque) (cons '() '()))
(define (empty-deque? deq) (null? (caar deq)))
(define (front-deque deq)
  (if (empty-deque? deq)
      (error "front deque with empty deque" deq)
      (node-item (car deq))))
(define (rear-deque deq)
  (if (empty-deque? deq)
      (error "rear deque with empty deque" deq)
      (node-item (cdr deq))))

(define (front-insert-deque! deq item)
  (let ((front-node (make-node item (car deq) '())))
    (cond ((empty-deque? deq)
           (set-car! deq front-node)
           (set-cdr! deq front-node)
           deq)
          (else
           (set-car! deq front-node)
           deq))))
(define (rear-insert-deque! deq item)
  (let ((rear-node (make-node item '() (cdr deq))))
    (cond ((empty-deque? deq)
           (set-car! deq rear-node)
           (set-cdr! deq rear-node)
           deq)
          (else
           (set-cdr! (cdr deq) rear-node)
           (set-cdr! deq rear-node)
           deq))))
(define (front-delete-deque! deq)
  (cond ((empty-deque? deq) deq)
        (else
         (let ((front-node (car deq)))
           (set-car! deq (next-node front-node))
           deq))))
(define (rear-delete-deque! deq)
  (cond ((empty-deque? deq) deq)
        (else
         (let ((rear-node (cdr deq)))
           (set-cdr! (pre-node rear-node) '())
           (set-cdr! deq (pre-node rear-node))
           deq))))

