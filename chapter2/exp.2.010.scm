
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                    (max p1 p2 p3 p4))))
(define (div-interval x y)
  (if (and (<= 0 (lower-bound y))
           (>= 0 (upper-bound y)))
      (error "区间越界了")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))
(define upper-bound cdr)
(define lower-bound car)

(define (sub-interval x y)
  (add-interval x
                (make-interval (- 0.0 (upper-bound y))
                               (- 0.0 (lower-bound y)))))
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

;; 区间 (2,3), (5,7)
(define interval-a (make-interval 2 3))
(define interval-b (make-interval 5 7))
(display (width interval-a))
(display (width interval-b))

