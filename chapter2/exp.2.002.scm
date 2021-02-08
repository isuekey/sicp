;; 平面上线段问题

(define (make-segment a b) (cons a b))
(define (start-segment l) (car l))
(define (end-segment l) (cdr l))

(define (print-point p)
  (newline)
  (display "(") (display (x-point p)) (display ",") (display (y-point p)) (display ")"))

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define a (cons 0 0))
(define b (cons 0 5.0))

(print-point a)
(print-point b)

(define seg (make-segment a b))

(define (print-segment l)
  (newline)
  (display "start ") (print-point (start-segment l))
  (display " to end ") (print-point (end-segment l)))

(print-segment seg)

(define (middle-point l)
  (let ((s (start-segment l))
        (e (end-segment l)))
    (cons (/ (+ (car s) (car e)) 2)
          (/ (+ (cdr s) (cdr e)) 2))))
(print-point (middle-point seg))
