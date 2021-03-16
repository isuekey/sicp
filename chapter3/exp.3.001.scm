
(define (make-accumulator start)
  (let ((sum start))
    (lambda (adder)
      (begin (set! sum (+ sum adder))
             sum))))

(define A (make-accumulator 5))
(A 10)
(define B (make-accumulator 2))
(B 10)
(A 10)
