
;; 规范化有理数

(define (make-rat n d)
  (let ((ds (/ d (abs d))))
    (let ((g (gcd n d)))
      (cons (/ (* n ds) g) (/ (* d ds) g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x)) (display "/") (display (denom x))
  0)

(print-rat (make-rat -5 10))
(print-rat (make-rat 5 -10))
(print-rat (make-rat -5 -10))
(print-rat (make-rat 5 10))
