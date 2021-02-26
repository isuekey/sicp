
;; unique-pairs n i,j 1<=j<i<=n

(define (enumeration-interval begin end step)
  (cond ((> begin end) ())
        ((= begin end) (cons end ()))
        (else (cons begin
                    (enumeration-interval (+ begin step) end step)))))

(enumeration-interval 1 10 1)

(define (unique-pairs-map n)
  (map (lambda (i)
         (map (lambda (j)
                (list i j))
              (enumeration-interval 1 (- i 1) 1))
         )
       (enumeration-interval 2 n 1)))
(unique-pairs-map 4)

(define (flatmap proc sequence)
  (accumulate append () (map proc sequence)))
(flatmap (lambda (x) x) (unique-pairs-map 4))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (prime? n)
  (= (smallest-divisor n) n))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divisor? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divisor? a b)
  (= (remainder b a) 0))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs-map n))))



