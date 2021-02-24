
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



