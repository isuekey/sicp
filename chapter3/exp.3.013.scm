
(define (last-pair x)
  (if (null? (cdr x)) x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle '(a b c)))

;; (last-pair z)
;; 和预料的一样，死循环了。
