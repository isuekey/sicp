(define (last-pair x)
  (if (null? (cdr x)) x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle '(a b c)))

(define (hasit pair alist)
  (cond ((null? alist) #f)
        ((not (pair? alist)) #f)
        ((equal? pair (car alist)) #t)
        (else (hasit pair (cdr alist)))))

(define (check-cycle alist)
  (let ((checked (list )))
    (define (iter-check target-list)
      (cond ((null? target-list) #f)
            ((not (pair? target-list)) #f)
            ((hasit target-list checked) #t)
            (else
             (begin (set! checked (cons target-list checked))
                    (or (iter-check (car target-list))
                        (iter-check (cdr target-list)))))))
    (iter-check alist)))
(check-cycle z)
