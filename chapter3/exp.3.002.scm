
(define (make-monitored afunction)
  (let ((called 0))
    (lambda (a . args)
      (cond ((eq? a 'how-many-calls) called)
            ((eq? a 'reset-count) (begin (set! called 0) called))
            (else (begin (set! called (+ called 1))
                         (apply afunction (cons a args))))))))
(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls)
(s 'reset-count)
(s 'how-many-calls)
(s 100)
(s 'how-many-calls)


