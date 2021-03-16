(define rand
  (let ((x 0))
    (define (reset n)
      (set! x n) x)
    (lambda (action)
      (cond ((eq? action 'reset) reset)
            ((eq? action 'generate)
             (begin (set! x (random-updatex)) x))))))
