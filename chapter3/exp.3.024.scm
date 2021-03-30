
(define (make-table)
  (let ((local-table (list '*table*)))
    (define dirt 0.0000001)
    (define (same-key? key1 key2)
      (cond ((equal? key1 key2) #t)
            ((or (not (number? key1)) (not (number? key2))) #f)
            (else
             (< (abs (- key1 key2)) dirt))))
    (define (assoc key list)
      (cond ((null? list) #f)
            ((same-key? key (caar list)) (car list))
            (else (assoc key (cdr list)))))
    (define (lookup key1 key2)
      (let ((subtable (assoc key1 (cdr local-table))))
        (if (not subtable) #f
            (let ((record (assoc key2 (cdr subtable))))
              (if record (cdr record)
                  #f)))))
    (define (insert! key1 key2 value)
      (let ((subtable (assoc key1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key2 value) (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key1 (cons key2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else
             (error "unknown operation -- table" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc))

            
      
             
