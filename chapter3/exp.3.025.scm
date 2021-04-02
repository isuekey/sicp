#|
  需要对任意数量的关键码生效（至少一个），
  其实需要修改的是lookup 与 insert!
  如果不要求
|#
;; 扩展表格
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else
             (error "unknown operation -- table" m))))
    (define (assoc key list)
      (if (null? list) #f
          (let ((record (car list)))
            (if (equal? key (car record)) record
                (assoc key (cdr list))))))
    (define (lookup-iter keys record)
      (cond ((null? keys) record)
            ((not record) #f)
            (else
             (lookup-iter (cdr keys) (assoc (car keys) (cdr record))))))
    (define (lookup . keys)
      (look-iter keys local-table))
    #|
    (define (insert-iter! key keys record)
      (let ((fund (assoc key (cdr record))))
        (cond ((null? keys)
        (if (null? fund)
            (let ((new-fund (list key)))
              (insert-iter! (car keys) (cdr keys) new-fund)
              (set-cdr! record (cons new-fund (cdr record))))
            
        (cond ((null? keys) 'ok)
              ((null? (cdr keys)) (set-cdr! fund (car keys)))
    我再想想。现在有些乱。虽然一定是递归的进行，但是
    有些过程细节，需要重新梳理
    |#        
    (define (insert! . keys)
      (if (null? keys) 'ok
          (insert-iter (car keys) (cdr keys) local-table)))
    dispatch))


(define (oo . keys)
  (display keys)
  (newline))
(oo 'a 'b 'c)
