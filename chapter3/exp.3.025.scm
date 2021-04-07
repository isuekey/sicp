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
      (cdr (lookup-iter keys local-table)))
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
    (define (insert-iter key1 key2 keys node)
      (let ((record (assoc key1 (cdr node))))
        (cond ((not record)
               (let ((new-node (cons key1 key2)))
                 (set-cdr! node (cons new-node (cdr node)))
                 (cond ((null? keys) 'ok)
                       (else
                        (set-cdr! new-node '())
                        (insert-iter key2 (car keys) (cdr keys) new-node)))))
              (else
               (cond ((null? keys)
                      (set-cdr! record key2))
                     (else
                      (insert-iter key2 (car keys) (cdr keys) record)))))))
    (define (insert! key1 key2 . keys)
      (insert-iter key1 key2 keys local-table))
    dispatch))

(define mm (make-table))
(define insert-mm (mm 'insert-proc))
(insert-mm 'a 'b 'c 'd)
(insert-mm 'a 'b 'd 'e)
(define lookup-mm (mm 'lookup-proc))
(lookup-mm 'a)
(lookup-mm 'a 'b)
(lookup-mm 'a 'b 'c)
(lookup-mm 'a 'b 'd)



