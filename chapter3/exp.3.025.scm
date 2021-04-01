;; 扩展表格
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else
             (error "unknown operation -- table" m))))
    dispatch))
#|
  需要对任意数量的关键码生效（至少一个），
  其实需要修改的是lookup 与 insert!
  如果不要求
|#
