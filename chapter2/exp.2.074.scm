
;; a)
(define (get-record orgKey employeeKey)
  (let (proc (get 'get-record orgKey))
    (if proc (proc employeeKey)
        (error "unknown action" 'get-record orgKey))))

(define (install-org-orgKey-package)
  (define (get-record employeeKey)
    'employeeInfo)
  (put 'get-record orgKey get-record)

  'done)
;; b)
(define (get-salary orgKey employeeKey)
  (let (proc (get 'get-salary orgKey))
    (if proc (proc employeeKey)
        (error "unknown action" 'get-record orgKey))))


(define (install-org-orgKey-package)
  (define (get-record employeeKey)
    '(employeeInfo salary) )
  (define (get-salary employeeKey)
    (cadr (get-record employeeKey)))

  (put 'get-record orgKey get-record)
  (put 'get-salary orgKey get-salary)


  'done)
;; c) 在所有公司中找特定雇员名
(define (find-employee-record employeeKey orgList)
  (filter (lambda (record) record)
          (map (lambda (orgKey)
                 (get-record orgKey employeeKey))
               orgList)))
;; d) 购并新公司后
#|
需要增加对应的install-package 过程
|#

