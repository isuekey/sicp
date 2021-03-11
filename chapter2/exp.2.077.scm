
;;
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "unkown type" z))))
(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "unkown type" z))))
(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "unkown type" z))))
(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "unkown type" z))))

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-real-imag (* (magnitude z1) (magnitude z2))
                         (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-real-imag (/ (magnitude z1) (magnitude z2))
                         (- (angle z1) (angle z2))))

  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))

  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  
  'done)

(install-complex-package)
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(magnitude (make-from-real-imag 3 4))
(define (magnitude z)
  (apply-generic 'magnitude z))
;; 根据所学内容，巩固一下
(define (apply-generic op . args)
  (let ((arg-types (map get-tag args)))
    (let ((proc (get op arg-types)))
      (if proc
          (apply proc (map contents args))
          (error "unknown operate on types", op, arg-types)))))
(define (get-tag tagged-datum) (car tagged-datum))
(define (contents tagged-datum) (cadr tagged-datum))

#|
根据题目内容
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
在使用magnitude的时候实际使用
((get 'magnitude '(complex)) (contents z))
(magnitude z-contents)
(contents z) 返回的是带有rectangular或者polar标志的数据
((get 'magnitude '(poloar)) (contents z-contents)) ;; 或者
((get 'magnitude '(reactangular)) (contents z-contents))

|#
