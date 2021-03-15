;; 需要完善

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (/ (* (numer x) (numer y))
                 (* (denom x) (denom y)))))
  (define (div-rat x y)
    (make-rat (/ (* (numer x) (denom y))
                 (* (denom x) (numer y)))))
  (define (equal-rat x y)
    (= (* (numer x) (denom y))
       (* (denom x) (numer y))))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'raise 'rational
       (lambda (ration raiseup) (raiseup ration)))
  
  'done)
(define (make-rational-number n d)
  ((get 'make 'rational) n d))
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
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  
  'done)
(define (type-tags arg) (car arg))
(define (attach-tag symbol contents) (list symbol contents))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "no method for these type -- APPLY-GENERIC" (list op type-tags))))))

(define (raise-rational ration raiseup)
  ((get 'raise 'rational) ration
   (lambda (x) ((get 'make-from-mag-ang 'complex)
                (/ (apply-generic 'numer ration)
                   (apply-generic 'denom ration))
                0))
   ))

#|

如何检查哪个类型更高
对于各自的包而言，可以知道自己的数据类型哪些特性其他包可以用，
但是不应该知道，被其他包如何使用自己的特性。
因此类型升级应该是包的组织者（也就是通用包处理的内容）

|#

(put 'raise 'rational
     (lambda (ration)
       ((get 'make-from-mag-ang 'complex)
        (/ (apply-generic 'numer ration)
           (apply-generic 'denom ration))
        0)))


(put 'raise 'scheme-number
     (lambda (x)
       ((get 'make-rational-number) x 1)))

(define numberLevle '((scheme-number 1) (rationl 2) (complex 3)))
(define (getLevel tag)
  (let ((level (filter (lambda (ele) (eq? tag (car ele))) numberLevel)))
    (if (null? level) 3
        (cadr (car level)))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          ;; 这里就不检查了 在 exp.2.082 提过应该是累积的方式处理。
          (let ((level1 (getLevel (car type-tags)))
                (level2 (getLevel (cadr type-tags)))
                (a1 (car args))
                (a2 (cadr args)))
            (cond ((< level1 level2)
                   (apply-generic op ((get 'raise (car type-tags)) a1) a2))
                  ((> level1 leve2)
                   (apply-generic op a1 ((get 'raise (cadr type-tags)) a2)))
                  (else
                   (error "不知道"))))))))
