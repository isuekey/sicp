

(define (equ? a b)
  (apply-generic 'equ? a b))

;; install-scheme-number-package
(put 'equ? '(scheme-number scheme-number) =)

;; install-rational-package
(put 'equ? '(rational rational)
     (lambda (r1 r2)
       (and (= (numer r1) (numer r2))
            (= (denom r2) (denom r2)))))
;; install-complex-package
(put 'equ? '(complex complex)
     (lambda (z1 z2)
       (and (= (real-part r1) (real-part r2))
            (= (imag-part r2) (imag-part r2)))))

;; 其实不严谨，但是无所谓了。如果是不定长的参数
;; 需要使用apply过程
(define (equ? a b . c)
  (apply apply-generic (append (list 'equ? a b) c)))

;; 这里主要考察的是不同包之间的协作与调用，
;; 而不是具体实现的
