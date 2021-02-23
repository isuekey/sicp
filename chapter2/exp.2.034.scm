
;; a_nx^n + a_{n-1}^{n-1} + ... + a_1x + a_0 =>
;; (...(a_nx + a_{n-1})x + ... + a_1)x + a_0
;;预期
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-coeff) <??>)
              0
              coefficient-sequence))
;;计算
;; 1 + 3x + 5x^2 + x^5 在 x=2 处的值

(define (accumulate op initial items)
  (if (null? items) initial
      (op (car items)
          (accumulate op initial (cdr items)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-coeff)
                (+ this-coeff
                   (* x higher-coeff))
                )
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 5 0 0 1))
