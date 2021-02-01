

;;; a_nx^n + a_n-1x^n-1 + ... + a_1x + a_0
;;; horner rules
;;;(...(a_nx + a_n-1)x + .... + a_1)x + a_0

(defun accumulate (op initial sequence)
  (if (null sequence)
      initial
    (funcall op (car sequence)
             (accumulate op initial (cdr sequence)))))

(defun filter (predicate sequence)
  (cond ((null sequence) nil)
        ((funcall predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (t (filter predicate (cdr sequence)))))


(defun horner-eval (x coe-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff
                                                   (* x higher-terms)))
              0
              coe-sequence))

;;; 1 + 3x + 5x^3 + x^5, x=2
;;; 1 + 6 + 40 + 32 = 79

(print (horner-eval 2 (list 1 3 0 5 0 1)))
