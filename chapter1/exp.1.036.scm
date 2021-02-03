;; 打印猜测的内容
(define tolerance 0.0001)
(define (fix-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (try first-guess))

(define ff
  (fix-point (lambda (a) (+ 1 (/ 1 a))) 1.0))

;;2.
;;1.5
;;1.6666666666666665
;;1.6
;;1.625
;;1.6153846153846154
;;1.619047619047619
;;1.6176470588235294
;;1.6181818181818182
;;1.6179775280898876
;;1.6180555555555556

;; x^x = 1000求解

(define x
  (let ((t (log 1000)))
    (fix-point (lambda (a) (/ t (log a))) 2)))
