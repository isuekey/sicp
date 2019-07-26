
(defun msqrt (x)
  (defun square (guess) (* guess guess))
  (defun good-enough? (guess)
    (< (abs (- (square guess) x)) 0.001))
  (defun improve (guess)
    (/ (+ guess (/ x guess)) 2.0))
  (defun sqrtit (guess)
    (if (good-enough? guess)
        guess
      (sqrtit (improve guess))))
  (sqrtit 1.0))

(print (msqrt 4))


    
