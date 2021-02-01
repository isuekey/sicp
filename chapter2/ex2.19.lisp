

(defvar us-coins (list 50 10 25 5 1))
(defvar uk-coins (list 100 50 20 10 5 2 1 0.5))

(defun cc (amount coin-values)
  (defun nomore? (values) (null values))
  (defun except-first-values (values) (cdr values))
  (defun first-value (values) (car values))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (nomore? coin-values)) 0)
        (t (+
            (cc amount (except-first-values coin-values))
            (cc (- amount (first-value coin-values)) coin-values)))))

(print (cc 100 us-coins))
