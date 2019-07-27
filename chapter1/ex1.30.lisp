
(defun sum (term a next b)
  (defun iter (a result)
    (if (> a b)
        result
      (iter (funcall next a) (+ result (funcall term a)))))
  (iter a 0))

(defun sq (x) (* x x))
(defun nt (x) (+ x 1) )

(print (sum 'sq 1 'nt 100))

  
