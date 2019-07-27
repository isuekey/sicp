
;;; iterator calculate
(defun accumulate (combiner nullvalue term a next b)
  (if (> a b)
      nullvalue
    (funcall combiner
             (funcall term a)
             (accumulate combiner nullvalue term (funcall next a) next b))))
(defun id (x) x)

;;; (print (accumulate '+ 0 'id 1 '1+ 100))

(defun item (n)
  (if (= (mod n 2) 0)
      (/ (+ n 2.0) (+ n 1))
    (/ (+ n 1.0) (+ n 2))))

;;; (print (* 4 (accumulate '* 1.0 'item 1 '1+ 1000)))

(defun accumulate-loop (combiner nullvalue term a next b)
  (defun loop-it (a result)
    (if (> a b)
        result
      (loop-it (funcall next a)
               (funcall combiner (funcall term a) result))))
  (loop-it a nullvalue))

(print (accumulate-loop '+ 0 'id 1 '1+ 1000))
(print (* 4 (accumulate-loop '* 1.0 'item 1 '1+ 1000)))
