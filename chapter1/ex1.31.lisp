
(defun product-loop (a b term next)
  (defun loop-it (a result)
    (if (> a b)
        result
      (loop-it (funcall next a) (* (funcall term a) result))))
  (loop-it a 1))

(defun item (n)
  (if (= (mod n 2) 0)
      (/ (+ n 2) (+ n 1))
    (/ (+ n 1) (+ n 2))))

;(print (* 4.0 (product-loop 1 2000 'item '1+)))


(defun product-iter (a b term next)
  (if (> a b)
      1
    (* (funcall term a)
       (product-iter (funcall next a) b term next))))
(print (* 4.0 (product-iter 1 2000 'item '1+)))
