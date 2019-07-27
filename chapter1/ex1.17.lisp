
(defun doub (n) (+ n n))
(defun halv (n) (/ n 2))
(defun even? (n) (= (mod n 2) 0))
(defun mul (n m)
  (mul-it 0 n m))

(defun mul-it (a n m)
  (cond ((= m 0) a)
        ((even? m) (mul-it a (doub n) (halv m)))
        (t (mul-it (+ a n) n (- m 1)))))


(print (mul 1 0))
(print (mul 2 0 ))
(print (mul 1 1))
(print (mul 2 1))
(print (mul 1 2))
(print (mul 2 2))
(print (mul 1 3))
(print (mul 2 3))

(print (mul 30 29))

;;; 不好意思 ex18直接就完事了

  
(defun mul17 (n m)
  (mul-it17 n m))

(defun mul-it17 (n m)
  (cond ((= m 0) 0)
        ((even? m) (doub (mul-it17 n (halv m))))
        (t (+ n (mul-it17 n (- m 1))))))

(print (mul17 1 0))
(print (mul17 2 0 ))
(print (mul17 1 1))
(print (mul17 2 1))
(print (mul17 1 2))
(print (mul17 2 2))
(print (mul17 1 3))
(print (mul17 2 3))
(print (mul17 30 29))

          
