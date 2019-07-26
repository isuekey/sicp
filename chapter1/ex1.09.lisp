(defun m+ (a b)
  (if (= a 0)
      b
    (1+ (m+ (1- a) b))))

(print (m+ 3 5))

(defun i+ (a b)
  (if (= a 0)
      b
    (i+ (1- a) (1+ b))))
(print (i+ 4 9))

      
