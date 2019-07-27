
(defun smallest-divisor (n)
  (smallest-divisor-it n 2))
(defun smallest-divisor-it (n div)
  (cond ((> (* div div) n) n)
        ((= (mod n div) 0) div)
        (t (smallest-divisor-it n (+ div 1)))))

(print (smallest-divisor 199))
(print (smallest-divisor 1999))
(print (smallest-divisor 19999))
(print (smallest-divisor 20))
