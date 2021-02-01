

(defvar x (cons (list 1 2) (list 3 4)))


(defun count-leaves (x)
  (cond ((null x) 0)
        ((not (consp x)) 1)
        (t (+ (count-leaves (car x))
              (count-leaves (cdr x))))))

(print (length x))
(print (count-leaves x))
(print (count-leaves (list x x)))
(print (length (list x x)))
