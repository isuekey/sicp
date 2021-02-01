

(defun memq (item x)
  (cond ((null x) nil)
        ((eq item (car x)) x)
        (t (memq item (cdr x)))))


(print (list 'a 'b 'c)) ; (a b c)
(print (list 'george)) ; (george)
(print (cdr '((x1 x2) (y1 y2)))) ;((y1 y2))
(print (cadr '((x1 x2) (y1 y2)))) ;(y1 y2)
(print (consp (car '(a short list)))) ;nil
(print (memq 'red '((red shoes) (blue socks))))
(print (memq 'red '(red shoes blue socks)))

