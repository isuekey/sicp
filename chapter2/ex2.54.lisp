
;;; (equal? '(this is a list) '(this is a list)); => true
;;; (equal? '(this is a list) '(this is a array)); => false
;;; (equal? '(this is a list) '(this (is a) list)); => false

(defun equal? (x y)
  (cond ((eq x y) t)
        ((eq (car x) (car y)) (equal? (cdr x) (cdr y)))
        ((and (consp (car x)) (consp (car y))) (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))
        (t nil)))

(print (equal? '(this is a list) '(this is a list))); => true
(print (equal? '(this is a list) '(this is a array))); => false
(print (equal? '(this is a list) '(this (is a) list))); => false
(print (equal? '(this (is a) list) '(this (is a) list))); => false
(print (equal? '(this is a list) '(this is a))); => false

