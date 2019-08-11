
(defvar ex1 (list 1 3 (list 5 7) 9))
(defvar ex2 (list (list 7)))
(defvar ex3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(print ex1)
(print (car (cdr (car (cdr (cdr ex1))))))
(print ex2)
(print (car (car ex2)))
(print ex3)
(print (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr ex3)))))))))))))

(defvar ex4 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 nil))))))))
(print ex4)

