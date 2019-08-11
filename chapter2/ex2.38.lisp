
(defun fold-right (op initial sequence)
  (if (null sequence)
      initial
    (funcall op (car sequence)
             (fold-right op initial (cdr sequence)))))

(defun fold-left (op initial sequence)
  (defun iter (result rest)
    (if (null rest)
        result
      (iter (funcall op result (car rest))
            (cdr rest))))
  (iter initial sequence))

;;;猜测
;;; (fold-right '/ 1 (list 1 2 3))  1/6
;;; (fold-left '/ 1 (list 1 2 3)) 1/6
;;; (fold-right 'list nil (list 1 2 3)) (nil 1 2 3)
;;; (fold-left 'list nil (list 1 2 3)) (1 2 3 nil)

(print (fold-right '/ 1 (list 1 2 3)))
(print (fold-left '/ 1 (list 1 2 3)))
(print (fold-right 'list nil (list 1 2 3)))
(print (fold-left 'list nil (list 1 2 3)))

;;;实际
;;;3/2
;;;1/6
;;;(1 (2 (3 nil)))
;;;(((nil 1) 2) 3)

(print (fold-right '+ 1 (list 1 2 3)))
(print (fold-left '+ 1 (list 1 2 3)))
(print (fold-right '* 1 (list 1 2 3)))
(print (fold-left '* 1 (list 1 2 3)))

;;; 猜测满足交换律
