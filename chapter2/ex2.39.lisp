

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


(defun mreverse1 (sequence)
  (fold-right (lambda (x y)
                (append y (list x))
                ) nil sequence))

(defun mreverse2 (sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))



(print (mreverse1 (list 1 2 3)))
(print (mreverse2 (list 1 2 3)))
