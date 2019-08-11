

(defun mreverse (list)
  (defun reverse-it (sublist result)
    (if (null sublist)
        result
      (reverse-it (cdr sublist) (cons (car sublist) result))))
  (reverse-it list nil))

(defvar emptylist (list))
(print (mreverse emptylist))

(defvar examplelist (list 23 72 149 34))
(print (mreverse examplelist))

    

