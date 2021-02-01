
;;; (last-pair (list 1 2 3 34)) => (34)

(defun last-pair (list)
  (defun last-pair-it (sublist result)
    (if (null sublist)
        result
      (last-pair-it (cdr sublist) (car sublist))))
  (last-pair-it list nil))

(defvar emptylist (list))
(print (last-pair emptylist))

(defvar examplelist (list 23 72 149 34))
(print (last-pair examplelist))

    

