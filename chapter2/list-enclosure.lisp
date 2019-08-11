

(defun mappend (list1 list2)
  (if (null list1)
      list2
    (cons (car list1) (mappend (cdr list1) list2))))

(defvar ll1 (list 1 2 3))
(defvar ll2 (list 7 8 9))

(print ll1)
(print ll2)
(print (mappend ll1 ll2))



