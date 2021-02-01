
(defvar x (list (list 1 2) (list 3 4)))
(print x)

(print (reverse x))

(defun mreverse (l)
  (defun reverse-it (subl result)
    (if (null subl)
        result
      (reverse-it (cdr subl) (cons (car subl) result))))
  (reverse-it l nil))
(print (mreverse x))

(defun deep-reverse (l)
  (defun reverse-it (subl result)
    (if (null subl)
        result
      (let ((subele (car subl))
            (sublist (cdr subl)))
        (if (consp subele)
            (reverse-it sublist (cons (deep-reverse subele) result))
          (reverse-it sublist (cons subele result))))))
  (reverse-it l nil))
(print (deep-reverse x))
