


(defun make-segment (v1 v2)
  (cons v1 v2))

(defun start-segment (segment)
  (car segment))

(defun end-segment (segment)
  (cdr segment))

