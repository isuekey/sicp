
(defun mysqrt (x) (guess-sqrt 1 x))
(defun guess-sqrt (guess target)
  (if (good-enough? guess target)
      guess
    (guess-sqrt (improve guess target) target)))
(defun good-enough? (guess target)
  (< (abs (- target (* guess guess))) 0.1))
(defun improve (guess target)
  (/ (+ guess target) 2))

(mysqrt 4)

