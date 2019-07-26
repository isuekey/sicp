(defun f (n)
  (if (< n 3)
      n
    (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(print (f 3))
(print (f 4))
;(print (f 5))
;(print (f 20))

(defun ff (n)
  (if (< n 3)
      n
    (ff-it 2 1 0 n)))
(defun ff-it (a b c count)
  (if (= count 2)
      a
    (ff-it (+ a b b c c c) a b (- count 1))))

(print (ff 2))
(print (ff 3))
(print (ff 4))
(print (ff 20))
