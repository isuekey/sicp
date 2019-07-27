;;;正整数的幂运算

(defun fast-expi (b n)
  (exp-it 1 b n))
(defun even? (m)
  (= (mod m 2) 0))
(defun exp-it (a b n)
  (cond ((= n 0) a)
        ((even? n) (exp-it a (* b b) (/ n 2)))
        (t (exp-it (* a b) b (- n 1)))))

(print (fast-expi 2 1))
(print (fast-expi 2 2))
(print (fast-expi 2 3))
(print (fast-expi 2 4))
(print (fast-expi 2 5))
(print (fast-expi 2 10))
(print (fast-expi 2 15))
(print (fast-expi 2 30))
(print (fast-expi 2 45))
(print (fast-expi 2 60))

