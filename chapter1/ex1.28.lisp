
(defun square (m) (* m m))

(defun expmod (base exp m)
  (cond ((= exp 0) 1)
        ((= (mod exp 2) 0) (mr-sq-expmod (expmod base (/ exp 2) m) m))
        (t (mod (* base (expmod base (- exp 1) m)) m))))

(defun mr-sq-expmod (root m)
  (defun mr-expmod (sqr root m1)
    (if (= sqr 1)
        (if (or (= root 1) (= root m1)) 1 0)
      sqr))
  (mr-expmod (mod (square root) m) root (- m 1)))

(defun miller-rabin-test (n)
  (defun try-it (a)
    (> (expmod a (- n 1) n) 0))
  (try-it (+ 1 (random (- n 1)))))

(defun fast-prime? (n times)
  (cond ((or (= times 0) (< times 0)) t)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (t nil)))

(print (fast-prime? 10007 30))
(print (fast-prime? 561 30))
(print (fast-prime? 10009 30))
(print (fast-prime? 10008 30))
(print (fast-prime? 10037 30))
(print (fast-prime? 6601 30))

;;; 可能有地方有问题，不是很准确。
;;; miller rabin 检测的理解不准确。

