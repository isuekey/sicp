(defun square (m) (* m m))
(defun expmod (base exp m)
  (cond ((= exp 0) 1)
        ((= (mod exp 2) 0) (mod (square (expmod base (/ exp 2) m)) m))
        (t (mod (* (mod base m) (expmod base (- exp 1) m)) m))))
(defun fermat-test (n)
  (defun try-it (a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(defun fast-prime? (n times)
  (cond ((= times 0) t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (t nil)))



(defun timed-prime-test (n)
  (princ #\Newline)
  (princ n)
  (start-prime-test n (get-internal-real-time)))

(defun start-prime-test (n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (get-internal-real-time) start-time))))
(defun report-prime (elapsed-time)
  (princ " **** ")
  (princ elapsed-time))

(defun search-for-prime (n count)
  (cond ((> count 3))
        ((fast-prime? n 10) (timed-prime-test n) (search-for-prime (+ n 1) (+ count 1)))
        (t (search-for-prime (+ n 1) count))))

(search-for-prime 10000 1)
(search-for-prime 100000 1)
(search-for-prime 1000000 1)
(search-for-prime 10000000 1)
(print (sqrt 10))

    
