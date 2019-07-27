(defun smallest-divisor (n)
  (smallest-divisor-it n 2))
(defun smallest-divisor-it (n div)
  (cond ((> (* div div) n) n)
        ((= (mod n div) 0) div)
        (t (smallest-divisor-it n (+ div 1)))))
(defun prime? (n)
  (= n (smallest-divisor n)))

(defun timed-prime-test (n)
  (princ #\Newline)
  (print n)
  (start-prime-test n (get-internal-real-time)))

(defun start-prime-test (n start-time)
  (if (prime? n)
      (report-prime (- (get-internal-real-time) start-time))))
(defun report-prime (elapsed-time)
  (print " **** ")
  (print elapsed-time))

(defun search-for-prime (n count)
  (cond ((> count 3))
        ((prime? n) (timed-prime-test n) (search-for-prime (+ n 1) (+ count 1)))
        (t (search-for-prime (+ n 1) count))))

(search-for-prime 10000 1)
(search-for-prime 100000 1)
(search-for-prime 1000000 1)
(search-for-prime 10000000 1)
(print (sqrt 10))

    
