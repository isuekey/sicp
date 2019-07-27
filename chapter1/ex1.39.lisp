

(defun tan-cf (x k)
  (defun lamber-tan (nk)
    (defun lambert-cf-it (count result)
      (if (= count 0)
          result
        (lambert-cf-it (1- count) (/ (funcall nk count) (- (- (* 2 count) 1) result)))))
    (lambert-cf-it k 0))
  (lamber-tan (lambda (i)
                (if (= i 1)
                    x
                  (* x x)))))

(print (tan-cf 0.5 100))
(print (tan-cf 0.0 100))
(print (tan-cf 0.7853981633974483 100))
