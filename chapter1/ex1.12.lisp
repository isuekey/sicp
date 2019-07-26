
(defun pptr (n r)
  (if (or (= n 1) (= r 1) (= n r))
      1
    (+ (pptr (- n 1) r) (pptr (- n 1) (- r 1)))))
;;;(print (pptr 10 4))

(defun pptrow (cn r)
  (prin1 (pptr cn r) )
  (princ #\Space  )
  (if (or (< r cn))
      (pptrow cn (+ r 1))
    ()))
(defun pptit (b n)
  (if (or (< b n) (= b n))
      (pptrow b 1)
    ())
  (princ #\Newline)
  (if (or (< b n) (= b n))
      (pptit (+ b 1) n)
    ()))

(pptit 1 9)


    
        
