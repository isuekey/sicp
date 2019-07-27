
;;; recursive cal

(defun cont-frac (n d k)
  (defun cont-frac-recur (count)
    (if (> count k)
        0
      (/ (funcall n count) (+ (funcall d count) (cont-frac-recur (1+ count))))))
  (cont-frac-recur 1))

(defun fans (k)
  (cont-frac (lambda (i) 1)
             (lambda (i) 1)
             k))

(print (fans 12))

(defun cont-frac-it (n d k)
  (defun cont-it (count result)
    (if (= count 0)
        result
      (cont-it (1- count) (/ (funcall n count) (+ (funcall d count) result)))))
  (cont-it k 0))


(defun fans-it (k)
  (cont-frac-it (lambda (i) 1.0)
                (lambda (i) 1)
                k))

(print (fans-it 12))

(defun e-2 (k)
  (cont-frac-it (lambda (i) 1.0)
                (lambda (i) (if (= (mod i 3) 2)
                                (/ (* (1+ i) 2) 3)
                              1))
                k))

(print (+ 2(e-2 100)))


