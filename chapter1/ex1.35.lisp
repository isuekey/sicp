


(defvar tolerance 0.000001)
(defun fixed-point (f fguess)
  (defun close? (v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (defun try (guess)
    (let ((next (funcall f guess)))
      (if (close? guess next)
          next
        (try next))))
  (try fguess))

;;; x-> 1 + 1/x fixed x^2 -x -1 =0 x = (1 +_ 5 ^ {1/2})/2

(defun fan ()
  (fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0))

(print (fan))



        
