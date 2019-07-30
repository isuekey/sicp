
(defun make-rat (n d)
  (let ((g (gcd n d)))
    (if (< (* n d) 0)
        (cons (- (/ (abs n) g)) (/ (abs d) g))
      (cons (/ (abs n) g) (/ (abs d) g)))))




(defun add-rat (x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(defun sub-rat (x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(defun mul-rat (x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(defun div-rat (x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(defun equal-rat? (x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

;;;(defun make-rat (n d)
;;;  (let ((g (gcd n d)))
;;;    (cons (/ n g) (/ d g))))
(defun numer (x) (car x))
(defun denom (x) (cdr x))

(defun print-rat (x)
  (princ #\Newline)
  (princ (numer x))
  (princ "/")
  (princ (denom x)))

(defvar one-half (make-rat -1 2))
(print-rat one-half)

(defvar one-third (make-rat -1 -3))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))
