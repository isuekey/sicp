
(defun add-interval (x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defun div-interval (x y)
  (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))


(defun make-interval (a b) (cons a b))
(defun lower-bound (interval) (car interval))
(defun upper-bound (interval) (cdr interval))

(defun make-center-width (c w)
  (make-interval (- c w) (+ c w)))
(defun center (i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(defun width (i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(defun make-center-percent (c p)
  (make-interval (- c (* c p)) (+ c (* c p))))
(defun percent (i)
  (cond ((= (center i) 0) 0)
        (t (/ (width i) (center i)))))

;;; 都为正数时，且p << c
(defun mul-percent (x y)
  (+ (percent x) (percent y)))
