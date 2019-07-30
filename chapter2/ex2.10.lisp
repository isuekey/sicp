
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
  (if (< (* (lower-bound y) (upper-bound y)) 0)
      (error "y interval cover 0")
    (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))


(defun make-interval (a b) (cons a b))
(defun lower-bound (interval) (car interval))
(defun upper-bound (interval) (cdr interval))

(defvar aa (make-interval 1 2))
(defvar bb (make-interval 3 4))
(defvar cc (make-interval -1 2))

(print (div-interval aa bb))
(print (div-interval aa cc))

