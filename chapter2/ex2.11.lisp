
(defun add-interval (x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(defun div-interval (x y)
  (if (< (* (lower-bound y) (upper-bound y)) 0)
      (error "y interval cover 0")
    (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))


(defun make-interval (a b)
  (if (< a b)
      (cons a b)
    (cons b a)))
(defun lower-bound (interval) (car interval))
(defun upper-bound (interval) (cdr interval))

(defvar aa (make-interval 1 2))
(defvar bb (make-interval 3 4))
(defvar cc (make-interval -1 2))

(defun mul-interval (x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((not (< lx 0))
           (cond ((not (< ly 0)) (make-interval (* lx ly) (* ux uy)))
                 ((not (> uy 0)) (make-interval (* ux ly) (* lx uy)))
                 (t (make-interval (* ux ly) (* ux uy)))))
          ((not (> ux 0))
           (cond ((not (< ly 0)) (make-interval (* lx uy) (* ux ly)))
                 ((not (> uy 0)) (make-interval (* ux uy) (* lx ly)))
                 (t (make-interval (* lx uy) (* lx ly)))))
          (t
           (cond ((not (< ly 0)) (make-interval (* lx uy) (* ux uy)))
                 ((not (> uy 0)) (make-interval (* ux ly) (* lx ly)))
                 (t (make-interval (min (* lx uy) (* ux ly)) (max (* lx ly) (* ux uy)))))))))

;;; 最后会有4次乘法，所以需要改进
;;; (-15, 10) (-14 9.8) , (-15 10) (-14 1) 
;;; 应该是分组的规则不对

;;; 考虑 (mid, with)方式进行分组, with>=0
;;; (m1, w1) (m2, w2)
;;; l1l2: m1m2 - m1w2 - w1m2 + w1w2
;;; l1u2: m1m2 + m1w2 - w1m2 - w1w2
;;; u1l2: m1m2 - m1w2 + w1m2 - w1w2
;;; u1u2: m1m2 + m1w2 + w1m2 + w1w2
;;; 这里要考虑 m1w2 - w1m2的值来判断l1u2 - u1l2的正负，
;;; 但是这么考虑的话无论如何都要进行4次乘法进行求值。
;;; 如果想只进行2次乘法运算，还需要思考新的模型。

(defun mul-interval-m (x y)
  (let ((m1 (/ (+ (lower-bound x) (upper-bound x)) 2))
        (w1 (/ (- (upper-bound x) (lower-bound x)) 2))
        (m2 (/ (+ (lower-bound y) (upper-bound y)) 2))
        (w2 (/ (- (upper-bound y) (lower-bound y)) 2)))))                                 
;;;
