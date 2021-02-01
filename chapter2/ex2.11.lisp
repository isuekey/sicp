
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
                 (t (let ((m1 (/ (+ lx ux) 2))
                          (m2 (/ (+ ly uy) 2)))
                      (cond ((< m1 0)
                             (cond ((< m2 0)
                                    (let ((lxuy (* lx uy))
                                          (uxly (* ux ly)))
                                      (cond ((< lxuy uxly) (make-interval lxuy (* lx ly)))
                                            (t (make-interval uxly (* lx ly))))))
                                   (t
                                    (let ((lxly (* lx ly))
                                          (uxuy (* ux uy)))
                                      (cond ((< lxly uxuy) (make-interval (* lx uy) uxuy))
                                            (t (make-interval (* lx uy) lxly)))))))
                            (t
                             (cond ((< m2 0)
                                    (let ((lxly (* lx ly))
                                          (uxuy (* ux uy)))
                                      (cond ((< lxly uxuy) (make-interval (* ux ly) uxuy))
                                            (t (make-interval (* ux ly) lxly)))))
                                   (t
                                    (let ((lxuy (* lx uy))
                                          (uxly (* ux ly)))
                                      (cond ((< lxuy uxly) (make-interval lxuy (* ux uy)))
                                            (t (make-interval uxly (* ux uy))))))))))))))))

;;; 考虑 (mid, with)方式进行分组, with>=0
;;; (m1, w1) (m2, w2)
;;; l1l2: m1m2 - m1w2 - w1m2 + w1w2
;;; l1u2: m1m2 + m1w2 - w1m2 - w1w2
;;; u1l2: m1m2 - m1w2 + w1m2 - w1w2
;;; u1u2: m1m2 + m1w2 + w1m2 + w1w2
;;; 最后会有3次乘法, 2次除2的操作
;;; 但是怎么说呢，目前有8种情况下只需要2次乘法，凑合用吧。

;;; 如果考虑 1, w1/m1, w2/m2的大小，可以将m1m2>0时的乘法计算控制到2次,
;;; m1=0, m2!=0; m1=0, m2=0; m1!=0, m2=0; 也可以将乘法控制到2次
;;; m1m2<0时 w1/m1与-w2/m2大小，也可以将乘法计算控制到2次
;;; 但是如果将除法运算考虑为乘以倒数的话其实都进行了4次乘法运算。
;;; 我考虑不出来只用2次乘法运算得出结果方法，即使对细节条件不做归纳整理
;;; 