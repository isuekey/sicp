
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

;;; interval 表示成中点与宽度 (mid, width)，或者边界方式[lower, upper]
;;; mid = (lower + upper) / 2, width = (upper - lower) / 2
;;; int1 + int2 => [low1+low2, upp1 + upp2] = ((low1+low2+upp1+upp2)/2, (upp1+upp2 - low1 -low2)/ 2) = (mid1+mid2, width1 + width2)
;;; 同理可求 int1 - int2 = (mid1 - mid2, width1 + width2)
;;; 所以 和与差的宽度是 width1+width2 是宽度的函数。
;;; [1, 2] * [3, 4] => [3, 8] (5.5, 2.5)
;;; [1, 2] * [5, 6] => [5, 12] (8.5, 3.5) 不仅与宽度有关还与边界有关
;;; 如果边界都是正实数时
;;; int1 * int2 => [low1 * low2, upp1 * upp2] = ((low1 *low2 + upp1 * upp2)/2, (upp1*upp2 - low1 * low2) /2)
;;; width = ((mid1 + wid1) * (mid2+wid2) - (mid1 - wid1) * (mid2 - wid2)) /2
;;; = (mid1 * wid2 + wid1 * mid2 + mid2* wid1 + mid1 * wid2) / 2
;;; = (mid1 * wid2 + mid2 * wid1)


