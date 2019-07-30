
;;; 2 3 互质 2^a3^b，
;;; 如果存在不同的数对(a1,b1)(a2,b2)使得
;;; 2^a1 * 3^b1 = 2^a2 * 3^b2
;;; 那么 2^(a1-a2) * 3^(b1 - b2) = 1;存在非0的解
;;; 也就是说2^x与3^y有公约数,要么3^y有偶数，要么2^x能被3整除。显然不成立
;;; 2^a3^b可以表示(a,b)
(defun econ (a b) (cons (cons 2 a) (cons 3 b)))

;;; 只考虑非负整数情况

(defun mexp (x n)
  (defun mexp-it (count result)
    (if (= count n)
        result
      (mexp-it (+ count 1) (* x result))))
  (mexp-it 0 1))

(defun mcons (a b) (* (mexp 2 a) (mexp 3 b)))

(defun car-it (za result base)
  (if (= (mod za base) 0)
      (car-it (/ za base) (+ result 1) base)
    result))
(defun mcar (z)
  (car-it z 0 2))
(defun mcdr (z)
  (car-it z 0 3))

(defvar mm (mcons 4 8))

(print mm)
(print (mcar mm))
(print (mcdr mm))
