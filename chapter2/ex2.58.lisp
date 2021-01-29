

(defvar dexp '(x + 3 * (x + y + 2)))

;;; 考察其表现
(print dexp); (x + 3 * (X + Y + 2))
(print (car dexp)); X
(print (cdr dexp)); (+ 3 * (X + Y + 2))
(print (cadr dexp)); +
(print (cddr dexp)); (3 * (X + Y + 2))
(print (car (cddr dexp))); 3
(print (cdr (cddr dexp))); (* (X + Y + 2))
(print (cadr (cddr dexp))); *
(print (cddr (cddr dexp))); ((X + y + 2))

;;; 直接考虑问题b
;;; 采用递归的思路处理
;;; 获取操作符号
;;; 前部/后部的计算，抽象后发现没啥变化



;;; exp的长度要么 >= 3 要么 = 1，其他不正确
(defun variable? (x) (symbolp x))
(defun same-variable? (v1 v2)
  (and (symbolp v1)
       (symbolp v2)
       (eq v1 v2)))
(defun split-list (alist begin end)
  (defun split-iter (sublist idx result)
    (cond ((< idx begin) (split-iter (cdr sublist) (1+ idx) result))
          ((< idx end) (split-iter (cdr sublist) (1+ idx) (cons (car sublist) result)))
          (t result)))
  (reverse (split-iter alist 0 nil)))
(defun index-first (alist var)
  (defun index-it (sublist idx var)
    (if (eq (car sublist) var)
        idx
      (index-it (cdr sublist) (1+ idx) var)))
  (index-it alist 0 var))

(defun sum? (exp)
  (and (consp exp)
       (let ((add-idx (index-first exp '+)))
         (> add-idx 0))))

(defun addend (exp)
  (let ((first-add (index-first exp '+)))
    (split-list exp 0 first-add)))
(defun augend (exp)
  (let ((first-add (index-first exp '+)))
    (split-list exp (1+ first-add) (list-length exp))))
(defun product? (exp)
  (and (consp exp)
       (eq (cadr exp) '*)))

(defun numberp? (a target)
  (and (numberp a) (= a target)))
(defun make-sum (a1 a2)
  (cond ((numberp? a1 0) a2)
        ((numberp? a2 0) a1)
        ((and (numberp a1) (numberp a2)) (+ a1 a2))
        (t (list a1 '+ a2))))
(defun make-product (m1 m2)
  (cond ((or (numberp? m1 0) (numberp? m2 0)) 0)
        ((numberp? m1 1) m2)
        ((numberp? m2 1) m1)
        ((and (numberp m1) (numberp m2) (* m1 m2)))
        (t (list m1 '* m2))))
;;; 比想象的难，改进一下，似乎应该是 同层遍历完，然后乘法同层递归完；进入下一层
(defun mderiv (exp var)
  (print exp)
  (cond ((numberp exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((= (list-length exp) 1) (mderiv (car exp) var))
        ((> (list-length exp) 2)
         (cond ((sum? exp) (make-sum (mderiv (addend exp) var)
                                     (mderiv (augend exp) var)))
               (t exp)))
        (t
         (error "unkown expression"))))


(defvar dexpt '(x + 3 * (x + y + 2) + y + 3))
;;;(print (cddr dexpt)); (3 * (x + y + 2) + y + 3))
;;;(print (car (cddr dexpt))); 3
;;;(print (cdr (cddr dexpt))); (* (x + y + 2) + y + 3)
;;;(print (cadr (cddr dexpt))); *
;;;(print (cddr (cddr dexpt))); ((x + y + 2) + y + 3)

(print "dexpt")
(print dexpt)
(print (addend dexpt))
(print (augend dexpt))

(print "mderiv")
(print (mderiv dexpt 'x))

