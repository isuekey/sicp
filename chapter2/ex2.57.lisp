



(print '(a b c))

(defun memq (item x)
  (cond ((null x) nil)
        ((eq item (car x)) x)
        (t (memq item (cdr x)))))

(print (memq 'apple '(pear banana prune)))

;;; dc/dx = 0, dx/dx = 1, d(u+v)/dx = du/dx + dv/dx, d(uv)/dx = udv/dx + vdu/dx
;;; 在大学的时候就听说了这个符号计算，但是却失去了好奇心
;;;

;;;(variable? e)
;;;(same-variable? v1 v2)
;;;(sum? e)
;;;(addend e)
;;;(augend e)
;;;(make-sum a1 a2)
;;;(product? e)
;;;(multiplier e)
;;;(multiplicand e)
;;;(make-product m1 m2)

(defun variable? (x) (symbolp x))
(defun same-variable? (v1 v2)
  (and (symbolp v1)
       (symbolp v2)
       (eq v1 v2)))
(defun sum? (exp)
  (and (consp exp)
       (eq (car exp) '+)))
(defun numberp? (a target)
  (and (numberp a) (= a target)))
(defun make-sum (a1 a2)
  (cond ((numberp? a1 0) a2)
        ((numberp? a2 0) a1)
        ((and (numberp a1) (numberp a2)) (+ a1 a2))
        (t (list '+ a1 a2))))
(defun make-product (m1 m2)
  (cond ((or (numberp? m1 0) (numberp? m2 0)) 0)
        ((numberp? m1 1) m2)
        ((numberp? m2 1) m1)
        ((and (numberp m1) (numberp m2) (* m1 m2)))
        (t (list '* m1 m2))))

(defun addend (exp)
  (cadr exp))
(defun augend (exp)
  (let ((gend (caddr exp))
        (gendlist (cddr exp)))
    (cond ((> (list-length gendlist) 1) (make-sum gend (cadr gendlist)))
          (t gend))))
(defun product? (exp)
  (and (consp exp)
       (eq (car exp) '*)))
(defun multiplier (exp)
  (cadr exp))
(defun multiplicand (exp)
  (let ((cand (caddr exp))
        (candlist (cddr exp)))
    (cond 
     ((> (list-length candlist) 1) (make-product cand (cadr candlist)))
     (t cand))))

;;; d(u^n)/dx = n(u^(n-1))du/dx

(defun exponentiation? (exp)
  (and (consp exp)
       (eq (car exp) 'expt)))

(defun base (exp)
  (cadr exp))
(defun exponent (exp)
  (caddr exp))
(defun make-exponentiation (base expo)
  (cond ((= expo 0) 1)
        ((= expo 1) base)
        (t (list 'expt base expo))))

(defun deriv (exp var)
  (cond ((numberp exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))
         )
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)))
         )
        ((exponentiation? exp)
         (let ((dbase (base exp))
               (dexp (exponent exp)))
           (make-product (make-product dexp (make-exponentiation dbase (- dexp 1)))
                         (deriv dbase var))))
        (t
         (error "unknown expression"))))

(print (deriv '(*  x y (+ x 3 4)) 'x))

;;; 还有漫长的路要走





