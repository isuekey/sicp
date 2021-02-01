
;;; 因为不知道怎么测试，所以只能算是伪代码
(defun make-vect (x y)
  (cons x y))
(defun xcor-vect (v)
  (car v))
(defun ycor-vect (v)
  (cdr v))

(defun add-vect (v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(defun sub-vect (v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(defun scale-vect (s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))



(defun make-frame (origin edge1 edge2)
  (list origin edge1 edge2))

(defun origin-frame (frame)
  (car frame))
(defun edge1-frame (frame)
  (cadr frame))
(defun edge2-frame (frame)
  (caddr frame))


(defun make-frame2 (origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(defun origin-frame2 (frame)
  (car frame))
(defun edge1-frame2 (frame)
  (cadr frame))
(defun edge2-frame2 (frame)
  (cddr frame))
