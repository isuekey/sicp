
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

