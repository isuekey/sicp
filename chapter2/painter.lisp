

(defun beside (painter-left painter-right))
(defun below (painter-below painter-up))
(defun flip-vert (painter))     ;上下反转
(defun flip-horiz (painter))    ;左右反转
(defun flip-pairs (painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(defun right-split (painter n)
  (if (= n 0)
      painter
    (let ((smaller (right-split painter (-n 1))))
      (beside painter (below smaller smaller)))))

(defun corner-split (painter n)
  (if (= n 0)
      painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (-n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split painter (-n 1))))
        (beside (below painter top-left)
                (bottom-right corner))))))

(defun square-limit (painter n)
  (let (quarter (corner-split painter n))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(defun square-of-four (tl tr bl br)
  (lambda (painter)
    (let ((top (beside (funcall tl painter) (funcall tr painter)))
          (tottom (beside (funcall bl painter) (funcall br painter))))
      (below bottom top))))

(defun flip-pairs2 (painter)
  (let ((combine4 (square-of-four identity flp-vert identity flip-vert)))
    (funcall combine4 painter)))

(defun square-limit2 (painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (funcall combine4 (corner-split painter n))))

(defun frame-coord-map (frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))
