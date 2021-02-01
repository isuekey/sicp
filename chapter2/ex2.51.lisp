
;;; 因为不知道怎么测试，所以只能算是伪代码


(defun transform-painter (painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (mkae-frame new-origin
                             (sub-vect (m corner1) new-origin)
                             (sub-vect (m corner2) new-origin)))))))

(defun below (painter-below painter-up)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-blow (transform-painter painter-below
                                         (make-vect 0.0 0.0)
                                         (make-vect 1.0 0.0)
                                         split-point))
          (paint-up (transform-painter painter-up
                                       split-point
                                       (make-vect 1.0 0.5)
                                       (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-below frame)
        (paint-up frame)))))

(defun rotate180 (painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(defun rotate270 (painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(defun below2 (painter-below painter-up)
  (rotate90 (beside (rotate180 painter-up)
                    (rotate180 painter-below))))

