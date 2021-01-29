
;;; 因为不知道怎么测试，所以只能算是伪代码

;;; 另SP表示 segments->painter

(defun border (frame)
  (draw-line ((frame-coord-map frame) (make-vect 0 0))
             ((frame-coord-map frame) (edge1-frame frame)))
  (draw-line ((frame-coord-map frame) (make-vect 0 0))
             ((frame-coord-map frame) (edge2-frame frame)))
  (draw-line ((frame-coord-map frame) (add-vect (edge1-frame frame) (edge2-frame frame)))
             ((frame-coord-map frame) (edge1-frame frame)))
  (draw-line ((frame-coord-map frame) (add-vect (edge1-frame frame) (edge2-frame frame)))
             ((frame-coord-map frame) (edge2-frame frame))))  

(defun contra (frame)
  (draw-line ((frame-coord-map frame) (make-vect 0 0))
             ((frame-coord-map frame) (add-vect (edge1-frame frame) (edge2-frame frame))))
  (draw-line ((frame-coord-map frame) (edge1-frame frame))
             ((frame-coord-map frame) (edge2-frame frame))))

(defun diamond (fame)
  (draw-line ((frame-coord-map frame) (make-vect 0 0))
             ((frame-coord-map frame) (add-vect (edge1-frame frame) (edge2-frame frame))))
  (draw-line ((frame-coord-map frame) (scale-vect (edge1-frame frame) 1/2))
             ((frame-coord-map frame) (scale-vect (edge2-frame frame) 1/2)))
  (draw-line ((frame-coord-map frame) (scale-vect (edge1-frame frame) 1/2))
             ((frame-coord-map frame) (add-vect (edge1-frame frame) (scale-vect (edge2-frame frame) 1/2))))
  (draw-line ((frame-coord-map frame) (scale-vect (edge1-frame frame) 1/2))
             ((frame-coord-map frame) (add-vect (scale-vect (edge1-frame frame) 1/2) (edge2-frame frame))))
  (draw-line ((frame-coord-map frame) (add-vect (edge1-frame frame) (scale-vect (edge2-frame frame) 1/2)))
             ((frame-coord-map frame) (add-vect (scale-vect (edge1-frame frame) 1/2) (edge2-frame frame))))
  )


(defun border2 (frame)
  ((SP (list (make-segment (make-vect 0 0)
                           (edge1-frame frame))
             (make-segment (make-vect 0 0)
                           (edge2-frame frame))
             (make-segment (add-vect (edge1-frame frame) (edge2-frame frame))
                           (edge1-frame frame))
             (make-segment (add-vect (edge1-frame frame) (edge2-frame frame))
                           (edge2-frame frame))
             )) frame ))

(defun wave (segmentlist)
  (lambda (frame)
    (foreach
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))


(defun wave2 (segmentlist)
  (SP segmentlist))
