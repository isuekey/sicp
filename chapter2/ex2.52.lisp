
;;; 因为不知道怎么测试，所以只能算是伪代码

(defun wave-with-face (wave-segmentlist)
  (let ((face-segments (list (make-vect v1 v2))))
    (SP (append wave-segmentlist face-segments))))

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

;;; 使用rotate进行处理
