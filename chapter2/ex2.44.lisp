
;;; 因为不知道怎么测试，所以只能算是伪代码
(defun up-split (painter n)
  (if (= n 0)
      painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))
