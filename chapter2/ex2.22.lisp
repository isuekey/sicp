
(defun square-list (items)
  (defun iter (things answer)
    (if (null things)
        answer
      (iter (cdr things)
            (cons (square (car things))
                  answer))))
  (iter items nil))
;;; 不为什么，就是这样。越先出来的数字平方，越靠近结果尾部。

;;; 更换顺序也不行，因为 (cons list ele)不会在list的尾部
;;; 追加，而是形成了一个类似二维数组的东东
