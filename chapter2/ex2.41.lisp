
(defun enumerate-interval (begin end)
  (defun iter (b e result)
    (if (> b e)
        result
      (iter b (- e 1) (cons e result))))
  (iter begin end nil))

(defun mmap (proc seq)
  (if (null seq)
      seq
    (cons (funcall proc (car seq)) (mmap proc (cdr seq)))))

(defun accumulate (proc init seq)
  (if (null seq)
      init
    (funcall proc (car seq) (accumulate proc init (cdr seq)))))

(defun flatmap (proc seq)
  (accumulate 'append nil (mmap proc seq)))

(defun filter (proc seq)
  (defun filter-it (sub result)
    (if (null sub)
        result
      (filter-it (cdr sub) (if (funcall proc (car sub))
                               (cons (car sub) result)
                             result))))
  (filter-it seq nil))

(defun num-tri (n ts)
  (filter (lambda (tris)
            (= ts (accumulate '+ 0 tris)))
          (flatmap (lambda (x)
                     (flatmap (lambda (y)
                                (mmap (lambda (z)
                                        (list x y z))
                                      (enumerate-interval 1 (- y 1))))
                              (enumerate-interval 2 (- x 1))))
                   (enumerate-interval 3 n))))


(print (mmap (lambda (x) x) (list 1 2)))

(print (num-tri 5 10))
