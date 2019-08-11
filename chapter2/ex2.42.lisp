
(defun enumerate-interval (begin end)
  (defun iter (ib ie result)
    (if (> ib ie)
        result
      (iter ib (1- ie) (cons ie result))))
  (iter begin end nil))

;;;(defun queens (board-size)
;;;  (defun iter-queen (col row result))
;;;  (defun check-queen (position queenhad))
;;;  (defun queen-cols))

(defun filter (proc seq)
  (defun filter-it (sub result)
    (if (null sub)
        result
      (filter-it (cdr sub) (if (funcall proc (car sub))
                               (cons (car sub) result)
                             result))))
  (filter-it seq nil))

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

(defun adjoin-position (new-row k rest-of-queens)
  (append rest-of-queens (list (cons new-row k))))


(defun safe? (k positions)
  (defun get-position-iter (begin pos seqs)
    (if (> begin k)
        pos
      (get-position-iter (+ begin 1) (car seqs) (cdr seqs))))
  
  (defun safe-position (position target)
    (if (null position)
        t
      (and (not (= (abs
                    (- (car position)
                       (car target)))
                   (abs
                    (- (cdr position)
                       (cdr target)))
                   ))
           (not (= (car position) (car target)))
           (not (= (cdr position) (cdr target))))
      ))
            
  
  (let ((kpos (get-position-iter 1 nil positions)))
    (let ((rest (remove kpos positions)))
      (accumulate (lambda (cur others)
                    (and (safe-position kpos cur) others)) t rest))))

(defun queens (board-size)
  (defun queen-cols (k)
    (if (= k 0)
        (list nil)
      (filter
       (lambda (positions) (safe? k positions))
       (flatmap
        (lambda (rest-of-queens)
          (mmap (lambda (new-row)
                  (adjoin-position new-row k rest-of-queens))
                (enumerate-interval 1 board-size)))
        (queen-cols (- k 1))))))
  (queen-cols board-size))

(print (queens 6))
(print (queens 7))
;;;
;;; 线性计算过程的递归过程跟数学归纳法很像，就是假定n-1成立，如何产生n成立
;;; 1表示步长，n和0表示递归边界
;;; 如果这样看待问题再将其中一些具体过程抽象，很多难以入手的问题并不复杂
;;;
