
(defun accumulate (op initial sequence)
  (if (null sequence)
      initial
    (funcall op (car sequence)
             (accumulate op initial (cdr sequence)))))

(defun filter (predicate sequence)
  (cond ((null sequence) nil)
        ((funcall predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (t (filter predicate (cdr sequence)))))


(defun mmap (p sequence)
  (accumulate (lambda (x y) (cons (funcall p x) y)) nil sequence))

(defun mappend (seq1 seq2)
  (accumulate 'cons (if (null seq1)
                        seq2
                      nil)
              (if (null seq1)
                  nil
                (cons (car seq1) (mappend (cdr seq1) seq2)))))
(defun  mlength (sequence)
  (accumulate (lambda (x y) (if (null x)
                                 y
                               (+ 1 y))) 0 sequence))


(print (mmap (lambda (x) (* x 2)) (list 1 2 3)))


(print (mappend (list 2 3 4) (list 5 7 9)))

(print (mlength (mappend (list 2 3 4) (list 5 7 9))))



