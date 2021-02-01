
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

(defun mmap (proc items)
  (if (null items)
      items
    (cons (funcall proc (car items)) (mmap proc (cdr items)))))

(defun accumulate-n (op init seqs)
  (if (null (car seqs))
      nil
    (cons (accumulate op init (mmap (lambda (x) (car x)) seqs))
          (accumulate-n op init (mmap (lambda (x) (cdr x)) seqs)))))

(defvar mseq (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(print mseq)
(print (accumulate-n '+ 0 mseq))
;;; 期望 22 26 30
