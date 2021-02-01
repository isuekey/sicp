
(defun mmap (proc items)
  (if (null items)
      items
    (cons (funcall proc (car items)) (mmap proc (cdr items)))))

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

(defun accumulate-n (op init seqs)
  (if (null (car seqs))
      nil
    (cons (accumulate op init (mmap (lambda (x) (car x)) seqs))
          (accumulate-n op init (mmap (lambda (x) (cdr x)) seqs)))))

(defun mappend (list1 list2)
  (if (null list1)
      list2
    (cons (car list1) (mappend (cdr list1) list2))))

(defun flatmap (proc seq)
  (accumulate 'append nil (mmap proc seq)))

(defun filter2 (proc seq)
  (defun filter-it (sub result)
    (if (null sub)
        result
      (filter-it (cdr sub) (if (funcall proc (car sub))
                               (cons (car sub) result)
                             result))))
  (filter-it seq nil))
