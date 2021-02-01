
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


(defun count-leaves2 (tr)
  (accumulate (lambda (x y) (cond ((null x) 0)
                                  ((not (consp x)) (+ 1 y))
                                  (t
                                   (+ (count-leaves2 x) y))))
              0 (mmap (lambda (x) x) tr)))

(defun count-leaves3 (tr)
  (accumulate '+ 0 (mmap (lambda (x) (cond ((null x) 0)
                                          ((consp x) (count-leaves3 x))
                                          (t 1))) tr)))


(defvar x (cons (list 1 2) (list 3 4)))

(defun count-leaves (x)
  (cond ((null x) 0)
        ((not (consp x)) 1)
        (t (+ (count-leaves (car x))
              (count-leaves (cdr x))))))

(print (length (list x)))
(print (count-leaves (list x x)))
(print (count-leaves2 (list x x x)))
(print (count-leaves2 (list 1 2)))
(print (count-leaves3 (list x x x x (list))))

