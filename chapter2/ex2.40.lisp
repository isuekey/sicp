
(defun accumulate (op initial sequence)
  (if (null sequence)
      initial
    (funcall op (car sequence)
             (accumulate op initial (cdr sequence)))))

(defun mmap (proc items)
  (if (null items)
      items
    (cons (funcall proc (car items)) (mmap proc (cdr items)))))

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
  (accumulate 'mappend nil (mmap proc seq)))

(defun enumerate-interval (begin end)
  (defun iter (sb se result)
    (if (> sb se)
        result
      (iter sb (- se 1) (cons se result))))
  (iter begin end nil))
(print (enumerate-interval 2 4))

(defun unique-pairs (n)
  (flatmap (lambda (i)
             (mmap (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(defun smallest-divisor (n)
  (smallest-divisor-it n 2))
(defun smallest-divisor-it (n div)
  (cond ((> (* div div) n) n)
        ((= (mod n div) 0) div)
        (t (smallest-divisor-it n (+ div 1)))))
(defun prime? (n)
  (= n (smallest-divisor n)))

(defun prime-pair (pair)
  (prime? (+ (car pair) (cadr pair))))

(defun make-pair-sum (pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(defun prime-sum-pairs (n)
  (mmap 'make-pair-sum
        (filter 'prime-pair (unique-pairs n))))

(print (prime-sum-pairs 5))

