

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

(defvar alist (list 1 2 3 4))
(print alist)

(defun enumerate-interval (begin end)
  (defun iter (sb se result)
    (if (> sb se)
        result
      (iter sb (- se 1) (cons se result))))
  (iter begin end nil))
(print (enumerate-interval 2 4))

(defun num-pair (n)
  (accumulate 'mappend nil (mmap (lambda (i)
                                   (mmap (lambda (j) (list i j))
                                         (enumerate-interval 1 (- i 1))))
                                 (enumerate-interval 2 n))))

(print (num-pair 5))

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

(defun prime-sum-pair (n)
  (mmap 'make-pair-sum
        (filter 'prime-pair (num-pair n))))

(print (prime-sum-pair 5))

(defun num-pair2 (n)
  (flatmap (lambda (i)
             (mmap (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(defun prime-sum-pair2 (n)
  (mmap 'make-pair-sum
        (filter 'prime-pair (num-pair2 n))))
(print (prime-sum-pair2 5))


(defun permutations (s)
  (if (null s)
      (list nil)
    (flatmap (lambda (x)
               (mmap (lambda (p) (cons x p))
                    (permutations (remove x s))))
             s)))

(print (permutations (list 1 2 3)))

(defun mremove (item seq)
  (filter (lambda (x) (not (= x item))) seq))
(print (mremove 3 (list 1 2 3)))
