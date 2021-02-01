
;;; 行向量v = (v_i), 矩阵行表示m = (m_ij)

(defvar vv (list 1 2 5))
(defvar mm (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(print vv)
(print mm)


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

(defun dot-product (v w)
  (accumulate '+ 0 (mapcar '* v w)))
(print vv)
(defvar ww (list 3 4 10))
(print ww)
(print (dot-product vv ww))

(defun matrix-vector (m v)
  (mmap (lambda (x) (accumulate '+ 0 (mapcar '* x v))) m))
(defvar vformul (list 1 2 5 6))
(print vformul)
(print mm)
(print (matrix-vector mm vformul))

(defun transpose (mat) (accumulate-n 'cons nil mat))

(defvar rm (list (list 1 2 3 4) (list 4 5 6 7) (list 7 8 9 10)))
(print rm)
(print (transpose rm))
;;; (1 4 7) (2 5 8) (3 6 9) (4 7 10)

(defun matrix-mul-matrix (m n)
  (let ((cols (transpose n)))
    (mmap (lambda (x) (mmap (lambda (y) (dot-product x y)) cols)) m)))

(defvar nr (list (list 1 2 3) (list 2 3 4)))
(print nr)
(print rm)
(print (matrix-mul-matrix nr rm))

