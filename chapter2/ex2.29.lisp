

;;;(defun make-mobile (left right)
;;;  (list left right))

;;;(defun make-branch (length structure)
;;;  (list length structure))

;;;(defun left-branch (mobile)
;;;  (car mobile))
;;;(defun right-branch (mobile)
;;;  (car (cdr mobile)))
;;;(defun branch-length (branch)
;;;  (car branch))
;;;(defun branch-structure (branch)
;;;  (car (cdr branch)))


(defun make-mobile (left right)
  (cons left right))
(defun make-branch (length structure)
  (cons length structure))

(defun left-branch (mobile)
  (car mobile))
(defun right-branch (mobile)
  (cdr mobile))
(defun branch-length (branch)
  (car branch))
(defun branch-structure (branch)
  (cdr branch))

(defun total-weight (mobile)
  (if (not (consp mobile))
      mobile
    (let ((leftbranch (branch-structure (left-branch mobile)))
          (rightbranch (branch-structure (right-branch mobile))))
      (+ (if (consp leftbranch)
             (total-weight leftbranch)
           leftbranch)
         (if (consp rightbranch)
             (total-weight rightbranch)
           rightbranch)))))

(defun mobile-equal (mobile)
  (let ((leftlength (branch-length (left-branch mobile)))
        (rightlength (branch-length (right-branch mobile)))
        (leftstructure (branch-structure (left-branch mobile)))
        (rightstructure (branch-structure (right-branch mobile))))
    (and (= (* leftlength (total-weight leftstructure))
            (* rightlength (total-weight rightstructure)))
         (if (consp leftstructure) (mobile-equal leftstructure) t)
         (if (consp rightstructure) (mobile-equal rightstructure) t))))


(defvar mmm (make-mobile
             (make-branch 5 (make-mobile
                             (make-branch 10 11)
                             (make-branch 11 10)))
             (make-branch 15 (make-mobile
                             (make-branch 8 3)
                             (make-branch 6 4)))))
(defvar mmm2 (make-mobile
             (make-branch 5 (make-mobile
                             (make-branch 10 11)
                             (make-branch 11 10)))
             (make-branch 15 (make-mobile
                             (make-branch 8 3)
                             (make-branch 6 3)))))

(print mmm)
(print (left-branch mmm))
(print (right-branch mmm))
(print (branch-length (left-branch mmm)))
(print (branch-structure (left-branch mmm)))

(print (total-weight mmm))
(print (mobile-equal mmm))
(print (mobile-equal mmm2))
