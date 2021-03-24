(define (last-pair x)
  (if (null? (cdr x)) x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle '(a b c)))

(define (hasit pair alist)
  (cond ((null? alist) #f)
        ((not (pair? alist)) #f)
        ((equal? pair (car alist)) #t)
        (else (hasit pair (cdr alist)))))

(define (check-cycle alist)
  (let ((checked (list )))
    (define (iter-check target-list)
      (cond ((null? target-list) #f)
            ((not (pair? target-list)) #f)
            ((hasit target-list checked) #t)
            (else
             (begin (set! checked (cons target-list checked))
                    (or (iter-check (car target-list))
                        (iter-check (cdr target-list)))))))
    (iter-check alist)))
(check-cycle z)

#|
  使用O(n)空间的做法是显而易见的
  按照要求修改为需要常量空间的做法
  凭空想象是很难的，
  但是老家伙们看过的东西比较多
  一般都会有一些思路
  alist 本身就是一个结构依据，
  遍历alist的元素，检查是否存在其他位置与自身相等
  把list看成一个树的话，
  如果要构成一个环，那么环的入口元素必然在它的叶子上
  如果没有构成环，就没有这种
|# 
(define (check-cycle alist)
  
  )
