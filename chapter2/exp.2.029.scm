
;; 二叉活动体

(define (make-mobile left right)
  (list left right))
;; 每个分支具有确定长度的杆，调一个重量或者其他活动体
(define (make-branch length structure)
  (list length structure))

(define (left-branch m)
  (car m))
(define (right-branch m)
  (car (cdr m)))
(define (branch-length b)
  (car b))
(define (branch-structure b)
  (car (cdr b)))
(define (total-weight m)
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))
(define (branch-weight b)
  (if (pair? (branch-structure b))
      (total-weight (branch-structure b))
      (branch-structure b)))

;; 平衡 任何活动体下直属分支 长度*重量相等
(define (total-balance m)
  (define (branch-moment b)
    (* (branch-length b)
       (branch-weight b)))
  (let ((left (left-branch m))
        (right (right-branch m)))
    (and (= (branch-moment left)
            (branch-moment right))
         (if (pair? (branch-structure left))
             (total-balance (branch-structure left))
             #t)
         (if (pair? (branch-structure right))
             (total-balance (branch-structure right))
             #t)))
  )

(define left1 (make-branch 10 3))
(define r1 (make-branch 4 10))
(define r2 (make-branch 5 8))
(define rm (make-mobile r1 r2))
(define right1 (make-branch 5 rm))
(define m1 (make-mobile left1 right1))
(total-weight m1) ;; 21
(total-weight rm) ;; 18
;; 除了一些低级的拼写错误，没有啥问题，已经更正了
(total-balance rm)
(total-balance m1)
(define left2 (make-branch 9 10))
(define m2 (make-mobile left2 right1))
(total-balance m2)

;; 如果
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))
;; 需要调整
(define (right-branch m)
  (cdr m))
(define (branch-structure b)
  (cdr b))

;;
(define left1 (make-branch 10 3))
(define r1 (make-branch 4 10))
(define r2 (make-branch 5 8))
(define rm (make-mobile r1 r2))
(define right1 (make-branch 5 rm))
(define m1 (make-mobile left1 right1))
(total-weight m1) ;; 21
(total-weight rm) ;; 18
(total-balance rm)
(total-balance m1)
(define left2 (make-branch 9 10))
(define m2 (make-mobile left2 right1))
(total-balance m2)

;; 工作正常
;; 再次提现了抽象隔离的优越性
;;
;; 但是有种制造优越性的感觉
;; 谁没事变更底层数据结构呢
;; 通常如果发生这种需求的时候，
;; 我宁可通过增加下行转换与上行转换
;; 通过修改数据结构的方式满足原有过程
;; 而不是调整原来的数据接口，历史数据调整
;; 通常产生灾难性的后果

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch m)
  (car m))
(define (right-branch m)
  (car (cdr m)))
(define (branch-length b)
  (car b))
(define (branch-structure b)
  (car (cdr b)))

(define (cons-mobile left right)
  (cons left right))
(define (cons-branch length structure)
  (cons length structure))

(define left1 (cons-branch 10 3))
(define r1 (cons-branch 4 10))
(define r2 (cons-branch 5 8))
(define rm (cons-mobile r1 r2))
(define right1 (cons-branch 5 rm))
(define m1 (cons-mobile left1 right1))

(define (down-trans-mobile consmobile)
  (if (pair? consmobile)
      (make-mobile (down-trans-branch (car consmobile))
                   (down-trans-branch (cdr consmobile)))
      consmobile)
  )
(define (down-trans-branch consbranch)
  (make-branch (car consbranch)
               (down-trans-mobile (cdr consbranch))))
(define mm1 (down-trans-mobile m1))
(define mrm (down-trans-mobile rm))
(total-weight mm1) ;; 21
(total-weight mrm) ;; 18
(total-balance mrm)
(total-balance mm1)
(define left2 (cons-branch 9 10))
(define m2 (cons-mobile left2 right1))
(define mm2 (down-trans-mobile m2))
(total-balance mm2)
;; 工作正常的
;; 这样看起来很繁琐，远不如修改选择过程方便
;; 但是在实际工作中，更多的是模块之间的调用
;; 模块之间的调用稳定性要求很高，只要在入口处处理一次就可以了。
;; 前面所述内容更多的是在某个模块内的思考过程
;; 需求层面上的防御式编程
;; 
