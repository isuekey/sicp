
;; 平衡二叉树实现的集合上O(n)复杂度 union-set, intersection-set实现
;; union-set

;; intersection-set

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))
(define tree->list tree->list-1)
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; 其实利用63 64结论的话，不考虑优化的情况，是很容易处理
;; 利用63 获得两个升序表 list1 list2 这里显然是 O(n)的
;; 合并两个表成一个升序表，之前的练习表明O(n)的合并也很容易
;; 游标的方式处理两个表。
;; 并集两个全要，抛弃一个重复；交集只要同时存在的
;; 根据64 结果升序表转换成平衡树 也时O(n)
;; 有限次的 O(n) 组合的过程，其复杂度还是 O(n)
;;
;; 个人处理这个问题的话很麻烦。我很难想到一个O(n)的方式处理过程
;; 但是分开成三个过程的组合的话，升序表组装成树的过程比较麻烦
;; 难以想到O(n)复杂度的过程。不要说限制复杂度，仅仅是实现这个效果
;; 就很难处理了。
;; 不过如果分成三个部分进行思考的话，确实有完成的可能。
;; 个人觉得教材在这里暗示了一些程序实现的策略。
;;

(define (union-set set1 set2)
  (define (union-list list1 list2)
    (cond ((null? list1) list2)
          ((null? list2) list1)
          (else (let ((item1 (car list1))
                      (item2 (car list2)))
                  (cond ((< item1 item2) (cons item1 (union-list (cdr list1) list2)))
                        ((> item1 item2) (cons item2 (union-list list1 (cdr list2))))
                        (else (cons item1 (union-list (cdr list1) (cdr list2)))))))))
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (list->tree (union-list list1 list2))
    ))

(union-set (list->tree (list 1 3 5 7)) (list->tree (list 1 2 3 4 5 6)))

(define (intersection-set set1 set2)
  (define (intersection-list list1 list2)
    (if (or (null? list1) (null? list2))
        ()
        (let ((item1 (car list1))
              (item2 (car list2)))
          (cond ((< item1 item2) (intersection-list (cdr list1) list2))
                ((> item1 item2) (intersection-list list1 (cdr list2)))
                (else (cons item1 (intersection-list (cdr list1) (cdr list2))))))))
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (list->tree (intersection-list list1 list2)))
  )

(intersection-set (list->tree (list 1 3 5 7)) (list->tree (list 1 2 3 4 5 6)))

