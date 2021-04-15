
#|
  如果基于exp.3.025进行修改的话
  需要修改两个过程 assoc 与 insert-iter
  assoc 的检索过程
  insert-iter 中的新增过程
  chapter:2.3.3 关于 二叉树的描述, 首先key必须是可以排序的
  node ((keybase val) left right)
  null? node => #f
  key == keybase => val
  key < keybase 搜索 left
  key > keybase 搜索 right
  新增的的时候类似，但是现在的我需要使用set!处理
  但是在2.3.3里必然没有使用这种方式处理
  我重新找到了原来的过程
  使用递归的方式重建二叉树叶子
|#

(define (make-table)
  (let ((local-table (list (cons '*table* '()))))
    ;; node: tree or leaf
    ;; tree: (list (cons key val) left right)
    ;; leaf: (list (cons key val))
    (define (same-key? key1 key2)
      (equal? key1 key2))
    (define (less-key? key1 key2)
      (< key1 key2))
    (define (item-tree tree)
      (car tree))
    (define (tree-key tree)
      (car (item-tree tree)))
    (define (leaf-tree? tree)
      (null? (cdr tree)))
    (define (left-tree tree)
      (cadr tree))
    (define (right-tree tree)
      (caddr tree))
    (define (assoc key tree)
      (cond ((null? tree) #f)
            ((same-key? key (tree-key tree)) (item-tree tree))
            ((leaf-tree? tree) #f)
            ((less-key? key (tree-key tree)) (assoc key (left-tree tree)))
            (else (assoc key (right-tree tree)))))
    (define (dispatch m)
      (cond ((equal? m 'lookup-proc) lookup)
            ((equal? m 'insert-proc) insert!)
            (else (error "unknown proc info" m))))
    #|
       后面的其实是熟练度的问题，所以不太想写了。
       但是对于程序员，熟练度很重要。还是要练习的。
       尤其这个地方使用的是函数式编程，
       跟命令式编程有很大的差异
    |#
    (define (item-tree item) (cdr item))
    (define (lookup-iter key1 keys item)
      (if (null? item) item
          (let ((record (assoc key (item-tree item))))
            (if keys
                (lookup-iter (car keys) (cdr keys) record)
                record))))
    (define (lookup key1 . keys)
      (lookup-iter key1 keys local-table))
    (define (is-tree? tree)
      (pair? tree))
    #|
       实践证明，差好多。 如果使用命令式，很容易写。
       但是函数式的，真见鬼。
       感觉无论如何需要一次set!，这是内部的, 
       确实是这样，某级的node的表格需要被替换掉。
       这样就好了。不然就可替换local-table的，并且所有
       从头开始，梳理完，觉得可以写了
    |#
    (define (adjoin item tree)
      (let ((key (tree-key)))
        (cond ((same-key? key (car item)) (set-car! tree item))
              ((less-key?))))
      (cond ((null? tree) #f)
            ((same-key? key (tree-key tree)) (item-tree tree))
            ((leaf-tree? tree) #f)
            ((less-key? key (tree-key tree)) (assoc key (left-tree tree)))
            (else (assoc key (right-tree tree)))))
                                
    (define (insert-iter! key1 key2 keys item)
      (let ((subitem (lookup-iter key1 '() item)))
        (if (not subitem)
            (cond ((null? keys)
                   (set-cdr i))))
        (if subitem
            (cond ((null? keys) (set-cdr! (item-tree subitem) key2))
                  ((not (is-tree? (item-tree subitem)))
                   (set-cdr! subitem (cons
                  (else
                   (insert-iter! key2 (car keys) (cdr))))))))))
    (define (insert! key1 key2 . keys)
      (insert-iter! key1 key2 keys local-table)
      'ok)
    dispatch))
#|
写的乱七八糟的，根本不通
核心是由于没有进行有效的拆解
纠缠在一起
其实拆分后应该很好实现
table:(*table*, tree) 是特殊的 record， recordkey is '*table*
make-table: () => table
table: lookup: (keys) => redord, 
table: insert!: (keys val) => 'result

record:(key, recordValue)
make-record: (key, recordValue) => record
recordValue: value or tree
record-value: (record) => recordValue
record-key: (record) => key

tree = node: (record, leftbranch, rightbranch)
empty-tree?: (tree) => #t or #f
is-tree?: (recordValue) => #t or #f
tree:find-node: (key, tree) => node or '()
tree:adjoin-record: (record, tree) => 'result
tree:left-branch:(tree) => leftBranch
tree:right-branch:(tree) => rightBranch
leftBranch=rightBranch=tree=node
tree:less-key?, same-key?, greater-key?

make-tree = make-node: (record, leftBranch, rightBranch) 与树一致
is-value?:(recordValue) => (not (is-tree? recordValue))
node-record: (node) => (car record)
tree:find-record: (key, tree) => (node-record (find-node key tree))

这样从归纳法上就解决问题
上面之所以难搞，就是混到一起了
|#

(define (make-table)
  (define (make-record key val) (cons key val))
  (define (record-value record) (cdr record))
  (define (record-key record) (car record))

  (define (make-tree record leftBranch rightBranch) (list record leftBranch rightBranch))
  ;; 这个过程不严谨，但是无所谓了，主要是思路
  (define (is-tree? value) (and (list? value) (= (length value) 3)))
  (define (same-key? key1 key2) (equal? key1 key2))
  (define (less-key? key1 key2) (< key1 key2))
  (define (greater-key? key1 key2) (> key1 key2))
  (define (tree-left-branch tree) (cadr tree))
  (define (tree-right-branch tree) (caddr tree))
  (define (empty-tree? tree)
    (or (null? tree) (null? (car tree))))
  ;; tree is node
  (define (node-record tree)
    (if (null? tree) tree (car tree)))
  (define (is-value? value) (not (is-tree? value)))
  (define (tree-find-node key tree)
    (cond ((empty-tree? tree) '())
          ((not (is-tree? tree)) '())
          (else 
           (let ((record (node-record tree))
                 (leftBranch (tree-left-branch tree))
                 (rightBranch (tree-right-branch tree))
                 (target-key (record-key (car tree))))
             (cond ((same-key? key target-key) tree)
                   ((less-key? key target-key) (tree-find-node key leftBranch))
                   ((greater-key? key target-key) (tree-find-node key rightBranch))
                   (else (error "unknown compare" key target-key)))))))
  (define (tree-find-record key tree)
    (node-record (tree-find-node key tree)))
  (define (tree-adjoin-record! record tree)
    (cond ((null? tree)
           (begin (set! tree (make-tree record '() '()))
                  tree))
          ((not (is-tree? tree))
           (begin (set! tree (make-tree record '() '()))
                  tree))
          ((empty-tree? tree)
           (begin (set-car! tree record)
                  tree))
          (else
           (let ((target-record (node-record tree))
                 (leftBranch (tree-left-branch tree))
                 (rightBranch (tree-right-branch tree))
                 (target-key (record-key (car tree)))
                 (key (record-key record)))
             (cond ((same-key? key target-key)
                    (begin (set-cdr! target-record (record-value record))
                           tree))
                   ((less-key? key target-key)
                    (begin (tree-adjoin-record! record leftBranch)
                           tree))
                   ((greater-key? key target-key)
                    (begin (tree-adjoin-record! record rightBranch)
                           tree))
                   (else (error "unknown compare" key target-key)))))))
  ;; 现在感觉好极了，经过分析，解决问题，巩固知识
  (let ((localtable (cons '*table* (make-tree '() '() '()))))
    (define (lookup-iter key keys tree)
      (let ((found-record (tree-find-record key tree)))
        (cond ((not found-record) #f)
              ((null? keys) (record-value found-record))
              (else (lookup-iter (car keys) (cdr keys) (record-value found-record))))))
    (define (lookup key . keys)
      (lookup-iter key keys (cdr localtable)))
    (define (insert-iter! key1 key2 keys tree)
      (cond ((null? keys)
             (tree-adjoin-record! (make-record key1 key2) tree))
            (else
             (let ((found-record (tree-find-record key1 tree)))
               (if (not (null? found-record))
                   (begin
                     (set-cdr! found-record
                               (insert-iter! key2 (car keys) (cdr keys) (record-value found-record)))
                     (cdr found-record))
                   (tree-adjoin-record!
                    (make-record
                     key1
                     (insert-iter! key2 (car keys) (cdr keys) (make-tree '() '() '())))
                    tree))))))
    (define (insert! key1 key2 . keys)
      (insert-iter! key1 key2 keys (cdr localtable))
      'ok)
    (define (dispatch m)
      (cond ((equal? m 'lookup-proc) lookup)
            ((equal? m 'insert-proc) insert!)
            ((equal? m '*table*) (cdr localtable))
            (else (error "unknown proc"))))
    dispatch
    ))
(define mm (make-table))
(define mmi (mm 'insert-proc))
(define mml (mm 'lookup-proc))
(define mmt (mm '*table*))
(mmi 'a 'b 'c)
(mmi 'a 'b)
(mmi 'a 'b 'c)
mmt
#|
;; 有很多细节有问题，
;; 但是已经很好了
;; 又花了2小时
;; 回顾一下问题，
;; 没有约定好返回值类型 导致书写时乱了
;;

  
|#
