
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
  (let ((local-table (list '*table* '())))
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
            (if (keys)
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
    (define (insert-iter! key1 key2 keys item)
      (let ((subitem (lookup-iter key1 '() item)))
        (if (subitem)
            (cond ((null? keys) (set-cdr! (item-tree subitem) key2))
                  ((not (is-tree? (item-tree subitem)))
                   (set-cdr! subitem (cons
                  (else
                   (insert-iter! key2 (car keys) (cdr

    (define (insert! key1 key2 . keys)
      (insert-iter! key1 key2 keys local-table)
      'ok)
    dispatch))

