
;; 允许重复的表来表示集合

;; element-of-set? 没啥区别
(define (element-of-set? ele aset)
  (cond ((null? aset) #f)
        ((equal? ele (car aset)) #t)
        (else (element-of-set? ele (cdr aset)))))
;; 允许重复 所以不用检查
(define (adjoin-set ele aset)
  (cons ele aset))
;; intersection-set 其实也没啥变化
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
;; union-set 其实也没啥变化
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
         (cons (car set1) (union-set (cdr set1) set2)))))
;; 
;; 虽然实现变化不大，但是计算速度可能千差万别。
;; 因为集合允许重复元素导致很多冗余计算
;;

;; 这种集合什么时候会用，很奇怪
;; 唯一的优势 增加元素的时候比较快。
;; 我们需要一种需求：数据量很大，质量很好的集合数据进行初始化，
;; 在之后的使用中再也不增加元素。因为是O(N^2)的复杂度，所以
;; 即使少量的增长冗余m数据也会导致2Nm的增长。
;; 一次加载，陆续只读使用的需求，用这种实现会好些。
;; 比如应用的一些初始化配置项，只有在启动时加载
;;
