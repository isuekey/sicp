
;; 构造数据抽象
;; 数据抽象将逻辑的处理过程与处理内容隔离开，是有力的设计方法

;; 数据抽象将帮助我们在程序的不同部分之间建立抽象屏障。

;; 复合数据的关键思想是闭包的概念
;;


(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (/ (* (numer x) (numer y))
               (* (denom x) (denom y)))))
(define (div-rat x y)
  (make-rat (/ (* (numer x) (denom y))
               (* (denom x) (numer y)))))
(define (equal-rat x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x)) (display "/") (display (denom x)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;; 使用过程来表示序对，以说明序对是个抽象的概念过程
;; 而不是具体的实现过程
(define (cons a b)
  (define (dispatch m)
    (cond ((= m 0) a)
          ((= m 1) b)
          (else (error "参数只能是0与1 -- 序对" m))))
  dispatch)
(define (car z) (z 0))
(define (cdr z) (z 1))
;; 从而表明数据与过程实现之间没有清晰的边界，其实是一致的

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (maker-interval (min p1 p2 p3 p4)
                    (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
(define (make-interval a b) (cons a b))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (make-center-percent c p)
  (let ((width (/ (* c p) 100)))
    (make-interval (- c width)
                   (+ c width))))
(define (percent i)
  (* (/ (width i) (center i)) 100))

;; 电阻并联计算总电阻误差的时候
;; R1*R2/(R1 + R2)
(define (part1 r1 r2)
  (div-interval (mul-interval r1 r2) (add-interval r1 r2)))
;;1/(1/R1+1/R2)
(define (part2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;; exp.2.016需要进一步考量

(list 1 2 3 4)
(cons 1 (cons 2 (cons 3 (cons 4 ()))))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25 36 49))
(list-ref squares 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define (length items)
  (define (iter subs count)
    (if (null? subs)
        count
        (iter (cdr subs) (+ count 1))))
  (iter items 0))
(define odds (list 1 3 5 7 9 11))
(length odds)

(define (append list1 list2)
  (define (appending idx result)
    (if (< idx 0)
        result
        (appending (- idx 1) (cons (list-ref list1 idx) result))))
  (appending (- (length list1) 1) list2))
(append odds squares)
(append squares odds)
;; 居然是迭代计算过程,但是计算复杂度却更大。
;; 教材给的是递归计算过程

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
(append odds squares)
(append squares odds)

(define squares (list 1 4 9 16 25 36 49))
(define odds (list 1 3 5 7 9 11))

;; 尾部标记法 (define (f x y . z) <body>) 表示f接受至少两个参数
;; x y 为正常参数 ， z为表 例如
;; (f 1 2 3 4 5 6)，x=1, y=2, z=(3 4 5 6)
;; (define (g . w) <body>) 表示0或多个参数，都用表w表示
;; lambda方式有些区别需要注意
;; (define f (lambda (x y .z) <body>))
;; (define g (lambda w <body>))

(define odds (list 1 3 5 7 9 11))
(define (scale-list items factor)
  (if (null? items)
      ()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))
(scale-list odds 10)

(define (map proc items)
  (if (null? items)
      ()
      (cons (proc (car items))
            (map proc (cdr items)))))
(map abs (list 1 -2 -0.5 3 18))
;; 根据map给出新的scale-list定义
(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))
;; 标准 map接收是过个相同长度的表
(map + (list 1 2 3) (list 4 5 6) (list 7 8 9))
;; 得到 (12 15 18)

;; 层次性结构

(cons (list 1 2) (list 3 4))
(define x (cons (list 1 2) (list 3 4)))
(length x)
3
;; (count-leaves x) => 4
(define xx (list x x))
(length xx)
2
;; (count-leaves xx) => 8

(define (count-leaves  x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(count-leaves x)
(count-leaves xx)

;; 对树的映射
(define (scale-tree tree factor)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
;; 用map实现
(define (scale-tree tree factor)
  (map (lambda (x)
         (if (pair? x)
             (scale-tree x factor)
             (* x factor)))
       tree)
  )
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
;; 由原来的思考整个树，变为思考某个元素的处理，隔离了树的细节
;; 考察直接定义，对之前的一些练习与思考有新的认识
;; 例如deep-reverse等的递归实现

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-square (car tree))
                 (sum-odd-square (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        ()
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

;; 序列操作

(define (filter predicate list)
  (cond ((null? list) list)
        ((predicate (car list))
         (cons (car list) (filter predicate (cdr list))))
        (else
         (filter predicate (cdr list)))))
(filter odd? (list 1 2 3 4 5 6 7))

(define (accumulate op initial items)
  (if (null? items) initial
      (op (car items)
          (accumulate op initial (cdr items)))))
(accumulate + 0 (list 1 2 3 4 5 6 7 8 9 10))
(accumulate * 1 (list 1 2 3 4 5 6 7 8 9 10))

;; Horner 规则 计算多项式
;; a_nx^n + a_{n-1}^{n-1} + ... + a_1x + a_0 =>
;; (...(a_nx + a_{n-1})x + ... + a_1)x + a_0

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-coeff)
                (+ this-coeff
                   (* x higer-coeff))
                   )
              0
              coefficient-sequence))

;; 向量v = (v_i)，矩阵 m = (m_ij)表示为向量（矩阵行）的序列
;; 这里很奇怪 数学上喜欢用列向量表示矩阵，程序上习惯用行向量表示矩阵

(define m (list (list 1 2 3 4)
                (list 4 5 6 6)
                (list 6 7 8 9)))

;; (dot-product v w)
;; (matrix-*-vector m v)
;; (matrix-m-matrix m n)
;; (transpose m)

(define (accumulate-n op init seqs)
  (if (null? (car seqs)) ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (enumerate-interval begin end)
  (if (>= begin end) (cons end ())
      (cons begin (enumerate-interval (+ begin 1) end))))
(enumerate-interval 1 10)

(accumulate append
            ()
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 2 n)))
(define (ok n) (accumulate append
            ()
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 2 n))))

(define (accumulate op initial items)
  (if (null? items) initial
      (op (car items)
          (accumulate op initial (cdr items)))))
(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))
(define (prime? n)
  (= (smallest-divisor n) n))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divisor? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divisor? a b)
  (= (remainder b a) 0))
(prime-sum-pairs 4)
(define (kankan n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))
(prime-sum-pairs 4)


(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

;; 抽象过程的方式处理wave
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (square-limit painter n)
  (let ((quarter (corner-split (painter n)))
        (let ((half (beside (flip-horiz quater) quarter)))
          (below (flip-vert half) half)))))


(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity fip-vert identity flip-vert)))
    (combine4 painter)))
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;; 可以抽象的处理，而不关心实现

;; make-frame origin-frame edge1-frame edge2-frame

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))
((frame-coord-map a-frame) (make-vect 0 0))
;; 结果如下
(origin-frame a-frame)


;;46
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)p

(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))
;;47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (car (cdr frame)))
(define (edge2-frame frame)
  (car (cdr (cdr frame))))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (car (cdr frame)))
(define (edge2-frame frame)
  (cdr (cdr frame)))

;; 23
(define (for-each proc list)
  (if (null? list) list
      (let ()
        (proc (car list))
        (for-each proc (cdr list)))))

(define (segments->painter segement-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))
;; 按照数学上的习惯都是逆时针旋转为正向。
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (tranform-painter painter1
                             (make-vect 0.0 0.0)
                             split-point
                             (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (painter-left frame)
        (painter-right frame)))))

        
(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
(memq 'apple '(peer banana prune))
(memq 'apple '(x (apple sauce) y apple peer))

(variable? e)
(same-variable? v1 v2)
(sum? e)
(addend e) ;; e的被加数
(augend e) ;; e的加数
(make-sum a1 a2)

(product? e)
(multiplier e) ;; e的被乘数 
(multiplicand e) ;; e的乘数
(make-product m1 m2)
;; 基本过程 number? 是否数值

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))

(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-product m1 m2)
  (cond ((=number? m1 0) 0)
        ((=number? m2 0) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set ele set)
  (if (element-of-sets ele set) set
      (cons ele set)))
;; 使用递归计算获取交集
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
;; 其实可以用filter来处理，可能计算量偏大
(define (intersection-set set1 set2)
  (filter (lambda (ele)
            (element-of-set ele set2))
          set1))
;; 使用教材类似的过程处理union-set
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
         (cons (car set1) (union-set (cdr set1) set2)))))

;; 有序表表示集合
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2)) '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2) (intersection-set (cdr set1) set2))
              ((> x1 x2) (intersection-set set1 (cdr set2)))))))
;; 二叉树表示集合
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))
(define (adjoin-set x set)
  (cond ((null? set) (list x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


;; Huffman树
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? node)
  (eq? (car node) 'leaf))
(define (symbol-leaf node) (cadr node))
(define (weight-leaf node) (caddr node))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols node)
  (if (leaf? node) (list (symbol-leaf node))
      (caddr node)))
(define (weight node)
  (if (leaf? node) (weight-leaf node)
      (cadddr node)))
;; 解码过程
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit --- CHOOSE BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;; ((A 4) (B 2) (C 1) (D 1))
;; 构造出树叶的排序集合
(define aList (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))

(define (make-leaf-set pairs)
  (if (null? pairs) '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;; 2.4
(make-from-real-imag (real-part z) (imag-part z))
(make-from-mag-ang (magnitude z) (angle z))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

;; 直角坐标系
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))
;; 极坐标系
(define (real-part z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part z)
  (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(define (make-from-mag-ang r a) (cons r a))

;; 2.4.2 带标志数据
(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum) (car datum)
      (error "Bad tagged datum" datum)))
(define (contents datum)
  (if (pair? datum) (cdr datum)
      (error "Bad tagged datum" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))
;; 直角坐标
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a)))))
;; 极坐标
(define (real-part-polar z) (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z) (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar (cons (sqrt (+ (square x) (square y)))
                           (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

;; 通用选择
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "unkown type" z))))
(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "unkown type" z))))
(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "unkown type" z))))
(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "unkown type" z))))
;; 这样在复数上的运算就不受具体结构的影响了。虽然速度一定不一样
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;; put 与 get 还没有实现，只能是想象
;; Ben的安装包
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) mangnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (maek-from-mag-ang r a))))
  'done)

;; Alyssa的安装包
(define (install-polar-package)
  ;; internal procedures
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (make-from-mag-ang r a) (cons r a))
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) mangnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (maek-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC"
                 (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'real-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang x y)
  ((get 'make-from-mag-ang 'polar) x y))


(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)
(define (apply-generic op arg) (arg op))


;; 数据导向的 通用算术包
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "no method for these type -- APPLY-GENERIC" (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number shceme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number shceme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number shceme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number shceme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (/ (* (numer x) (numer y))
                 (* (denom x) (denom y)))))
  (define (div-rat x y)
    (make-rat (/ (* (numer x) (denom y))
                 (* (denom x) (numer y)))))
  (define (equal-rat x y)
    (= (* (numer x) (denom y))
       (* (denom x) (numer y))))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational-number n d)
  ((get 'make 'rational) n d))

;; 从这里看，我们对显示调用，数据导向以及消息传递的理解不深刻
;; 至少数据导向的理解过于浮于表面。
;; 

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-real-imag (* (magnitude z1) (magnitude z2))
                         (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-real-imag (/ (magnitude z1) (magnitude z2))
                         (- (angle z1) (angle z2))))

  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  
  'done)

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; 强制
;; 显示处理
(define (add-complex-to-schemenum z x)
  (make-from-real-imag (+ (real-part z) x)
                       (imag-part z)))
;; 强制转换
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if 
