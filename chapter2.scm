
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

