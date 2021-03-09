
(define (make-leaf-set pairs)
  (if (null? pairs) '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

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
(define aList (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))

(make-leaf-set aList)

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
;; 这个是错的
(define (successive-merge leafs)
  (define (iter-merge leafs)
    (cond ((null? leafs) '())
          ((pair? (cadr leafs)) (cons (make-code-tree (cadr leafs) (car leafs))
                                      (iter-merge (cddr leafs))))
          (else (car leafs))))
  (if (or (null? leafs) (< (length leafs) 2))
      leafs
      (successive-merge (iter-merge leafs))))
(generate-huffman-tree aList)

(define (encode message tree)
  (if (null? message) '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
(define (encode-symbol symbol tree)
  (cond ((null? tree) '())
        ((leaf? tree) '())
        ((memq symbol (symbols (left-branch tree))) (cons 0 (encode-symbol symbol (left-branch tree))))
        ((memq symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "bad letter -- ENCODE SYMBOL" symbol))))

(define bList (list (list 'A 2) (list 'NA 16) (list 'BOOM 1)
                    (list 'SHA 3) (list 'GET 2) (list 'YIP 9)
                    (list 'JOB 2) (list 'WAH 1)))
(make-leaf-set bList)
(define bTree (generate-huffman-tree bList))

(define bMessage '(Get a job
                       Sha na na na na na na na na
                       Get a job
                       Sha na na na na na na na na
                       Wah yip yip yip yip yip yip yip yip yip
                       Sha boom))
(length (encode bMessage bTree))
;; 144
;; 
;; 如果用等长编码 需要三个位来描述 000-111
(* (length bMessage) 3)
;; 108 个编码就够了，好尴尬
;; 是为啥呢，因为bTree不正确
;; 不符合Huffman树的规范
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge leafs)
  (cond ((null? leafs) '())
        ((= (length leafs) 1) (car leafs))
        ((= (length leafs) 2) (make-code-tree (cadr leafs) (car leafs)))
        (else (let ((first (car leafs))
                    (second (cadr leafs))
                    (third (caddr leafs)))
                (cond ((<= (weight first) (weight second))
                       (successive-merge (cons (make-code-tree second first) (cddr leafs))))
                      (else
                       (successive-merge (cons first (cons (make-code-tree third second) (cdddr leafs))))))))))

(define bTree (generate-huffman-tree bList))
;; (generate-huffman-tree aList)

(define bMessage '(Get a job
                       Sha na na na na na na na na
                       Get a job
                       Sha na na na na na na na na
                       Wah yip yip yip yip yip yip yip yip yip
                       Sha boom))
(length (encode bMessage bTree))
;; 86
;; 这个结果还是令我满意的。节约了20%空间
;; 至于是否是最优解，我没有看答案
;;
;; 编码树其实集合的长度至少要是2，因为如果少于一个字符，那么不需要编码
;; 只需要给长度数字就可以了.
;;
;; 

