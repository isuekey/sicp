
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

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))
(define sample-code '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define sample-message (decode sample-code sample-tree))

;; encode过程
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
(encode sample-message sample-tree)
;; 考虑
;; ((leaf e 16) ((leaf d 8) ((leaf c 4) ((leaf b 2) (leaf a 1) (b a) 3) (c b a) 7) (d c b a) 15) (e d c b a) 31)
;; 这种频率的编码
;; 这个需要概率学的支持。我怂了。
#|
1-5个级别的字符
假定某字符级别为x（1<=x<=5），
出现概率近似 2^(x-1)/2^5,
判断检测深度:左侧检查次数 5 - x + 1
右侧检查次数 5-x + 4-x + .. = (5-x+1)(5-x)/2
总次数 (5-x+1) (5-x+2)/2
最频率字符 x=5 单次计算1 n个字符速度增长O(0)
最不频繁 x= 1 单次计算1 n个字符速度增长O(n^2)
呃，没有问平均效率，瞎担心
我也不知道结果如何，我认为对的就可以了。留个念项以备复习用
|#

