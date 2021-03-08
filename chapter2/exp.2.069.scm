
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
(define (successive-merge leafs)
  (define (iter-merge leafs)
    (cond ((null? leafs) '())
          ((pair? (cadr leafs)) (cons (make-code-tree (cadr leafs) (car leafs))
                                      (iter-merge (cddr leafs))))
          (else (car leafs))))
  (if (or (null? leafs) (< (length leafs) 2))
      (car leafs)
      (successive-merge (iter-merge leafs))))
(successive-merge (make-leaf-set aList))

;; 看着是对的实际上是错的
;; 不符合Huffman要求
;; ((leaf d 1) (leaf c 1) (leaf b 2) (leaf a 4))
;; 按照Huffman要求。权重最低的两个节点进行组合
;; ((leafc leafd (c d) 2) (leaf b 2) (leaf a 4))
;; ((leafb nodecd (b c d) 4) (leaf a 4))
;; (leafa nodebcd (a b c d) 8)
;; ((leaf a 4) ((leaf b 2) ((leaf c 1) (leaf d 1) (c d) 2) (b c d) 4) (a b c d) 8)
;; 这才是符合Huffman要求的树
;; 而不是
;; (((leaf a 4) (leaf b 2) (a b) 6) ((leaf c 1) (leaf d 1) (c d) 2) (a b c d) 8)
(define (successive-merge leafs)
  (define (iter-merge leafs)
    (display leafs)
    (newline)
    (cond ((null? leafs) '())
          ((= (length leafs) 1) (car leafs))
          (else (let ((first (car leafs))
                      (second (cadr leafs)))
                  (if (<= (weight first) (weight second))
                      (iter-merge (cons (make-code-tree second first) (cddr leafs)))
                      (cons fisrt (iter-merge (cdr leafs))))))))
  (if (or (null? leafs) (< (length leafs) 2))
      (car leafs)
      (successive-merge (iter-merge leafs))))
(successive-merge (make-leaf-set aList))
