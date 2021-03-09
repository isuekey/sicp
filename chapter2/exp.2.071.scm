;; 需要在本题或者下一题中回顾本节所用的代码
;; 最好能够自行写出绝大多数的代码。
;; 不然实践能力偏弱
(define (enumerate-interval begin end)
  (if (>= begin end) (cons end ())
      (cons begin (enumerate-interval (+ begin 1) end))))

(define chars '(a b c d e f g h i j k l m n o p q r s t))
(enumerate-interval 1 5)
(enumerate-interval 1 10)
(define (pairs interval chars)
  (if (null? interval)
      ()
      (cons (cons (car chars) (car interval))
            (pairs (cdr interval) (cdr chars)))))
(define hertz5 (map (lambda (x) (expt 2 (- x 1)))
                    (enumerate-interval 1 5)))
(define hertz10 (map (lambda (x) (expt 2 (- x 1)))
                    (enumerate-interval 1 10)))

(define interval5 (pairs hertz5 chars))
(define interval10 (pairs hertz10 chars))

interval5
interval10
(define message5 '(a e))
(define message10 '(a j))
message5
message10

(define (adjoin-leaf-set ele set)
  (cond ((null? set) (list ele))
        ((< (weight ele) (weight (car set))) (cons ele set))
        (else (cons (car set)
                    (adjoin-set ele (cdr set))))))
(define (weight node)
  (if (leaf? node) (weight-leaf node)
      (cadddr node)))
(define (leaf? node)
  (eq? (car node) 'leaf))
(define (weight-leaf leaf)
  (caddr leaf))

(define (make-leaf-set pairs)
  (if (null? pairs) '()
      (let ((pair (car pairs)))
        (adjoin-leaf-set (make-leaf (car pair)
                                    (cdr pair))
                         (make-leaf-set (cdr pairs))))))
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (generate-huffman pairs)
  (successive-merge (make-leaf-set pairs)))
;; 升序的
(define (successive-merge leafsets)
  (cond ((null? leafsets) '())
        ((= (length leafsets) 1) (car leafsets))
        ((= (length leafsets) 2) (make-code-tree (cadr leafsets) (car leafsets)))
        (else (if (<= (weight (car leafsets)) (weight (cadr leafsets)))
                  (successive-merge (cons (make-code-tree (cadr leafsets) (car leafsets))
                                          (cddr leafsets)))
                  (successive-merge (cons (make-code-tree (make-code-tree (caddr leafsets)
                                                                          (cadr leafsets))
                                                          (car leafsets))
                                          (cdddr leafsets)))))))
(define (make-code-tree left right)
  (list left right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (append list1 list2)
  (if (null? list1) list2
      (cons (car list1) (append (cdr list1) list2))))
;; (append '(a b c d) '(e f g))
(define (symbols node)
  (if (leaf? node) (list (symbol-leaf node))
      (caddr node)))
(define (symbol-leaf leaf) (cadr leaf))
(define (left-branch node) (car node))
(define (right-branch node) (cadr node))

(define tree5 (generate-huffman interval5))
(define tree10 (generate-huffman interval10))
tree5
tree10

(define (encode message tree)
  (if (null? message) '()
      (append (find-symbol (car message) tree) (encode (cdr message) tree))))
(define (find-symbol item tree)
  (cond ((leaf? tree) '())
        ((has-symbol? item (left-branch tree))
         (cons 0 (find-symbol item (left-branch tree))))
        (else (cons 1 (find-symbol item (right-branch tree))))))
(define (has-symbol? item node)
  (item-in? item (symbols node)))
(define (item-in? item list)
  (cond ((null? list) #f)
        ((eq? item (car list)) list)
        (else (item-in? item (cdr list)))))
(encode message5 tree5)
(encode message10 tree10)

;; n=5
;; 最频繁1：0，最不频繁4：1111
;;
;;
;; n=10
;; 最频繁1：0，最不频繁9：111111111
;; 
;; 重新写了下，下笔前觉得很困难，但是按照之前学过的思路
;; 逐步实现的话，也就是麻烦些，没啥难度
;; 


  
