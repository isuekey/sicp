
;; 升序表

(define (make-tree entry left right)
  (list entry left right))

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

(list->tree (list 1 2 3))
(list->tree (list 1 2 3 4 5 6 7 8 9))
(list->tree (list 1 3 5 7 9 11))
;;                   5
;;         1                  9
;;    ()     3           7        11
;;         () ()       () ()    ()  ()

;; 通过n控制递归深度，使得this-entry对应到表中的相应位置，
;; 递归结束后形成相应的树

;; 深度d为log_2(n)，宽度为2^d，总复杂度O(n)
