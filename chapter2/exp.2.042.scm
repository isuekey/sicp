
(define (queens block-size)
  (define (queen-cols k)
    (if (= k 0)
        (list ())
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 block-size)))
          (queen-cols (- k 1))))))
  (queen-cols block-size))

;; k=0 是 空集
;; k-1 的 queen-cols
;; 填充k列所有值，得到block-size的备选k queen-cols
;; 检查k是否能够在 之前的记录里存在
;; 递归的返回到k=block-size的结果上去

(define (filter predicate sequence)
  (cond ((null? sequence) ())
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (flatmap proc sequence)
  (accumulate append () (map proc sequence)))
(define (accumulate proc initial sequence)
  (if (null? sequence) initial
      (proc (car sequence) (accumulate proc initial (cdr sequence)))))
(define (enumerate-interval begin end)
  (if (>= begin end) (list end)
      (cons begin (enumerate-interval (+ begin 1) end))))
(define (safe? k positions)
  (define (iter col subs)
    (if (null? subs) #t
        (let ((kpos (car positions))
              (cpos (car subs)))
          (cond ((= (cdr kpos) (cdr cpos)) #f)
                ((= (- (+ (cdr cpos) k) col) (cdr kpos)) #f)
                ((= (+ (- (cdr cpos) k) col) (cdr kpos)) #f)
                (else
                 (iter (- col 1) (cdr subs)))))))
  (if (null? positions) #t
      (iter (- k 1) (cdr positions))))

(define (adjoin-position new-row k rest-of-queens)
  (cons (cons k new-row) rest-of-queens))
(queens 8)


