
;; 预期
(define (map p sequence)
  (accumulate (lambda (x y)
                <??>)
              () sequence))
(define (append seq1 seq2)
  (accumulate cons <??> <??>))

(define (length sequence)
  (accumulate <??> 0 sequence))

;; 实现

(define (accumulate op initial items)
  (if (null? items) initial
      (op (car items)
          (accumulate op initial (cdr items)))))

(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              () sequence))
(map square (list 1 2 3 4 5))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(append (list 1 3 5) (list 2 4 6))

