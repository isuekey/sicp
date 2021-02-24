
;; 使用 folder-right folder-left 重写 reverse

(define (folder-right op initial sequences)
  (if (null? sequences) initial
      (op (car sequences)
          (accumulate op initial (cdr sequences)))))

(define (folder-left op initial sequence)
  (define (iter subs result)
    (if (null? subs) result
        (iter (cdr subs)
              (op result (car subs)))))
  (iter sequence initial))

(define (reverse items)
  (folder-right cons () items))
(reverse (list 1 2 3 4))

;; 不可大意
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse items)
  (fold-right (lambda (x y)
                (append y (list x)))
              () items))
(reverse (list 1 2 3 4))

(define (reverse items)
  (fold-left (lambda (x y)
               (cons y x))
             () items))
(reverse (list 1 2 3 4))
