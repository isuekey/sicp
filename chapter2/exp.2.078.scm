
;; type-tag, contents, attach-tag
(define (type-tag d)
  (if (pair? d) (car d)
      (error "bad tagged datum" d)))
(define (contents d)
  (if (pair? d) (cadr d)
      (error "bad tagged datum" d)))
(define (attach-tag type contents)
  (cons type contents))

;; 使用系统的内部类型
(define (type-tag d)
  (cond ((number? d) 'scheme-number)
        ((pair? d) (car d))
        (else (error "bad tagged datum" d))))

(define (contents d)
  (cond ((number? d) d)
        ((pair? d) (cadr d))
        (else (error "bad tagged datum" d))))

(define (attach-tag type contents)
  (if (number? contents) contents
      (cons type contents)))

;; 题目好像是上面的意思，具体参考答案

