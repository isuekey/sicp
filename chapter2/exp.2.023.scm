
;; 目标
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))


(define (foreach proc list)
  (define (iter proc subs item)
    (proc item)
    (if (null? subs) subs
        (iter proc (cdr subs) (car subs))))
  (if (null? list) list
      (iter proc (cdr list) (car list))))
(foreach (lambda (x) (newline) (display x))
          (list 57 321 88))

;; 但是感觉foreach中用了两次条件检查比较奇怪
;; 试图一次检查

(define (foreach proc list)
  (if (null? list) list
      (let ()
        (proc (car list))
        (foreach proc (cdr list)))))
(foreach (lambda (x) (newline) (display x))
          (list 57 321 88))

;; 这里let过程主要是为了建立一个过程块
;; 应该有更好的过程表示方式
;; 
