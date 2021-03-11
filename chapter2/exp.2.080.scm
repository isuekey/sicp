
#|
按照我最开所想，
最好每个安装包都增加相应的内容,这种实现也很方便
但是现实是别人安装包很难随意的变更

答案给出了更有效的补丁方式。注意这是补丁，是对其他安装包的扩展
而不是其他包的过程的增强。
|#

;; 这里需要使用exp.2.078的增强内容
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

(define (=zero? x) (apply-generic '=zero? x))
(put '=zero? '(scheme-number) (lambda (x) (= x 0)))
(put '=zero? '(rectangular)
     (lambda (x) (and (=zero? (real-part x))
                      (=zero? (imag-part x)))))
(put '=zero? '(polar)
     (lambda (x) (=zero? (magnitude x))))
(put '=zero? '(complex) =zero?)
(put '=zero? '(rational)
     (lambda (x) (and (=zero? (numer x)))))

;; 这种做法的要好很多
;; 如果安装包提供了相应的操作，也可以通过简单的修改使用相应的过程

