
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion typ2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "no method for these types" (list op type-args))))))
              (error "no method for these types" (list op type-args)))))))


;; a)
;; 同类型也会调用强制转换，是因为对应的op没有定义。
;; 如果增加下列过程
(define (schem-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex
              complex->complex)

#|
就会在无定义操作的情况下，重复循环进行
|#

(define (exp x y) (apply-generic 'exp x y))
(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y))))

;; (exp z1 z2) 会死循环(栈溢出而崩溃，这要看具体的环境，我反正是不知道）

;; b) 没有。很容以出问题。

;; c)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2) (error "no method for these types" (list op type-args))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion typ2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "no method for these types" (list op type-args)))))))
              (error "no method for these types" (list op type-args)))))))

