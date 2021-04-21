
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;; add: (a b c-in sum c-out)
;;(define add1-example '(a b c-in1 sum c-out1))
;;(define add2-example '(a2 b2 c-in2 sum2 c-out2))
;;add1-example
;;add2-example
(define (get-c-out add)
  (car (cddddr add)))
(define (set-c-in! add-in add-out)
  (set-car! (cddr add) (get-c-out add-out)))
;;(set-c-in! add2-example (get-c-out add1-example))
;;add2-example

;; 先计算两个加法器的组合
(define (twoAdder add1 add2)
  (set-c-in! add2 add1)
  (apply full-adder add1)
  (apply full-adder add2))

(define (array-adder adders)
  (define (array-adder-iter adder1 sub-adders)
    (if (null? sub-adders) (apply full-adder adder1)
        (let ((adder2 (car sub-adders)))
          (set-c-in! adder2 adder1)
          (apply full-adder adder1)
          (array-adder-iter adder2 (cdr sub-adders)))))
  (if (null? adders) 'ok
      (array-adder-iter (car adders) (cdr adders))))

;; adder-length = (length adders)
;; 总耗时 adder-length * (delay adder)

