
(define (f g)
  (g 2))

(define (square a) (* a a ))

(f square)
(f (lambda (a) (* a (+ a 1))))

;; (f f)
;;The object 2 is not applicable.
;;To continue, call RESTART with an option number:
;; (RESTART 2) => Specify a procedure to use in its place.
;; (RESTART 1) => Return to read-eval-print level 1.

(f f)
(f 2)
(2 2)
;; 查看答案后了解到的。
;; 正则序与应用序都是成立的
;;
