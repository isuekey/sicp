
;; a,b正整数表示为2^a*3^b的有理数。
;; 我们就可以用有理数和算术运算表示正整数序对;
;; 2,3互素不存在归约计算。可以用反正法证明
;; 任何重复的数字必然会有相同的正整数序对a,b

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car x)
  (define (car-iter xr er)
    (if (= (remainder xr 2) 1)
        er
        (car-iter (/ xr 2) (+ er 1))))
  (car-iter x 0))

(define (cdr x)
  (define (cdr-iter xr er)
    (if (not (= (remainder xr 3) 0))
        er
        (cdr-iter (/ xr 3) (+ er 1))))
  (cdr-iter x 0))

(define c (cons 3 4))
(display c)
(display (car c))
(display (cdr c))



