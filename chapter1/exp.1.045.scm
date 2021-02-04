
;; x^n = n 的求值过程采用 y=x/(y^(n-1))的不动点求解
;; 平均阻尼方式处理
;; y = (y + x/(y^(n-1)))/2
;; x^2 = n, x^3 = n一次阻尼就可以完成
;; 但是 x^4 = n需要两次阻尼
;; 即 y = (y + (y + x/(y^(n-1)))/2)/2
;; 猜测需要 n/2次阻尼
;; 

(define (fixed-point g guess)
  (define (try guess)
    (let ((next (g guess)))
      (if (close-enough? next guess)
          next
          (try next))))
  (try guess))
(define (close-enough? a b)
  (< (abs (- a b)) dx))
(define dx 0.00001)

(define (average-damp f)
  (lambda (x)
    (average x (f x))))
(define (average x y) (/ (+ x y) 2))

(define (exp a n)
  (define (exp-iter a i r)
    (cond ((= i 0) r)
          ((even? i) (exp-iter (square a) (/ i 2) r))
          (else (exp-iter a (- i 1) (* r a)))))
  (exp-iter a n 1))
(define (even? a) (= (remainder a 2) 0))
(define (square a) (* a a))
(define (root n x)
  (define (f y) (/ x (exp y (- n 1))))
  (cond ((= n 1) x)
        ((even? n)
         (fixed-point
          ((repeat average-damp (/ n 2)) f)
          1.0))
        (else
         (fixed-point
          ((repeat average-damp (/ (- n 1) 2)) f)
          1.0))
        ))
(define (repeat g n)
  (define (iter i r)
    (if (= i n) r
        (iter (+ i 1)
              (lambda (x) (g (r x))))))
  (iter 1 g))
(root 2 4)
(root 3 8)
(root 2 9)
(root 10 1000000000)
(root 10 10000000000)




