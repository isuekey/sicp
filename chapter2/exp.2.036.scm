

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init <??>)
            (accumulate-n op init <??>))))

;; 预期
;; accumulate-n
;; ((1 2 3) (4 5 6) (7 8 9) (10 11 12))
;; (accumulate-n + 0 s)
;; 得到 (22 26 30)

(define x (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
x

(define (accumulate op initial items)
  (if (null? items) initial
      (op (car items)
          (accumulate op initial (cdr items)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(accumulate-n + 0 x)
;; 总感觉还有更牛逼的方式

;; 果然答案
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

