
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression for DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? x y)
  (and (symbol? x) (symbol? y) (eq? x y)))
(define (exp? exp)
  (if (not (pair? exp)) #f
      (> (length exp) 2)))
(define (sum? exp)
  (if (exp? exp) (eq? (cadr exp) '+)  #f))
(define (addend exp) (car exp))
(define (augend exp)
  (let ((aug (cddr exp)))
    (if (= (length aug) 1)
        (car aug)
        aug)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
(deriv '(x + 3 + y + x) 'x)
(deriv '(x + (3 + (y + x))) 'x)

(define (product? exp)
  (if (exp? exp) (eq? (cadr exp) '*) #f))
(define (multiplier exp) (car exp))
(define (multiplicand exp)
  (let ((cand (cddr exp)))
    (if (= (length cand) 1)
        (car cand)
        cand)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define tt '(x + (3 * (x + (y + 2)))))

;; a
;; (x + (3 * (x + (y + 2))))
(deriv '(x + (3 * (x + (y + 2)))) 'x)

(deriv '(x + 3 * (x + y + 2)) 'x)
;; 顺道就做了。 主要是augend multiplicand 的处理
;; 实际上有问题
(deriv '(x + 3 * x + y + 2) 'x)
4
;; 没有问题
(deriv '(x + 3 * x + y + 4 * x) 'x)
;; 这就有问题 会是 16
;; b
;; 根据教材的提示，我们应该先区分出加法，再区分出乘法
;; 这是由于我们用递归计算实现的过程，后检测出的表达式先被执行
;; 乘法项含有加法，需要使用()进行处理，这已经包含在以有的过程中了
;; 至于复合幂乘不在本题范围内。但是思路与加法组合乘法是一致的。
;; 调整加法相关的过程
(define (memq exp op)
  (cond ((not (pair? exp)) #f)
        ((eq? (car exp) op) exp)
        (else (memq (cdr exp) op))))
(define (sum? exp)
  (memq exp '+))
(define (addend exp)
  (define (iter subs)
    (cond ((null? subs) '())
          ((eq? (car subs) '+) '())
          (else (cons (car subs) (iter (cdr subs))))))
  (let ((addends (iter exp)))
    (if (= (length addends) 1) (car addends)
        addends)))
(addend '(3 * x + y + 4 * x))
;; (3 * x)
(addend '(y + 4 * x))
;; y
(define (augend exp)
  (cdr (memq exp '+)))
(sum? '(3 * x + y + 4 * x))
(memq '(3 * x + y + 4 * x) '+)
(augend '(3 * x + y + 4 * x))
(deriv '(x + 3 * x + y + 4 * x) 'x)
;; 8

