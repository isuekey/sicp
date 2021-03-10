
;; 求导过程
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (defiv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ;; ...
        (else (error "unknown expression type - DERIV" exp))))

;; 看上面的内容，其实与复数实现很像。两个维度
;; 类型:加法求导，乘法求导
;; 求导:
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; 数据导向风格
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; a)
;; (get 'deriv (operate exp)) 获得实际求导过程
;; (operands exp) 求导过程的表达式
;; number? 和 same-variable? 也可以放到分派中，
;; 但是这是所有求导都遵循的内容，没有差异行为



;; b) 针对和式与乘式求导的安装包
(define (install-add-deriv-package)
  (define (make-sum addend augend)
    (list '+ addend augend))
  (define (addend s) (cadr s))
  (define (augend s) (acddr s))
  (define (sum? s) (eq? (car s) '+))

  (put 'make-sum '+ make-sum)
  (put 'deriv '+ (lambda (exp var)
                   (make-sum (deriv (addend exp) var)
                             (deriv (augend exp) var))))
  
  'done)

(define (install-mul-deriv-package)
  (define (make-product multiplier multiplicand)
    (list '* multiplier multiplicand))
  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))

  (install-add-deriv-package)
  (define make-sum (get 'make-sum '+))
  (put 'deriv '*
       (lambda (operands var)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)))))
  'done)

;; 这里写的其实似是而非

;; c)
(define (install-expo-deriv-package)
  (define (exponentiation? exp)
    (and (pair? exp) (eq? (car exp) '**)))
  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          (else (list '** base exponent))))
  (define (base exp)
    (cadr exp))
  (define (exponent exp)
    (caddr exp))
  (install-mul-deriv-package)
  (define make-product (get 'make-product '*))
  (put 'deriv '** (lambda (exp var)
                    (let ((exp-base (base exp))
                          (exp-exponent (exponent exp)))
                      (make-product
                       (make-product exp-exponent (make-exponentiation exp-base (- exp-exponent 1)))
                       (deriv exp-base var)))))

  'done)
;; d)
((get (operator exp) 'deriv) (operands exp) var)
#|
各自的安装包里的(put 'deriv <operator> <??>)更改为
(put <operator> 'deriv <??>)即可

总体而言这个习题，还是个思想训练，有些细节是无法处理到的
因为没有put、get实现，无法验证效果
|#
