
;; fringe 一个用表表示的树作为参数, 返回一个表
;; 将树的叶子展平，顺序从左到右

(define x (list (list 1 2) (list 3 4)))
x
;; 预期
;; (fringe x) => (list 1 2 3 4)
;; (fringe (list x x)) => (list 1 2 3 4 1 2 3 4)

(define (fringe items)
  (define (iter subs answer)
    (cond ((null? subs) answer)
          ((pair? (car subs))
           (iter (cdr subs) (iter (car subs) answer)))
          (else
           (iter (cdr subs) (cons (car subs) answer)))
          ))
  (iter items ()))
(fringe x)
(fringe (list x x))
;; 很不好意思得到一个反序的
;; 简单处理一下反序
(define (fringe items)
  (define (iter subs answer)
    (cond ((null? subs) answer)
          ((pair? (car subs))
           (iter (cdr subs) (iter (car subs) answer)))
          (else
           (iter (cdr subs) (cons (car subs) answer)))
          ))
  (reverse (iter items ())))
(fringe x)
(fringe (list x x))
;; 考虑如何实现一个正序的
;; 考虑递归计算过程
(define (fringe items)
  (define (iter subs answer)
    (cond ((null? subs) answer)
          ((not (pair? (car subs)))
           (cons (car subs) (iter (cdr subs) answer)))
          (else
           (iter (cdr subs) (iter (car subs) answer)))))
  (iter items ()))
(fringe x)
(fringe (list x x))
;; (3 4 1 2)
;; (3 4 1 2 3 4 1 2)
;; 有进步了差强人意，继续考察
(define y (list (list 1 2 3) (list 4 5 6)))
(fringe y)
(fringe (list y y))
;; 预测
;; (4 5 6 1 2 3)
;; (4 5 6 1 2 3 4 5 6 1 2 3)
;; 显然 pair? 为真的结果反了
;; 即 else 过程有问题
(define (fringe items)
  (define (iter subs answer)
    (cond ((null? subs) answer)
          ((not (pair? (car subs)))
           (cons (car subs) (iter (cdr subs) answer)))
          (else
           (iter (car subs) (iter (cdr subs) answer)))))
  (iter items ()))
(fringe x)
(fringe (list x x))
(fringe (list y y))
(fringe (list (list x x) (list y y)))
;; 果然符合预期了。但是这是蒙的。
;; 并不是通过归纳分析出来的递归计算。
;; 再次考察了解到
;; 首先假定实现iter是个递归展平，将subs中元素从左到右，
;; 补充到answer左侧，且顺序不变的过程。
;; 如果(car subs) 是表，则结果为(iter (car subs) (iter (cdr subs) answer))
;; 如果(car subs)不是表，则为(cons (car subs) (iter (cdr subs) answer))
;; subs为空停止
;; 通过上述分析可知递归实现的核心是数学归纳过程
;; 应该从数学归纳抽象上明白过程含义
;; 上述代码有问题 answer是不必要的，iter也是不必要的

;; (define (fringe items)
;;   (cond ((null? items) items)
;;         ((not (pair? (car items)))
;;          (cons (car items) (fringe (cdr items))))
;;         (else
;;          (fringe (car items) (fringe (cdr items))))))
;; (fringe x)
;; (fringe (list x x))
;; (fringe (list y y))
;; (fringe (list (list x x) (list y y)))
;; 得意忘形了 answer是必要的 iter 也是必要的


