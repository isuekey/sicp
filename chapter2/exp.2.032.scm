
;; 一个有限集合，获得所有子集的表方式展示
(list 1 2 3)
;; 获得 (() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3)), 可以顺序无关

;; 如果考虑直接方式会很麻烦，
;; 我们考察递归这种归纳方式。其实是受找零过程的启发。
;; 含有指定元素子集，与排除此元素的其他子集的和
;; 比如说g 是(2 3) 的所有子集的集合，f是g的所有元素增加一个元素1的集合
;; 那么(1 2 3)就是g与f的并集
;; 递归的向下进行后就得到结果
;; 利用list与cons的关系容易的到

(define (list-set items)
  (if (null? items)
      items
      (cons (list-set (cdr items))
            (map (lambda (x)
                   (cons (car items) x))
                 (list-set (cdr items))))))
(list-set (list 1 2 3))
;; 虽然分析的头头是道，但是结果不对，都是白扯
;; map会忽略null? 但是我们其实是需要空集的
;; 其实还有其他的问题，导致结果不正确
;; 我们得到其实是一个树的结构，还要展平
;; 理论上可以了，我们看看教材怎么处理的

(define (subsets s)
  (if (null? s)
      (list )
      (let ((rest (subsets (cdr s))))
        (append rest (map <??> rest)))))
;; 教材用了append过程来展平了,其他的感觉接近

(define (subsets s)
  (if (null? s)
      (list )
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (x) (cons (car s) x))
                     rest)
                )
        )
  ))
(subsets (list 1 2 3))
;; 返回了空值 ()
;; 检查一下
(map (lambda (x)
       (cons 1 x)
       )
     (list (list 2 3) (list 3) (list )))
(define (subsets s)
  (if (null? s)
      (list )
      (let ((rest (subsets (cdr s))))
        (display rest)
        (display s)
        (display (cdr s))
        (display (car s))
        (newline)
        (define mapped 
          (map (lambda (x)
                 (cons (car s) x)
                 )
               rest))
        (display "mapped ")
        (display mapped)
        (newline)
        (define res (append rest mapped))
        (display "result ")
        (display res)
        (newline)
        res
        )
      )
  )
(subsets (list 1 2 3))
;; 经过检查 空值的时候返回的内容很重要
(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (x) (cons (car s) x))
                     rest)
                )
        )
  ))
(subsets (list 1 2 3))
;; 正常工作

;; 调整元有代码
(define (list-set items)
  (if (null? items)
      (list ())
      (cons (list-set (cdr items))
            (map (lambda (x)
                   (cons (car items) x))
                 (list-set (cdr items))))))
(list-set (list 1 2 3))
;; 很接近答案了
(define (list-set items)
  (if (null? items)
      (list ())
      (append (list-set (cdr items))
            (map (lambda (x)
                   (cons (car items) x))
                 (list-set (cdr items))))))
(list-set (list 1 2 3))
;; 跟答案一致
;; 目前看来抽象分析已经有些眉目了，再接再厉


