
(define x (list 'a 'b))

(define z1 (cons x x))

(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x) (set-car! (car x) 'wow) x)

#|
E0:
  x: (a b)
  z1: (x x)
  z2: ((a b) a b)
  set-to-wow!: <??>
|#

(set-to-wow! z1)

#|
E1:
  P: E0
  x: z1
  ((set-car! (car x) 'wow) x)，（car x）拿到了 E0：x也就是（a b），被替换成 （wow b）
  返回x
  ((wow b) wow b)
|#

(set-to-wow! z2)

#|
E2:
  p: E0
  x: z2
  ((set-car! (car x) 'wow) x)
  这里 (list 'a 'b)执行两次，是两个数据
  ((wow b) a b)
|#
