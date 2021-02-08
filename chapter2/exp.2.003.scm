
;;
;; 描述矩形，计算周长与面积
;; 简单思路 1, 左上顶点 宽 高 旋转角度; 2, 一条边与这条边上的高（顺时针方向）
;;

(define (print-point p)
  (newline)
  (display "(") (display (x-point p)) (display ",") (display (y-point p)) (display ")"))


(define (perimeter rect)
  (* (+ (width rect) (height rect)) 2))
(define (area rect)
  (* (width rect) (height rect)))
(define (print-rect rect)
  (newline)
  (display (start-point rect)) (newline)
  (display "width:") (display (width rect)) (display ", height:") (display (height rect)) (newline)
  (display "primeter:") (display (perimeter rect)) (display ", area:") (display (area rect))
  )

(define (make-rect point width height angle)
  (cons (cons point angle) (cons width height)))
(define (start-point rect)
  (car (car rect)))
(define (width rect)
  (car (cdr rect)))
(define (height rect)
  (cdr (cdr rect)))
(define rect1 (make-rect (cons 0 5) 2 4 0))
(print-rect rect1)

(define (make-rect start-point end-point height)
  (cons (cons start-point end-point) height))
(define (start-point rect)
  (car (car rect)))
(define (width rect)
  (let ((start (start-point rect))
        (end (cdr (car rect))))
    (sqrt (+ (expt (- (car end) (car start)) 2)
             (expt (- (cdr end) (cdr start)) 2)))
    ))
(define (height rect)
  (cdr rect))
(define rect2 (make-rect (cons 0 5) (cons 2 5) 4))
(print-rect rect2)



