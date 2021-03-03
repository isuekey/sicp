(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))
((frame-coord-map a-frame) (make-vect 0 0))
;; 结果如下
(origin-frame a-frame)


;;46
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)p

(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))
;;47

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (car (cdr frame)))
(define (edge2-frame frame)
  (cdr (cdr frame)))

(define (for-each proc list)
  (if (null? list) list
      (let ()
        (proc (car list))
        (for-each proc (cdr list)))))

(define (segments->painter segement-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (make-segment begin-vect end-vect)
  (cons begin-vect end-vect))

(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (border->painter frame)
  (let ((origin (make-vect 0 0))
        (edge1 (edge1-frame frame))
        (edge2 (edge2-frame frame)))
    ((segments->painter
      (list
       (make-segment origin edge1)
       (make-segment edge1 (add-vect edge1 edge2))
       (make-segment (add-vect edge1 edge2) edge2)
       (make-segment edge2 origin))
     frame)))
(define (diagonal->painter frame)
  (let ((origin (make-vect 0 0))
        (edge1 (edge1-frame frame))
        (edge2 (edge2-frame frame)))
    ((segments->painter
      (list
       (make-segment origin (add-vect edge1 edge2))
       (make-segment edge1 edge2)))
     frame)))

(define (diamond->painter frame)
  (let ((edge1 (edge1-frame frame))
        (edge2 (edge2-frame frame)))
    ((segments->painter
      (list
       (make-segment (scale-vect 0.5 edge1) (add-vect edge1 (scale-vect 0.5 edge2)))
       (make-segment (add-vect edge1 (scale-vect 0.5 edge2)) (add-vect edge2 (scale-vect 0.5 edge1)))
       (make-segment (add-vect edge2 (scale-vect 0.5 edge1)) (scale-vect 0.5 edge2))
       (make-segment (scale-vect 0.5 edge2) (scale-vect 0.5 edge1))))
     frame)))

(define (wave frame)
  (let ((edge1 (edge1-frame frame))
        (edge2 (edge2-frame frame)))
    ((segments->painter
      (list
       (make-segment (add-vect (scale-vect 0.8 edge1) (scale-vect 0 edge2)) (add-vect (scale-vect 0.6 edge1) (scale-vect 0.2 edge2)))
       (make-segment (add-vect (scale-vect 0.6 edge1) (scale-vect 0.2 edge2)) (add-vect (scale-vect 0.7 edge1) (scale-vect 0.3 edge2)))
       (make-segment (add-vect (scale-vect 0.7 edge1) (scale-vect 0.3 edge2)) (add-vect (scale-vect 0.7 edge1) (scale-vect 0.4 edge2)))
       (make-segment (add-vect (scale-vect 0.7 edge1) (scale-vect 0.4 edge2)) (add-vect (scale-vect 0.9 edge1) (scale-vect 0.3 edge2)))
       (make-segment (add-vect (scale-vect 0.9 edge1) (scale-vect 0.3 edge2)) (add-vect (scale-vect 1 edge1) (scale-vect 0.4 edge2)))
       (make-segment (add-vect (scale-vect 1 edge1) (scale-vect 0.6 edge2)) (add-vect (scale-vect 0.9 edge1) (scale-vect 0.7 edge2)))
       (make-segment (add-vect (scale-vect 0.9 edge1) (scale-vect 0.7 edge2)) (add-vect (scale-vect 0.7 edge1) (scale-vect 0.6 edge2)))
       (make-segment (add-vect (scale-vect 0.7 edge1) (scale-vect 0.6 edge2)) (add-vect (scale-vect 0.7 edge1) (scale-vect 0.8 edge2)))
       (make-segment (add-vect (scale-vect 0.7 edge1) (scale-vect 0.8 edge2)) (add-vect (scale-vect 0.4 edge1) (scale-vect 1 edge2)))
       (make-segment (add-vect (scale-vect 0.2 edge1) (scale-vect 1 edge2)) (add-vect (scale-vect 0.5 edge1) (scale-vect 0.6 edge2)))
       (make-segment (add-vect (scale-vect 0.5 edge1) (scale-vect 0.6 edge2)) (add-vect (scale-vect 0 edge1) (scale-vect 0.8 edge2)))
       (make-segment (add-vect (scale-vect 0 edge1) (scale-vect 0.6 edge2)) (add-vect (scale-vect 0.2 edge1) (scale-vect 0.5 edge2)))
       (make-segment (add-vect (scale-vect 0.2 edge1) (scale-vect 0.5 edge2)) (add-vect (scale-vect 0 edge1) (scale-vect 0.4 edge2)))
       (make-segment (add-vect (scale-vect 0 edge1) (scale-vect 0.2 edge2)) (add-vect (scale-vect 0.5 edge1) (scale-vect 0.4 edge2)))
       (make-segment (add-vect (scale-vect 0.5 edge1) (scale-vect 0.4 edge2)) (add-vect (scale-vect 0.6 edge1) (scale-vect 0.3 edge2)))
       (make-segment (add-vect (scale-vect 0.6 edge1) (scale-vect 0.3 edge2)) (add-vect (scale-vect 0.4 edge1) (scale-vect 0.2 edge2)))
       (make-segment (add-vect (scale-vect 0.4 edge1) (scale-vect 0.2 edge2)) (add-vect (scale-vect 0.6 edge1) (scale-vect 0 edge2)))
       )) frame)))

;; 查看答案后发现不正确。描述应该是标准向量作为frame的大小
;; 调整如下
;; 
(define (border->painter frame)
  (let ((origin (make-vect 0 0))
        (edge1 (make-vect 1 0))
        (edge2 (make-vect 0 1)))
    ((segments->painter
      (list
       (make-segment origin edge1)
       (make-segment edge1 (add-vect edge1 edge2))
       (make-segment (add-vect edge1 edge2) edge2)
       (make-segment edge2 origin))
     frame)))
(define (diagonal->painter frame)
  (let ((origin (make-vect 0 0))
        (edge1 (make-vect 1 0))
        (edge2 (make-vect 0 1)))
    ((segments->painter
      (list
       (make-segment origin (add-vect edge1 edge2))
       (make-segment edge1 edge2)))
     frame)))

(define (diamond->painter frame)
  (let ((edge1 (make-vect 1 0))
        (edge2 (make-vect 0 1)))
    ((segments->painter
      (list
       (make-segment (scale-vect 0.5 edge1) (add-vect edge1 (scale-vect 0.5 edge2)))
       (make-segment (add-vect edge1 (scale-vect 0.5 edge2)) (add-vect (scale-vect 0.5 edge1) edge2))
       (make-segment (add-vect (scale-vect 0.5 edge1) edge2) (scale-vect 0.5 edge2))
       (make-segment (scale-vect 0.5 edge2) (scale-vect 0.5 edge1))))
     frame)))

(define (wave frame)
  (let ((edge1 (make-vect 1 0))
        (edge2 (make-vect 0 1)))
    ((segments->painter
      (list
       (make-segment (add-vect (scale-vect 0.8 edge1) (scale-vect 0 edge2)) (add-vect (scale-vect 0.6 edge1) (scale-vect 0.2 edge2)))
       (make-segment (add-vect (scale-vect 0.6 edge1) (scale-vect 0.2 edge2)) (add-vect (scale-vect 0.7 edge1) (scale-vect 0.3 edge2)))
       (make-segment (add-vect (scale-vect 0.7 edge1) (scale-vect 0.3 edge2)) (add-vect (scale-vect 0.7 edge1) (scale-vect 0.4 edge2)))
       (make-segment (add-vect (scale-vect 0.7 edge1) (scale-vect 0.4 edge2)) (add-vect (scale-vect 0.9 edge1) (scale-vect 0.3 edge2)))
       (make-segment (add-vect (scale-vect 0.9 edge1) (scale-vect 0.3 edge2)) (add-vect (scale-vect 1 edge1) (scale-vect 0.4 edge2)))
       (make-segment (add-vect (scale-vect 1 edge1) (scale-vect 0.6 edge2)) (add-vect (scale-vect 0.9 edge1) (scale-vect 0.7 edge2)))
       (make-segment (add-vect (scale-vect 0.9 edge1) (scale-vect 0.7 edge2)) (add-vect (scale-vect 0.7 edge1) (scale-vect 0.6 edge2)))
       (make-segment (add-vect (scale-vect 0.7 edge1) (scale-vect 0.6 edge2)) (add-vect (scale-vect 0.7 edge1) (scale-vect 0.8 edge2)))
       (make-segment (add-vect (scale-vect 0.7 edge1) (scale-vect 0.8 edge2)) (add-vect (scale-vect 0.4 edge1) (scale-vect 1 edge2)))
       (make-segment (add-vect (scale-vect 0.2 edge1) (scale-vect 1 edge2)) (add-vect (scale-vect 0.5 edge1) (scale-vect 0.6 edge2)))
       (make-segment (add-vect (scale-vect 0.5 edge1) (scale-vect 0.6 edge2)) (add-vect (scale-vect 0 edge1) (scale-vect 0.8 edge2)))
       (make-segment (add-vect (scale-vect 0 edge1) (scale-vect 0.6 edge2)) (add-vect (scale-vect 0.2 edge1) (scale-vect 0.5 edge2)))
       (make-segment (add-vect (scale-vect 0.2 edge1) (scale-vect 0.5 edge2)) (add-vect (scale-vect 0 edge1) (scale-vect 0.4 edge2)))
       (make-segment (add-vect (scale-vect 0 edge1) (scale-vect 0.2 edge2)) (add-vect (scale-vect 0.5 edge1) (scale-vect 0.4 edge2)))
       (make-segment (add-vect (scale-vect 0.5 edge1) (scale-vect 0.4 edge2)) (add-vect (scale-vect 0.6 edge1) (scale-vect 0.3 edge2)))
       (make-segment (add-vect (scale-vect 0.6 edge1) (scale-vect 0.3 edge2)) (add-vect (scale-vect 0.4 edge1) (scale-vect 0.2 edge2)))
       (make-segment (add-vect (scale-vect 0.4 edge1) (scale-vect 0.2 edge2)) (add-vect (scale-vect 0.6 edge1) (scale-vect 0 edge2)))
       )) frame)))

;; 有很多冗余计算
;; 按照逆时针顺序定义的edge1，edge2, 可是上面写的好像也对，
;; 因为整个坐标系也搞反了
;; 如果是对称图像问题不大
(define (border->painter frame)
  (let ((origin (make-vect 0 0))
        (edge1 (make-vect 1 0))
        (edge2 (make-vect 0 1)))
    ((segments->painter
      (list
       (make-segment origin edge1)
       (make-segment edge1 (add-vect edge1 edge2))
       (make-segment (add-vect edge1 edge2) edge2)
       (make-segment edge2 origin))
     frame)))
(define (diagonal->painter frame)
  (let ((origin (make-vect 0 0))
        (edge1 (make-vect 1 0))
        (edge2 (make-vect 0 1)))
    ((segments->painter
      (list
       (make-segment origin (add-vect edge1 edge2))
       (make-segment edge1 edge2)))
     frame)))

(define (diamond->painter frame)
  (let ((edge1 (make-vect 1 0))
        (edge2 (make-vect 0 1)))
    ((segments->painter
      (list
       (make-segment (scale-vect 0.5 edge1) (add-vect edge1 (scale-vect 0.5 edge2)))
       (make-segment (add-vect edge1 (scale-vect 0.5 edge2)) (add-vect (scale-vect 0.5 edge1) edge2))
       (make-segment (add-vect (scale-vect 0.5 edge1) edge2) (scale-vect 0.5 edge2))
       (make-segment (scale-vect 0.5 edge2) (scale-vect 0.5 edge1))))
     frame)))
;; 这里是写反了,因为图像不对称了
(define (wave frame)
  (let ((edge1 (make-vect 1 0))
        (edge2 (make-vect 0 1)))
    ((segments->painter
      (list
       (make-segment (add-vect (scale-vect 0.8 edge1) (scale-vect 0 edge2)) (add-vect (scale-vect 0.6 edge1) (scale-vect 0.2 edge2)))
       (make-segment (add-vect (scale-vect 0.6 edge1) (scale-vect 0.2 edge2)) (add-vect (scale-vect 0.7 edge1) (scale-vect 0.3 edge2)))
       (make-segment (add-vect (scale-vect 0.7 edge1) (scale-vect 0.3 edge2)) (add-vect (scale-vect 0.7 edge1) (scale-vect 0.4 edge2)))
       (make-segment (add-vect (scale-vect 0.7 edge1) (scale-vect 0.4 edge2)) (add-vect (scale-vect 0.9 edge1) (scale-vect 0.3 edge2)))
       (make-segment (add-vect (scale-vect 0.9 edge1) (scale-vect 0.3 edge2)) (add-vect (scale-vect 1 edge1) (scale-vect 0.4 edge2)))
       (make-segment (add-vect (scale-vect 1 edge1) (scale-vect 0.6 edge2)) (add-vect (scale-vect 0.9 edge1) (scale-vect 0.7 edge2)))
       (make-segment (add-vect (scale-vect 0.9 edge1) (scale-vect 0.7 edge2)) (add-vect (scale-vect 0.7 edge1) (scale-vect 0.6 edge2)))
       (make-segment (add-vect (scale-vect 0.7 edge1) (scale-vect 0.6 edge2)) (add-vect (scale-vect 0.7 edge1) (scale-vect 0.8 edge2)))
       (make-segment (add-vect (scale-vect 0.7 edge1) (scale-vect 0.8 edge2)) (add-vect (scale-vect 0.4 edge1) (scale-vect 1 edge2)))
       (make-segment (add-vect (scale-vect 0.2 edge1) (scale-vect 1 edge2)) (add-vect (scale-vect 0.5 edge1) (scale-vect 0.6 edge2)))
       (make-segment (add-vect (scale-vect 0.5 edge1) (scale-vect 0.6 edge2)) (add-vect (scale-vect 0 edge1) (scale-vect 0.8 edge2)))
       (make-segment (add-vect (scale-vect 0 edge1) (scale-vect 0.6 edge2)) (add-vect (scale-vect 0.2 edge1) (scale-vect 0.5 edge2)))
       (make-segment (add-vect (scale-vect 0.2 edge1) (scale-vect 0.5 edge2)) (add-vect (scale-vect 0 edge1) (scale-vect 0.4 edge2)))
       (make-segment (add-vect (scale-vect 0 edge1) (scale-vect 0.2 edge2)) (add-vect (scale-vect 0.5 edge1) (scale-vect 0.4 edge2)))
       (make-segment (add-vect (scale-vect 0.5 edge1) (scale-vect 0.4 edge2)) (add-vect (scale-vect 0.6 edge1) (scale-vect 0.3 edge2)))
       (make-segment (add-vect (scale-vect 0.6 edge1) (scale-vect 0.3 edge2)) (add-vect (scale-vect 0.4 edge1) (scale-vect 0.2 edge2)))
       (make-segment (add-vect (scale-vect 0.4 edge1) (scale-vect 0.2 edge2)) (add-vect (scale-vect 0.6 edge1) (scale-vect 0 edge2)))
       )) frame)))
;; 按照常用的坐标系处理，而不是自由心证坐标系
(define (wave frame)
  ((segments->painter
    (list
     (make-segment (make-vect 0 0.8) (make-vect 0.2 0.6))
     (make-segment (make-vect 0.2 0.6) (make-vect 0.3 0.7))
     (make-segment (make-vect 0.3 0.7) (make-vect 0.4 0.7))
     (make-segment (make-vect 0.4 0.7) (make-vect 0.3 0.9))
     (make-segment (make-vect 0.3 0.9) (make-vect 0.4 1.0))
     (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.9))
     (make-segment (make-vect 0.7 0.9) (make-vect 0.6 0.7))
     (make-segment (make-vect 0.6 0.7) (make-vect 0.8 0.7))
     (make-segment (make-vect 0.8 0.7) (make-vect 1.0 0.4))
     (make-segment (make-vect 1.0 0.2) (make-vect 0.6 0.5))
     (make-segment (make-vect 0.6 0.5) (make-vect 0.8 0.0))
     (make-segment (make-vect 0.6 0.0) (make-vect 0.5 0.2))
     (make-segment (make-vect 0.5 0.2) (make-vect 0.4 0.0))
     (make-segment (make-vect 0.2 0.0) (make-vect 0.4 0.5))
     (make-segment (make-vect 0.4 0.5) (make-vect 0.3 0.6))
     (make-segment (make-vect 0.3 0.6) (make-vect 0.2 0.4))
     (make-segment (make-vect 0.2 0.4) (make-vect 0.0 0.6))
     (make-segment (make-vect 0.8 0) (make-vect 0.6 0.2))
     (make-segment (make-vect 0.8 0) (make-vect 0.6 0.2))
     (make-segment (make-vect 0.8 0) (make-vect 0.6 0.2))
     )) frame))
