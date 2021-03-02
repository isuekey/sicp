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

       
                                                                   
