
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (below (flip-vert quarter) quarter)))
      (beside (flip-horiz half) half))))

;; 增加笑脸 a
(define (wave frame)
  ((segments->painter
    (list
     (make-segment (make-vect 0 0.8) (make-vect 0.2 0.6))
     (make-segment (make-vect 0.2 0.6) (make-vect 0.3 0.7))
     (make-segment (make-vect 0.3 0.7) (make-vect 0.4 0.7))
     (make-segment (make-vect 0.4 0.7) (make-vect 0.3 0.9))
     (make-segment (make-vect 0.3 0.9) (make-vect 0.4 1.0))
     (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.9))
     (make-segnment (make-vect 0.7 0.9) (make-vect 0.6 0.7))
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
     (make-segment (make-vect 0.35 0.9) (make-vect 0.45 0.9)) ;; left eye
     (make-segment (make-vect 0.55 0.9) (make-vect 0.65 0.9)) ;; right eye
     (make-segment (make-vect 0.4 0.8) (make-vect 0.5 0.75)) ;; mouth
     (make-segment (make-vect 0.5 0.75) (make-vect 0.6 0.8))
     )) frame))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;; b c 不明白干啥
(define (corner-split painter n)
  (if (= n 0)
      painter
      (beside (below painter (up-split painter (n 1)))
              (below (right-split painter (n 1)) (corner-split painter (- n 1))))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180 identity flip-horiz)))
    (combine4 (corner-split painter n))))

