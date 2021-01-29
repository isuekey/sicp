
;; sinx = 3 * sin(x/3) - 4 (sin(x/3))^3

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))
(sine 12.15)
0.39980345741334
;; sine 12.15 4次（思考），5次（实践）
;; (p (p (p (p (p (sine 0.05))))))
(sine a)
;; 这是一个递归计算过程
;; 空间增长是以3为底a的对数阶。$ O(log_3^a) $
;; 步数与空间增长同阶

