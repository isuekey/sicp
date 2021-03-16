

(define (monte-carlo trails experiment)
  (define (iter trails-remaining trails-passed)
    (cond ((= trails-remaining 0)
           (/ trails-passed trails))
          ((experiment)
           (iter (- trails-remaining 1) (+ trails-passed 1)))
          (else
           (iter (- trails-remaining 1) trails-passed))))
  (iter trails 0))

;; 正方形[(2,4), (8,10)]: 36
;; 圆形 9pi
;; 9pi/36 = monte-carlo
;; pi = (monte-carlo * 36)/9

(define (random-in-range begin end)
  (+ begin (random (- end begin))))

(define (estimate-pi trails)
  (/ (* 36.0 (monte-carlo trails (validate-point trails))) 9))

(define (validate-point trails)
  (define (in-circle x y)
    (<= (+ (square (- x 5))
           (square (- y 7)))
        9.0))
  (let ((y 4.0)
        (step (/ 6.0 trails)))
    (lambda ()
      (let ((x (random-in-range 2.0 8.0)))
      (begin (set! y (+ y step))
             ;; (display y) (display x)
             ;; (newline)
             (in-circle x y))))))


(define (estimate-pi trails)
  (/ (* 36.0 (monte-carlo trails validate-point2)) 9))
(define (validate-point2)
  (define (in-circle x y)
    (<= (+ (square (- x 5))
           (square (- y 7)))
        9.0))
  (let ((y (random-in-range 4.0 10.0))
        (x (random-in-range 2.0 8.0)))
    (in-circle x y)))

(estimate-pi 100)
(estimate-pi 1000)
(estimate-pi 100000)
;; 总是4
;; 核心是因为 获取随机值的时候有问题，
;; 两次随机编程以圆心为原点的正态分布，见鬼。
;; 所以只能是一个轴向随机，令了一个轴向应该等差增长。
;; 瞎扯蛋，代码写错了。
;; 3^2=36，不知道在想啥。
;; 但是，确实只需要一个方向的随机数就可以累积出圆的面积概率

