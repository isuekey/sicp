

;;; 通过直角三角形顶点表示 左上角 右上角 右下角 三个顶点
(defun make-segment (p1 p2)
  (cons p1 p2))
(defun start-segment (seg)
  (car seg))
(defun end-segment (seg)
  (cdr seg))

(defun make-diag-rect (p1 p2 p3)
  (cons (make-segment p1 p3) p2))
(defun make-point (x y)
  (cons x y))
(defun x-point (p)
  (car p))
(defun y-point (p)
  (cdr p))

(defun square (x) (* x x))

(defun length-segment (seg)
  (let ((p1 (start-segment seg))
        (p2 (end-segment seg)))
    (sqrt (+ (square (- (x-point p1) (x-point p2)))
             (square (- (y-point p1) (y-point p2)))))))

(defun left-top-point (rect)
  (start-segment (car rect)))
(defun right-top-point (rect)
  (cdr rect))
(defun right-bottom-point (rect)
  (end-segment (car rect)))

(defun top-segment (rect)
  (make-segment (left-top-point rect) (right-top-point rect)))
(defun right-segment (rect)
  (make-segment (right-bottom-point rect) (right-top-point rect)))

(defun perimeter (rect)
  (* (+ (length-segment (top-segment rect)) (length-segment (right-segment rect))) 2))

(defun area (rect)
  (* (length-segment (top-segment rect)) (length-segment (right-segment rect))))

(defvar ptl (make-point 0 10))
(defvar ptr (make-point 5 10))
(defvar ptb (make-point 5 0))
(defvar mrect (make-diag-rect ptl ptr ptb))
(print (perimeter mrect))
(print (area mrect))


;;; 通过顶点与相邻两边的矢量方式表示 左上角顶点 上边 左边
;;; 一种思路是直接转出 make-diag-rect
(defun make-vec-rect (p1 vtop vleft)
  (let ((p2 (make-point (+ (x-point p1) (car vtop)) (+ (y-point p1) (cdr vtop))))
        (p3 (make-point (+ (x-point p1) (car vtop) (car vleft))
                        (+ (y-point p1) (cdr vtop) (cdr vleft)))))
    (make-diag-rect p1 p2 p3)))

(defvar vtop (cons 5 0))
(defvar vleft (cons 0 -10))
(defvar mvrect (make-vec-rect ptl vtop vleft))

(print (perimeter mvrect))
(print (area mvrect))
