
;; 向量v = (v_i)，矩阵 m = (m_ij)表示为向量（矩阵行）的序列
;; 这里很奇怪 数学上喜欢用列向量表示矩阵，程序上习惯用行向量表示矩阵

(define m (list (list 1 2 3 4)
                (list 4 5 6 6)
                (list 6 7 8 9)))
;; 这样描述矩阵后，这种习惯原因自然就很清楚了。这样最容易让人阅读代码
;; 至于过程实现中存在的与数学公式细微差异，会因为实现过程相对苦难而不明显
;; 

(define (accumulate op initial items)
  (if (null? items) initial
      (op (car items)
          (accumulate op initial (cdr items)))))

;; 这里的map是Scheme提供的标准map，接收相同长度的表作为参数，返回相同长度的表
;; 之上的元素是每个参数表的对应元素在操作后的结果
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(dot-product (list 2 3 5) (list 1 2 3))
23
;; 点积 因为结果是一个数，两个向量失去维度特征，所以称为点积
(define (accumulate-n op init seqs)
  (if (null? (car seqs)) ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons () mat))
(transpose m)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row))m)))
(matrix-*-matrix m (transpose m))
