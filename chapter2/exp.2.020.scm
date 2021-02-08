
;;返回与第一个参数相同奇偶性的表
(define nums1 (list 1 2 3 4 5 6 7 8 9 10 11))
(define nums2 (list 2 3 4 5 6 7 8 9 10 11 12 13))

(define (same-parity first . other)
  (if (null? other)
      other
      (let ((remain (remainder first 2))
            (check (car other))
            (subs (cdr other)))
        (if (= remain (remainder check 2))
            (cons check (same-parity first subs))
            (same-parity first subs)))))
;; (same-parity nums1)
;; (same-parity nums2)
(same-parity 1 nums1)
(same-parity 2 nums2)
;; 报错了 两个原因
;; 1.题目理解不对实现结果跟要求对不上
;; 2.递归调用的时候参数不对

(define (same-parity nums)
  (define (iter remainder-check subs)
    (let ((first (car subs))
          (subsubs (cdr subs)))
      (cond ((null? subsubs)
             (cond ((not first) subsubs)
                   ((= remainder-check (remainder first 2)) (cons first ()))))
            ((= remainder-check (remainder first 2))
             (cons first (iter remainder-check subsubs)))
            (else (iter remainder-check subsubs)))))
  (if (or (null? nums) (null? (cdr nums)))
      ()
      (iter (remainder (car nums) 2) (cdr nums))))
(same-parity nums1)
(same-parity nums2)

;; 有错误 空值的时候应该跳过而不是继续处理
;;
(define nums1 (list 1 2 3 4 5 6 7 8 9 10 11))
(define nums2 (list 2 3 4 5 6 7 8 9 10 11 12 13))
(define (same-parity nums)
  (define (iter subs first)
    (cond ((null? subs) subs)
          ((= (remainder first 2) (remainder (car subs) 2))
           (cons (car subs) (iter (cdr subs) first)))
          (else (iter (cdr subs) first))))
  (if (or (null? nums) (null? (cdr nums))) ()
      (let ((first (car nums)))
        (iter (cdr nums) first)))
  )
(same-parity nums1)
(same-parity nums2)

;; 按照题目完成

(define (same-parity first . other)
  (define (iter subs)
    (cond ((null? subs) subs)
          ((= (remainder first 2) (remainder (car subs) 2))
           (cons (car subs) (iter (cdr subs))))
          (else (iter (cdr subs)))))
  (iter other))
(same-parity 1 2 3 4 5 6 7 8 9 10 11)
(same-parity 2 3 4 5 6 7 8 9 10 11 12 13)
;; 这是递归计算
;; 在给出一个迭代计算
(define (same-parity first . other)
  (define (iter subs res)
    (cond ((null? subs) res)
          ((= (remainder first 2) (remainder (car subs) 2))
           (iter (cdr subs) (cons (car subs) res)))
          (else (iter (cdr subs) res))))
  (iter other ()))
(same-parity 1 2 3 4 5 6 7 8 9 10 11)
(same-parity 2 3 4 5 6 7 8 9 10 11 12 13)
;; 得到一个反序的
(define (reverse items)
  (define (iter subs reversed)
    (if (null? subs)
        reversed
        (iter (cdr subs) (cons (car subs) reversed))))
  (iter items ()))
(define (same-parity first . other)
  (define (iter subs res)
    (cond ((null? subs) res)
          ((= (remainder first 2) (remainder (car subs) 2))
           (iter (cdr subs) (cons (car subs) res)))
          (else (iter (cdr subs) res))))
  (reverse (iter other ()))
  )
(same-parity 1 2 3 4 5 6 7 8 9 10 11)
(same-parity 2 3 4 5 6 7 8 9 10 11 12 13)

