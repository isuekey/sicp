
;;; (/ h 3)(+ y0 4y1 2y2 ... 2yn2 4yn1 yn)
;;; yk = (f (+ a (* h k)))

(defun simpson-sum (a b cal step)
  (defun real-sum (range evenstep)
    (* (/ (/ range evenstep) 3) (+ (funcall cal a) (funcall cal b) (range-sum (/ range evenstep) (- evenstep 1)))))
  (defun range-sum (steprange oddstep)
    (defun item (itemIndex)
      (if (= (mod itemIndex 2) 1)
          (* 4 (funcall cal (+ a (* steprange itemIndex))))
        (* 2 (funcall cal (+ a (* steprange itemIndex))))))
    (defun item-sum (odds)
      (if (= odds 0)
          0
        (+ (item odds) (item-sum (- odds 1)))))
    (item-sum oddstep))
  (real-sum (- b a) (* step 2)))

(defun cube (a) (* a a a))

(print (simpson-sum 0.0 1.0 'cube 100))
(print (simpson-sum 0.0 1.0 'cube 1000))


