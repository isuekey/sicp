
(defun filtered-accumulate (combiner nullvalue term a next b filter)
  (defun loop-it (a result)
    (if (> a b)
        result
      (if (funcall filter a)
          (loop-it (funcall next a)
                   (funcall combiner (funcall term a) result))
        (loop-it (funcall next a) result))))
  (loop-it a nullvalue))

(defun id (x) x)


(defun even? (n) (= (mod n 2) 0))
(defun odd? (n) (= (mod n 2) 1))

(print (filtered-accumulate '+ 0 'id 1 '1+ 10 'odd?))

(defun next (div)
  (if (= (mod div 2) 0)
      (+ div 1)
    (+ div 2)))
(defun smallest-divisor (n)
  (smallest-divisor-it n 2))
(defun smallest-divisor-it (n div)
  (cond ((> (* div div) n) n)
        ((= (mod n div) 0) div)
        (t (smallest-divisor-it n (next div)))))
(defun prime? (n)
  (= n (smallest-divisor n)))


(print (filtered-accumulate '+ 0 'id 2 '1+ 10 'prime?))

(defun mgcd (a b)
  (if (= b 0)
      a
    (mgcd b (mod a b))))

(defun mgcd? (a) (= (mgcd a 10) 1))
(print (filtered-accumulate '+ 0 'id 1 '1+ 10 'mgcd?))
