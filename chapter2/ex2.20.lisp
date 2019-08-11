

(defun same-parity (&rest x)
  (defun same-parity-it (first args result)
    (let ((remain (mod first 2))
          (next (car args)))
      (if (null args)
          result
        (if (= (mod next 2) remain)
            (same-parity-it first (cdr args) (cons next result))
          (same-parity-it first (cdr args) result)))))
  (if (null x)
      x
    (reverse (same-parity-it (car x) (cdr x) (list (car x))))))


(print (same-parity 4 5 7 8 10))

