
(defun split (fst snd)
  (defun split-it (painter n)
    (if (= n 0)
        painter
      (let ((smaller (split painter (- n 1))))
        (funcall fst painter (funcall snd smaller smaller)))))
  (lambda (painter n)
    (split-it painter n)))

(defun right-split (split beside below))
(defun up-split (split below beside))
