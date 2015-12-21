(defun number-incrementor (i)
  (let ((n 0))
    (lambda ()
      (setf n (+ i n)))))