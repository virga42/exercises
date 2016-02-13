((defpackage lisp-utils
  (:use :common-lisp)
  (:export *make-list)))

(defmacro my-make-listm (n &body body)
  `(loop repeat ,n 
      collecting ,@body))

(defun my-make-listf (n &key initial-element initial-function)
  (let* ((start (if (null initial-element) 1 initial-element))
         (end (+ start (1- n))))
  (loop for i from start to end 
    collecting (funcall initial-function))))

(defun my-counter-fn ()
  (let ((x 0))
    (lambda ()
      (setf x (+ x 1))
      x)))

(setf my-counter (my-counter-fn))

(my-make-listm 10 (funcall my-counter))