(defpackage :button-01
  (:use :common-lisp :ltk)
  (:export :main :text-incrementor))

(in-package :button-01)

 (defun text-incrementor ()
  (let ((txt 0))
    (lambda () 
      (setf txt (1+ txt)))))

(defun main ()
  (with-ltk ()
  (let ((b (make-instance 'button 
                         :master nil
                         :text "Press me"
                         ))
         (button-txt (text-incrementor)))
    (setf (command b) (lambda () (setf (text b) button-txt)))
    (pack b))))