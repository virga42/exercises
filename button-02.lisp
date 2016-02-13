(defpackage :button-02
  (:use :common-lisp :ltk)
  (:export :main :text-incrementor))

(in-package :button-01)

 (defun text-incrementor ()
  (let ((txt 0))
    (lambda () 
      (setf txt (1+ txt)))))

;; first attempt -- acceptable but not ideal
(defun main ()
  (with-ltk ()
    (let* ((root (make-instance 'frame))
           (frm (make-instance 'frame :master root)))
      (pack root :side :top)
      (pack frm :side :top)

      (dolist (number '(1 2 3 4 5))
        (let ((btn (make-instance 'button
                                  :master frm
                                  :text number
                                  ))
               (button-txt (text-incrementor)))
          (setf (command btn) (lambda () (setf (text btn) (funcall button-txt))))
          (pack btn :side :top :anchor :w))))))

;; this is the favorable solution
(defun main2 ()
  (with-ltk ()
    (let* ((root (make-instance 'frame))
           (frm (make-instance 'frame :master root))
           (btns (loop for i from 1 to 30
                    collect 
                      (make-instance 'button
                                      :master frm
                                      :text "mash me"
                                      ))))
      (pack root :side :top)
      (pack frm :side :top)

      (dolist (btn btns)
        (let ((button-txt (text-incrementor)))
        (setf (command btn) (lambda () (setf (text btn) (funcall button-txt))))
        (pack btn))))))

;; intentionally broken -- safe to ignore
(defun main3 ()
  (with-ltk ()
    (let* ((root (make-instance 'frame))
           (frm (make-instance 'frame :master root))
           (btns (loop for i from 1 to 30
                    collect 
                      (make-instance 'button
                                      :master frm
                                      :text "mash me"
                                      ))))
      (pack root :side :top)
      (pack frm :side :top)
        
        (dolist (btn btns)
            do (let ((button-txt (text-incrementor)))
                  (setf (command btn) (lambda () (setf (text btn) (funcall button-txt))))
                  (pack btn))
            ))))



          
