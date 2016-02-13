;;; using ltk create a msg box with a button
;;;   when pressed a msg string will display 
;;;   seconds -- incrementing every second
;;; clicking the button will stop the timer

(defpackage :stopwatch
  (:use :common-lisp :ltk)
  (:export :main))

(in-package :stopwatch)

(defun main ()
  (with-ltk ()
    (let ((counter-prog (counter-program))
          (b (make-instance 'button
                            :master nil)
          (txt (make-instance 'message
                              :master nil))
          (setf (command b) (toggle-btn b))
          (pack b)
          (pack txt)

(defun toggle-btn (btn fn)
  (let ((state 'off))
    (lambda (new-state)
      (if (eq state 'on)
          (setf state 'off))
          (setf state 'on))))))
      (if (eq state 'on)
          (setf (text btn) "Start")
          (setf (text btn) "Stop")

(defun callback (btn fn)
  (setf (command btn)
    (lambda () (funcall fn btn))))

(defun polling-fn ()
  (let ((state '()))
    (lambda (new-state)
      

(defun get-current-time ()
  (print '')) 