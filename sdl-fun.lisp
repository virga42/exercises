(ql:quickload :lispbuilder-sdl)

(defpackage :sdl-fun
  (:use :common-lisp :sdl)
  (:export :main))

(in-package :sdl-fun)

(defparameter *window-width* 320)
(defparameter *window-height* 320)

(defun decrement-helper (n)
  (if (<= n 0)
      0
      (decf n)))

(defun increment-helper (n val)
  (if (>= n val)
      val
      (incf n)))

(defun zoom-toggle (zoom-state)
  (if (eq :off zoom-state)
      :on
      :off))

(defun main ()
  (sdl:with-init ()
    (let ((x 160)
	  (y 160)
	  (*random-color* (sdl:color :r (random 255) :g (random 255) :b (random 255)))
	  (zoom-box :off))
      (sdl:window *window-width* *window-height*)
      (sdl:enable-key-repeat 200 10)
      (sdl:with-events ()
	(:quit-event () t)
	(:key-down-event (:key key)
			 (cond
			   ((sdl:key= key :sdl-key-up) (setf y (decrement-helper y)))
			   ((sdl:key= key :sdl-key-down) (setf y (increment-helper y *window-height*)))
			   ((sdl:key= key :sdl-key-left) (setf x (decrement-helper x)))
			   ((sdl:key= key :sdl-key-right) (setf x (increment-helper x *window-width*)))
			   ((sdl:key= key :sdl-key-space) (setf zoom-box (zoom-toggle zoom-box)))))
	(:idle ()
	       (sdl:clear-display sdl:*black*)
	       (sdl:draw-vline x 0 360 :color *random-color*)
	       (sdl:draw-hline 0 360 y :color *random-color*)
	       (when (eq :on zoom-box)
		 (sdl:draw-box (sdl:rectangle-from-midpoint-* x y 20 20) :stroke-color *random-color* :clipping t))
	       (sdl:update-display))))))
		
