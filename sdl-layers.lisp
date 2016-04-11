(ql:quickload :lispbuilder-sdl)

(defpackage :sdl-layers
  (:use :common-lisp :sdl)
  (:export :main))

(in-package :sdl)

(defun main ()
  (sdl:with-init ()
    (sdl:window 300 300)
    (let ((bottom-surface (create-surface 300 300))
	  (top-surface (create-surface 300 300 :alpha 0)))
      (sdl:with-events ()
	(:quit-event () t)
	
	(:idle ()
	 (draw-surface bottom-surface)
	 (draw-surface top-surface)
	 (draw-box-* 100 100 50 50 :color *blue*)
	 (fill-surface *green* :surface bottom-surface)
	 (fill-surface *red* :surface top-surface)
	 (update-display))))))
