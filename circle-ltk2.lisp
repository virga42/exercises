(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :ltk)
  (ql:quickload :local-time))

(defpackage :planets2
  (:use :common-lisp :ltk)
  (:export :main))

(in-package :planets2)

(defparameter *update-interval* 10)
(defparameter x-origin 300)
(defparameter y-origin 300)

(defun rad (degrees)
  (* pi (/ degrees 180.0)))

(defun x-coord (degree)
  (cos (rad degree)))

(defun y-coord (degree)
  (sin (rad degree)))

(defun coord-multiplier (coord multiplier)
  (* coord multiplier))

(defun calc-radius (diameter)
  (/ diameter 2.0))

(defun calculate-coords (diameter degree multiplier)
  (let* ((radius (calc-radius diameter))
	(x0 (round (+ x-origin (coord-multiplier (x-coord degree) multiplier) (- radius))))
	(x1 (+ x0 diameter))
	(y0 (round (+ y-origin (coord-multiplier (y-coord degree) multiplier) (- radius))))
	(y1 (+ y0 diameter)))
    (list x0 y0 x1 y1)))

(defun milliseconds-to-nanoseconds (n)
  (* n 1000000))

(defclass planet ()
   ((image-item :accessor image-item)
    (degree :accessor degree :initform 0)
    (degree-increment :accessor degree-increment :initarg :degree-increment :initform 2)
    (diameter :accessor diameter :initarg :diameter :initform 10)
    (multiplier :accessor multiplier :initarg :multiplier :initform 5)
    (interval :accessor interval :initarg :interval :initform 10)
    (coords :accessor coords)))

(defgeneric orbit (planet canvas))
(defmethod orbit ((p planet) canvas)
  (let* ((new-degree (if (>= (degree p) 360)
			0
		        (+ (degree p) (degree-increment p))))
	 (new-coords (calculate-coords (diameter p) (degree p) (multiplier p))))
    (setf (degree p) new-degree)    (setf (coords p) new-coords)
    (set-coords canvas (image-item p) new-coords)))


(defgeneric draw-planet (planet canvas))33(defmethod draw-planet ((p planet) canvas)
  (let* ((coords (calculate-coords (diameter p) (degree p) (multiplier p))))
    (setf (coords p) coords)
    (setf (image-item p) (make-oval canvas (first coords)
				    (second coords)
				    (third coords)
				    (fourth coords)))))

(defclass chord ()
  ((image-item :accessor image-item)))

(defgeneric draw-chord (chord planet planet canvas))
(defmethod draw-chord ((a-chord chord) (first-planet planet) (second-planet planet) canvas)
  (setf (image-item a-chord) (make-line canvas (list (/ (+ (first (coords first-planet))
							     (third (coords first-planet))) 2)
						     (/ (+ (second (coords first-planet))
							     (fourth (coords first-planet))) 2)
						     (/ (+ (first (coords second-planet))
							     (third (coords second-planet))) 2)
						     (/ (+ (second (coords second-planet))
							     (fourth (coords second-planet))) 2)))))

(defun main ()
  (with-ltk ()
    (let ((outer-planet (make-instance 'planet :diameter 25 :interval 25 :multiplier 200))
	  (inner-planet (make-instance 'planet :multiplier 125 :degree-increment 5))
	  (a-chord (make-instance 'chord))
	  (c (make-instance 'canvas :width 800 :height 600))
      (continuep t))
      (focus c)
      (bind c "<KeyPress-q>" (lambda (event) (exit-wish)))
      (pack c)
      (draw-planet outer-planet c)
      (configure (image-item outer-planet) :fill :blue)
      (draw-planet inner-planet c)
      (configure (image-item inner-planet) :fill :red)
      (loop while continuep do
	   (labels ((ticker-generator ()
		      (let ((milliseconds-per-tick 33)
			    (time-of-last-tick (local-time:now)))
			(lambda ()
			  (let ((now (local-time:now)))
			    (if (local-time:timestamp<
				 (local-time:timestamp+ time-of-last-tick
							(milliseconds-to-nanoseconds 
							 milliseconds-per-tick) :nsec) now)
				(setf time-of-last-tick now)
				nil))))))
	     (let ((my-ticker (ticker-generator)))
	       (do ((i 0 (1+ i)))
		   ((< i 0))
		 (when (funcall my-ticker)
		     (orbit outer-planet c)
		     (orbit inner-planet c)
		   (when (and (> (degree inner-planet) 0) (= 0 (mod (degree inner-planet) 10)))
		     (draw-chord a-chord outer-planet inner-planet c))
		   (process-events)
))))))))


