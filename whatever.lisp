(print "Hello World!")

(defun greeting (name)
	(format t "Hello ~a!" name))

(defun goodbye (name)
	(format t "Goodbye, ~a." name))

(greeting "George")
(greeting "Jim")

(defun goodbye-alex ()
	(greeting "Alex"))

(defparameter goodbye-alexpar (lambda (goodbye "Alex")))
