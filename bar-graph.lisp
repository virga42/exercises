;; create a program that generates a bar graph
;; the program takes as input the size of the chart
;;  and the values you want to display

(defun print-chart (height width)
  (let ((lines '()))
    (loop for i from 1 to height 
      do (push (concatenate 'string (repeat-string "-" width) "\n") lines))
    (format t lines)))

(defun repeat-string (string n)
  (if (= n 0)
    ""
    (concatenate 'string string (repeat-string string (- n 1)))))

(defun make-bar (height)
  (let ((bar ""))
    (loop for i from 1 to height
      do (setf bar (concatenate 'string "***\n" bar)))
  bar))
