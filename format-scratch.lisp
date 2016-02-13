(defparameter *date* "2016-01-26")

(defun flatten-list (lst)
  (if (null lst)
    '()
    (if (atom (car lst))
      (cons (car lst) (flatten-list (cdr lst)))
      (append (flatten-list (car lst)) (flatten-list (cdr lst))))))

(defun prep-values (list)
  (let* ((workitem (first list))
	 (predecessor-list (second list))
	 (successor-list (third list))
	 (values-str (list (single-value workitem workitem *date* "self"))))
    (when predecessor-list
      (setf values-str (append (values-lst workitem 
			  predecessor-list 
			  *date* 
			  "predecessor")
	      values-str)))
    (when successor-list
      (setf values-str (append (values-lst workitem
			  successor-list
			  *date*
			  "successor")
	      values-str)))
    values-str))

(defun collect-values (workitems-list)
  (let ((collection (loop for item in workitems-list
		       collecting (prep-values item))))
    (delimit-values (flatten-list collection))))

(defun single-value (workitem rel-workitem date rel-type)
  (let ((str #?"\('${workitem}', '${date}', '${rel-workitem}', '${rel-type}'\)"))
    str))

(defun values-lst (workitem lst date rel-type)
  (let ((values-str '()))
	(loop for item in lst do
	     (push (single-value workitem item date rel-type) values-str))
	values-str))

(defun delimit-values (item)
  (format t "~{~a~^, ~}" item))

;; (setf *date* "2016-02-26")

;; (defun format-test (a)
;;   (format t "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, ~:;, ~]~}~]~}" a)) 

