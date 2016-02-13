;;;
;;; Get Predecessor and Successor nodes
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-interpol)
  (ql:quickload :cl-mysql))

(defpackage :workitem-timeline
  (:use :common-lisp :cl-interpol :cl-mysql)
  (:export :main))

(in-package :workitem-timeline)

(defparameter first-time-loadp t)
(defparameter *snap* "~/bottega/exercises/PredecessorsSuccessorsSnap.csv")
(defparameter *workitems* "~/bottega/exercises/formattedid.csv")
(defparameter *date* "2016-01-26")

(defparameter *host* "epdb3.epnet.com")
(defparameter *database* "ese_operational_efficiency")
(defparameter *user* "root")
(defparameter *pass* "root")

(cl-interpol:ENABLE-INTERPOL-SYNTAX)

; list functions

(defun flatten-list (lst)
  (if (null lst)
    '()
    (if (atom (car lst))
      (cons (car lst) (flatten-list (cdr lst)))
      (append (flatten-list (car lst)) (flatten-list (cdr lst))))))

(defun make-set (lst)
  (cond 
    ((null lst) '())
    ((member (car lst) (cdr lst)) (make-set (cdr lst)))
    (t (cons (car lst) (make-set (cdr lst))))))

(defun reverse-pairs (lst)
  (loop for i in lst collect (reverse i)))

; file import functions

(defun split-on-comma (string)
  (loop for i = 0 then (1+ j)
       as j = (position #\Comma string :start i)
       collect (subseq string i j)
       while j))

(defun trim-ws (string)
  (string-trim '(#\Space #\Newline #\Backspace #\Tab 
        #\Linefeed #\Page #\Return #\Rubout)
	       string))

(defun read-delimited-file (filename)
  (let ((lines '()) (in (open filename)))
    (when in 
      (loop for line = (read-line in nil)
	 while line do
	   (let* ((split-line (split-on-comma line))
		  (parent (intern (first split-line)))
		  (child  (intern (third split-line))))
	     (push (list parent child) lines))))
      (close in)
      (reverse lines)))

(defun read-file-into-list (filename)
  (let ((lines '()) (in (open filename)))
    (when in
      (loop for line = (read-line in nil)
	   while line do
	   (let ((ticket (intern (trim-ws line))))
	     (push ticket lines))))
    (close in)
    lines))

;; database functions

(defun read-query-into-list (datestamp)
  (let ((sql-string #?"SELECT formattedid FROM ESE_Snapshot_All
WHERE datestamp = '${datestamp}'
and idtype = 'US'
and componentteam <>'';"))
  (cl-mysql:connect :host *host* 
		    :database *database* 
		    :user *user* 
		    :password *pass*)
  (let ((result (cl-mysql:query sql-string :store t)))
	(cl-mysql:disconnect)
	(mapcar #'(lambda (x) (intern (car x))) (caar result)))))

(defun read-predecessor-table (datestamp)
  (let ((sql-string #?"SELECT preformattedid, sucformattedid
FROM ESE_Predecessor_Successor
WHERE datestamp = '${datestamp}';"))
    (cl-mysql:connect :host *host* 
		      :database *database* 
		      :user *user* 
		      :password *pass*)
  (let ((result (cl-mysql:query sql-string :store t)))
    (cl-mysql:disconnect)
    (loop for item in (caar result) collecting
	 (mapcar #'(lambda (x) (intern x)) item)))))

(defun insert-into-workitem-timeline (values)
  (let ((sql-string #?"INSERT INTO workitem_timeline
\(formattedid, date_context, linked_formattedid, link_type\)
VALUES
${values};"))
  (cl-mysql:connect :host *host* 
  		    :database *database* 
  		    :user *user* 
  		    :password *pass*)
  (unwind-protect (cl-mysql:query sql-string)
    (cl-mysql:disconnect))))

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
  (format nil "~{~a~^, ~}" item))

;; hash functions

(defun populate-hash (kvp-lst h)
  (dolist (kvp kvp-lst)
    (let ((k (get-key kvp))
	  (v (get-value kvp)))
      (setf (gethash k h) (cons v (gethash k h))))))

(defun get-value (key-value-pair)
  (second key-value-pair))

(defun get-key (key-value-pair)
  (first key-value-pair))

(defun get-descendents (hash key items-seen)
  (let ((children (gethash key hash)))
    (cond 
      ((null children) '())
      (t (loop for child in children when (not (member child items-seen)) 
	    collecting (cons child (get-descendents hash child 
						    (push child items-seen))))))))

;; main function

(defun main (&key (source 'db) snap-filename tickets-filename)
  (let* ((predecessor-hash (make-hash-table))
	 (successor-hash (make-hash-table))
	 (predecessor-tickets (if (equalp source 'db)
				  (read-predecessor-table *date*)
				  (read-delimited-file snap-filename)))
	 (successor-tickets (reverse-pairs predecessor-tickets))
	 (workitems (if (equalp source 'db)
			(read-query-into-list *date*)
			(read-file-into-list tickets-filename))))
    (populate-hash predecessor-tickets predecessor-hash)
    (populate-hash successor-tickets successor-hash)
    (let* ((insert-values-lst (loop for workitem in workitems 
				 collect (cons workitem 
					       (cons (flatten-list 
						      (get-descendents predecessor-hash workitem nil))
						     (cons (flatten-list 
							    (get-descendents successor-hash workitem nil)) nil)))))
	   (collection (collect-values insert-values-lst)))
      (if (equalp source 'db)
      	  (insert-into-workitem-timeline collection) 
      	  insert-values-lst)
)))

