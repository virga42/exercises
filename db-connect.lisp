(in-package :cl-mysql)

(defun run-qry ()
  (let* ((connection
	  (connect
	   :host "epdb3.epnet.com"
	   :database-name "ese_operational_efficiency"
	   :username "root"
	   :password "root"))
	  (sql "SELECT * FROM ESE_Snapshot WHERE idtype = 'US' and datestamp = '2016-01-26';"))
    (query sql :store t)))
