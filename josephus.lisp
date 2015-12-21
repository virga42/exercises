;; Josephus problem
;; People are standing in a circle waiting to be executed. Counting begins at a 
;;   specified point in the circle and proceeds around the circle in a specified 
;;   direction. After a specified number of people are skipped, the next person is 
;;   executed. The procedure is repeated with the remaining people, starting with 
;;   the next person, going in the same direction and skipping the same number of 
;;   people, until only one person remains, and is freed.

;; The problem — given the number of people, starting point, direction, and number 
;;   to be skipped — is to choose the position in the initial circle to avoid 
;;   execution.

(defun josephus-solver (lst i)
	(if (eq (length lst) 1)
		lst
		(josephus-solver (remove-prisoner lst i) i)))

(defun remove-prisoner (lst i)
	"returns a list of prisoners minus killed prisoners and starting with next in line"
	(let* ((l (length lst)) (pos (next-pos l i)))
		(print (nth pos lst)) 
		(append (rest-nth (+ pos 1) lst) (up-to-nth pos lst))))

(defun next-pos (len i)
	(mod i len))

(defun make-lst (n)
	"return a list n items long"
	(loop for i from 1 to n collect i))

(defun rest-nth (index lst)
	"return a list of items from (and including) index to end of list"
	(if (= index 0)
		lst
		(rest-nth (- index 1) (cdr lst))))

(defun up-to-nth (index lst)
	"return a list of items up to (and not including) index in original list"
	(if (= index 0)
		'()
		(cons (car lst) (up-to-nth (- index 1) (cdr lst)))))

(defun josephus-helper (n start-pos &optional (direction 0))
	(let ((index (- start-pos 1)) (prisoners (make-lst n)))
	(cond ((= direction 0)
			(append (rest-nth index prisoners) (up-to-nth index prisoners)))
		  ((= direction 1)
			(append (reverse (up-to-nth (+ index 1) prisoners)) (reverse (rest-nth (+ index 1) prisoners)))))))