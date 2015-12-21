; write two functions 
; 1 list of random numbers of length n
; 2 dedupes list
; function will take two parameters; max num, num of random numbers

(defun make-list-random-numbers (n max-num)
	(if (= n 0)
		'()
		(cons (random max-num) (make-list-random-numbers (- n 1) max-num))))

(defun dedupe-lst (lst)
	(let ((deduped-lst '()))
		(dolist (n lst)
			(if (eq (find n deduped-lst) nil)
				(push n deduped-lst)))
	deduped-lst)) 