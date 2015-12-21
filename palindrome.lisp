; Determine whether a word is a palindrome
; It's not fair to use reverse

(defun stack-push (stack item)
	(cons item stack))

(defun stack-pop (stack)
	(setf tmp (car stack))
	(setf (car stack) (cadr stack))
	(setf (cdr stack) (cddr stack))
	stack)

(defun list-to-stack (lst)
	(list-to-stack-helper lst '()))

(defun list-to-stack-helper (lst stack)
	(if (eq nil lst)
		stack
		(list-to-stack-helper (cdr lst) (stack-push stack (car lst)))))

(defun string-to-list (s)
	(setf char-lst (coerce s 'list)))

(defun palindrome-checker (lst reverse-lst)
	(cond ((or (eq nil lst) (string= (car lst) nil)) (print "true"))
		((not (string= (car lst) (car reverse-lst))) (print "false"))
		(t (palindrome-checker (stack-pop lst) (stack-pop reverse-lst)))))

(defparameter word-lst (string-to-list "noxinnixon"))

(palindrome-checker word-lst (list-to-stack word-lst))