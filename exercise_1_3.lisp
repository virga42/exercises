;;; Exercise 1.3. Define a procedure that takes three numbers as arguments
;;; and returns the sum of the squares of the two larger numbers.

(defun sum-of-squares (x y)
	(+ (square x) (square y)))

(defun square (number)
	(* number number))

(defun largest-values (a b c)
	(if (or (> a b) (> a c))
		(if (> b c)
			(list a b)
			(list a c))
		(list b c)))

(defun proc1 (lst)
	(if (eq nil lst)
		0
		(+ (square (first lst))
			(proc1 (cdr lst)))))


(print (sum-of-squares (first (largest-values -5 7 -2))
	(second (largest-values -5 7 -2))))

(print (proc1 (largest-values 5 7 2)))

(defun a-plus-abs-b (a b)
	(funcall (if (> b 0) #'+ #'-) a b))