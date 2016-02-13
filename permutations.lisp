(defun all-permutations (lst)
  (cond ((null lst) nil)
        ((null (cdr lst)) (list lst))
        (t (loop for element in lst
             append (mapcar (lambda (l) (cons element l))
                            (all-permutations (remove element lst)))))))

(defun binary-helper (lst)
  ((list 0 lst) (list 1 lst))

(defun binary-permutations (n)
  (cond 
    ((= n 1) '(0 1))
    (t (append (mapcar #'(lambda (x) (list 0 x)) (binary-permutations (1- n)))
               (mapcar #'(lambda (x) (list 1 x)) (binary-permutations (1- n)))))))

(defun bit-shift (lst direction &key (wrap nil))
  (cond
   ((equalp direction 'left)
    (append (cdr lst) (if wrap (list (car lst)) (list 0))))
  ((equalp direction 'right)
    (reverse (bit-shift (reverse lst) 'left :wrap wrap)))))


