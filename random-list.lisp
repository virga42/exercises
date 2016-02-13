; (defun rand-numbers ()
;   (let ((rand-lst (iniitialize-lst 1000000 100000000)))
;     (lambda (fn)
;       (cond 
;         ((equalp fn 'add) 
;           (progn 
;             (setf rand-lst (sort (cons (random 100000000) rand-lst) #'<))
;             (pop rand-lst)))
;         (t (print (sort rand-lst #'<)))))))

(defun rand-numbers ()
  (let ((rand-lst (iniitialize-lst 1000000 100000000)))
    (lambda (fn)
      (cond 
        ((equalp fn 'add) 
          (let* ((new-random (random 100000000))
                ((mins (min-of-lst rand-lst)))
                ((min-number (car mins)))
                ((new-lst (cdr mins))))
            (if (< new-random min-number)
                  min-number
                  (progn 
                    (setf rand-lst (cons new-random new-lst))
                    new-random))))
        (t (print (sort rand-lst #'<)))))))

  (defun iniitialize-lst (n limit)
    (if (= n 0)
        '()
        (cons (random limit) (iniitialize-lst (1- n) limit))))

; (setf l (rand-numbers))

; (dotimes (i 100)
;   do
;     (funcall l 'add))

(defun min-of-lst (lst)
  (let ((min-number (car lst))
        (new-lst '()))
    (dolist (x (cdr lst)) (if (<= x min-number)
                              (progn
                                (setf new-lst (cons min-number new-lst))
                                (setf min-number x))
                              (setf new-lst (cons x new-lst))))
    (list min-number new-lst))) 



