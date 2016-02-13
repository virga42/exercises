;; hash function
(defun create-hash (key-val)
  (let ((key (if (listp key-val)
                  (car key-val)
                  key-val)))
    (reduce #'* (mapcar #'char-code (coerce key 'list)))))

;; create an dictionary
(defun dictionary ()
  (let* ((dict-len 10)
         (dict (make-array dict-len :initial-element nil)))
    (lambda (fn key-val)
      (cond
        ((eq 'add fn) (add-to-dict dict dict-len key-val)) 
        ((eq 'del fn) (del-from-dict dict dict-len key-val)) 
        ((eq 'ret fn) (return-value dict dict-len key-val))
        (t nil)))))

(defun add-to-dict (dict dict-len key-val)
  (let* ((hash (create-hash key-val))
         (dict-index (mod hash dict-len))
         (lst-at-index (aref dict dict-index)))
    (setf (aref dict dict-index) (add-to-dict-helper key-val lst-at-index)))
  dict)

(defun add-to-dict-helper (key-val lst)
  (cond ((or (null lst) 
         (equalp (car key-val) (car (car lst))))
         (cons key-val (cdr lst)))
        (t (cons (car lst) (add-to-dict-helper key-val (cdr lst))))))

(defun del-from-dict (dict dict-len key-val)
  (let* ((hash (create-hash key-val))
         (dict-index (mod hash dict-len))
         (lst-at-index (aref dict dict-index)))
    (setf (aref dict dict-index) (del-from-dict-helper key-val lst-at-index)))
  dict)

(defun del-from-dict-helper (key-val lst)
  (cond ((or (null lst) 
         (equalp key-val (car (car lst)))) (cdr lst))
        (t (cons (car lst) (del-from-dict-helper key-val (cdr lst))))))

(defun return-value (dict dict-len key-val)
  (let* ((hash (create-hash key-val))
       (dict-index (mod hash dict-len))
       (lst-at-index (aref dict dict-index)))
    (return-value-helper key-val lst-at-index)))

(defun return-value-helper (key-val lst)
  (cond 
    ((null lst) '())
    ((equalp key-val (car (car lst))) (second (car lst)))
    (t (return-value-helper key-val (cdr lst)))))