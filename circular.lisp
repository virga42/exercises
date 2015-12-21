(setf *print-circle* t)

(defun make-circular-list (lst)
  (setf (cdr (last lst)) lst))

(defun view-circular-list (lst)
  (let ((start (car lst)) (start-count 0))
  (do ((pos (car lst) (car lst)))
    ((and (eq pos start) (> start-count 0)))
    (if (eq pos start)
      (setf start-count (1+ start-count)))
    (print pos)
    (setf lst (cdr lst)))))

(defun circular-list-forward (lst)
  (cdr lst))

(defun circular-forward-helper (lst n)
    (dotimes (i n)
      (setf lst (circular-list-forward lst)))
    lst)

(defun circular-list-backward (lst)
  (when lst (do ((next lst (cdr next)))
          ((eq (cdr next) lst) next))))

(defun circular-list-delete (lst)
  (cond
    ((null lst) lst)
    ((eq lst (cdr lst)) nil)
    (t (let ((result (circular-list-backward lst)))
      (setf (cdr result) (cddr result))))))

(defun circular-list-insert (lst item)
  (if lst
    (setf (cdr lst) (cons item (cdr lst)))
    (setf lst (cons item nil)
      (cdr lst) lst)))

(defun execute-prisoners (lst)
  (do ((pos (car lst) (car lst)))
    ((eq pos (cadr lst)))
    (progn
      (print lst)
      (setf lst (circular-list-delete (circular-forward-helper lst 2)))))
  lst)


