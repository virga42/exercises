(defpackage :binary-tree
  (:use :common-lisp)
  (:export is-tree))

(in-package :binary-tree)

(defun is-tree (tree)
  (cond
    ((= (length tree) 2) nil)
    ((> (length tree) 3) nil)
    ((atom tree) t)
    ((and (is-tree (second tree)) (is-tree (third tree))) t)
    (t nil)))

