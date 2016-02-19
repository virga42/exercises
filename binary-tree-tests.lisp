(defpackage :binary-tree-tests
  (:use :common-lisp :lisp-unit :binary-tree))

(in-package :binary-tree-tests)

(define-test is-tree
  (assert-true (is-tree  nil))
  (assert-true (is-tree '(a nil nil)))
  (assert-true (is-tree '(a (b nil nil) nil)))
  (assert-true (is-tree '(a (b nil nil) (c nil nil))))
  (assert-true (is-tree '(a (b (c nil nil) nil) nil)))
  (assert-false (is-tree '(a (b nil nil))))
  (assert-false (is-tree '(nil nil)))
  (assert-false (is-tree '(a (nil b) nil)))
  (assert-false (is-tree '(a (b (c)))))
  (assert-false (is-tree '(a b c d))))

(run-tests :all)
