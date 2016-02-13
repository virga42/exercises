(defpackage :button-01-tests
  (:use :common-lisp :lisp-unit :button-01 :ltk))

(in-package :button-01-tests)

(define-test text-incrementor
  (assert-equal 1 (text-incrementor)))

(run-tests)