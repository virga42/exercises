(defun new-if (predicate then-clause else-clause)
    (cond (predicate then-clause)
          (t else-clause)))

(print "new-if:")
(new-if (= 2 3) (print "yes") (print "no"))

(print "if:")
(if (= 2 3) (print "yes") (print "no"))