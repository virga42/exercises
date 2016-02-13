; '(mining going-home eating sleeping going-work)

(defun new-miner ()
  (let ((current-state 'mining)
        (hours-worked 0)
        (hours-slept 0)
        (hunger-level 0)
        (miles-from-home 20))
  (lambda (event)
    (cond 
      ((eq event 'wife-calls) (progn
                                (print "wife calls")
                                (setf current-state 'going-home)))
      (t nil))
    (cond
      ((eq current-state 'mining) (progn 
                                      (setf hours-worked (1+ hours-worked))
                                      (setf hunger-level (+ hunger-level 2))
                                      (when (or (>= hours-worked 8) (> hunger-level 6))
                                            (setf current-state 'going-home))))
      ((eq current-state 'going-home) (progn
                                        (setf miles-from-home (- miles-from-home 4))
                                        (when (<= miles-from-home 0)
                                            (setf current-state 'eating))))
      ((eq current-state 'eating) (progn
                                    (setf hunger-level (1- hunger-level))
                                    (when (<= hunger-level 0)
                                      (if (< hours-worked 8)
                                        (setf current-state 'going-work)
                                        (setf current-state 'sleeping)))))
      ((eq current-state 'sleeping) (progn
                                      (setf hours-slept (1+ hours-slept))
                                      (setf hunger-level (1+ hunger-level))
                                      (when (>= hours-slept 8)
                                        (setf current-state 'eating)
                                        (setf hours-worked 0))))
      ((eq current-state 'going-work) (progn
                                        (setf miles-from-home (+ miles-from-home 6))
                                        (when (>= miles-from-home 20)
                                          (setf current-state 'mining)
                                          (setf hunger-level (+ hunger-level .5))))))
      current-state)))

(let ((my-miner (new-miner))
      (other-miner (new-miner)))
  (dotimes (i 100)
    ; (print "Miner A: ")
    (print (funcall my-miner (if (> (random 100) 95) 'wife-calls nil)))
    ; (print "Miner B: ")
    ; (print (funcall other-miner (if (> (random 100) 97) 'wife-calls nil)))
    (sleep .2))) 