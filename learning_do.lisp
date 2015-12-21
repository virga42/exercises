(setf m (list 1 7 3 4))

(defun capture-cards (cards-on-table card-from-hand)
  (let ((sum 0))
    (do ((card (car cards) (car cards)))
      ((or (null cards) (= sum card)))
      (setf sum (+ card sum))
      (setf cards (cdr cards)))
  (cdr cards)))
