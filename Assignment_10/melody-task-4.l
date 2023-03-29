; Task 4: Demo for Mutation and Crossover

; Note: The reason why I don't just display the variables outright
; because their representation is not easy on the eyes.
(defmethod mutation-demo (&aux notes mutant)
	(setf notes (new-notes))
	(dotimes (i 10)
		(format t "notes  = ")
		(display notes)
		(setf mutant (mutation notes))
		(format t "~%mutant = ")
		(display mutant)
		(format t "~%")
	)
	nil
)

(defmethod crossover-demo (&aux mother father offspring)
	(setf mother (new-notes))
	(setf father (new-notes))
	(dotimes (i 10)
		(setf offspring (crossover mother father))
		(format t "mother    = ")
		(display mother) (terpri)
		(format t "offspring = ")
		(display offspring) (terpri)
		(format t "father    = ")
		(display father) (terpri)
	)
	nil
)