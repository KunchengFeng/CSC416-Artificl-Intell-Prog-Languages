; Task 3: Crossover
(defmethod crossover ((mother list) (father list) &aux position)
	(setf position (+ 1 (random (length mother))))
	(append (first-n mother position) (rest-n father position))
)

(defmethod first-n((l list) (pos number) &aux newPos)
	(setf newPos (- pos 1))
	(cond
		((>= newPos (length l))
			(first-n l newPos)
		)
		((>= newPos 0)
			(append (first-n l newPos) (list (nth newPos l)))
		)
		(t
			(list)
		)
	)
)

(defmethod rest-n((l list) (pos number))
	(cond
		((>= pos (length l))
			(list)
		)
		(t
			(append (list (nth pos l)) (rest-n l (+ pos 1)))
		)
	)
)