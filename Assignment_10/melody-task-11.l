; Task 11: The GA
(defmethod next-generation((old collection) &aux new)
	(setf new (empty-collection old))
	(perform-copies old new)
	(perform-crossover old new)
	new
)

(defconstant *number-of-generations* 25)

(defmethod ga (&aux c)
	(setf desired-coefficient 0.0)
	(setf c (initial-collection))
	(format t "Initial collection: (desired coefficient: 0) ~%~%")
	(summarize c)

	(setf desired-coefficient -1.0)
	(setf c (evolve c))

	(setf desired-coefficient -0.5)
	(setf c (evolve c))

	(setf desired-coefficient 0.0)
	(setf c (evolve c))

	(setf desired-coefficient 0.5)
	(setf c (evolve c))

	(setf desired-coefficient 1.0)
	(setf c (evolve c))

	nil
)

(defmethod evolve((c collection))
	(format t "The desired coefficient is ~A~%" desired-coefficient)
	(dotimes(i *number-of-generations*)
		(setf c (next-generation c))
		(check-average c)
	)
	(terpri)
	(summarize c)
	c
)

(defmethod summarize((c collection))
	(display c)
	(check-average c)
	(terpri)
)

(defmethod check-average((c collection))
	(format t "Average fitness of collection ~A = ~A~%"
		(collection-generation c)
		(average c)
	)
)