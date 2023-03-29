; Task 8: Incorporating Mutation
(defmethod mutate((m melody) &aux notes)
	(setf notes (mutation (melody-notes m)))
	(make-instance 'melody
		:number (melody-number m)
		:notes notes
		:fitness (*fitness* notes)
	)
)

(defconstant *percent-mutate* 50)

(defmethod maybe-mutate((m melody))
	(if (<= (+ 1 (random 100)) *percent-mutate*)
		(mutate m)
		m
	)
)

(defmethod mutate-demo()
	(setf m (random-melody))
	(display m)
	(terpri)
	(dotimes(i 20)
		(setf m (mutate m))
		(display m)
		(terpri)
	)
)

(defmethod maybe-mutate-demo()
	(setf m (random-melody))
	(display m) (terpri)
	(dotimes(i 20)
		(setf new (maybe-mutate m))
		(display new)
		(if (not (equal new m)) (princ " *"))
		(terpri)
		(setf m new)
	)
	nil
)