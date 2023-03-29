; Task 6: The Melody Class
(defclass melody ()
	(
		(notes :accessor melody-notes :initarg :notes)
		(fitness :accessor melody-fitness :initarg :fitness)
		(number :accessor melody-number :initarg :number)
	)
)

(defmethod random-melody(&aux notes)
	(setf notes (new-notes))
	(make-instance 'melody
		:notes notes
		:fitness (*fitness* notes)
		:number 0
	)
)

(defmethod new-melody((n number) (notes list))
	(make-instance 'melody
		:notes notes
		:fitness (*fitness* notes)
		:number n
	)
)

(defmethod display ((m melody) &aux m-num fitness)
	(setf n-num (melody-number m))
	(setf fitness (melody-fitness m))

	(format t "~A~A" n-num (filler n-num))
	(display (melody-notes m))
	(format t " ~A~A" fitness (filler fitness))
)

; copied from rbg-string assignment
( defmethod filler ( ( n number ) )
	( cond
		( ( < n 10 ) "     " )
		( ( < n 100 ) "    " )
		( ( < n 1000 ) "   " )
		( ( < n 10000 ) "  " )
		( ( < n 100000 ) " " )
	)
)

(defmethod melody-demo (&aux m0 m1 m2 m3)
	(format t "Current desired coefficient: ~A~%" desired-coefficient)
	(setf m0 (random-melody))
	(display m0) (terpri)
	(setf m1 (new-melody 1 (new-notes)))
	(display m1) (terpri)
	(setf m2 (new-melody 2 (new-notes)))
	(display m2) (terpri)
	(setf m3 (new-melody 3 (new-notes)))
	(display m3) (terpri)
	nil
)