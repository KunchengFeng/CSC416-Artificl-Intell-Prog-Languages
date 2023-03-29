; Task 7: The Collection Class
(defconstant *collection-size* 100)
(defconstant *selection-size* 8)

(defclass collection ()
	(
		(melodies :accessor collection-melodies :initarg :melodies)
		(generation :accessor collection-generation :initarg :generation :initform 0)
	)
)

(defmethod size((c collection))
	(length (collection-melodies c))
)

(defmethod display((c collection))
	(format t "~%~%Generation ~A collection ... ~%" (collection-generation c))
	(dolist (i (collection-melodies c))
		(display i) (terpri)
	)
)

(defmethod initial-collection(&aux melodies)
	(setf melodies ())
	(dotimes (i *collection-size*)
		(push (new-melody (+ i 1) (new-notes)) melodies)
	)
	(make-instance 'collection :melodies (reverse melodies))
)

(defmethod average((c collection) &aux sum melodies)
	(setf sum 0.00)
	(setf melodies (collection-melodies c))
	(dotimes (i *collection-size*)
		(setf sum (+ sum (melody-fitness (nth i melodies))))
	)
	(/ sum *collection-size*)
)

(setf *select-demo* nil)

(defmethod select-melody((c collection) &aux candidates mfm)
	(setf candidates (select-candidates c))
	(setf mfm (most-fit-melody candidates))
	(if *select-demo* (select-demo-helper candidates mfm))
	mfm
)

(defmethod select-candidates((c collection) &aux melodies candidates ran)
	(setf melodies (collection-melodies c))
	(setf candidates (list))
	(dotimes (i *selection-size*)
		(setf ran (random *collection-size*))
		(push (nth ran melodies) candidates)
	)
	candidates
)

; Sometimes the fitness could end up been in the negatives
(defmethod most-fit-melody((candidates list) &aux best-score best-melody melody fitness)
	(setf best-melody (first candidates))
	(setf best-score (melody-fitness best-melody))

	(dotimes (i (length candidates))
		(setf melody (nth i candidates))
		(setf fitness (melody-fitness melody))
		(cond
			((>= fitness best-score)
				(setf best-score fitness)
				(setf best-melody melody)
			)
		)
	)
	best-melody
)

(defmethod select-demo-helper((candidates list) (best melody))
	(format t "The sample of melodies ... ~%")
	(dolist (c candidates)
		(display c)
		(terpri)
	)
	(format t "~%The one with the best score ... ~%")
	(display best)
	(terpri) (terpri)
	nil
)

(defmethod collection-demo(&aux c)
	(setf c (initial-collection))
	(display c)
	(format t "Average fitness = ~A~%~%~%" (average c))
	(setf *select-demo* t)
	(format t "Sampling ...~%~%")
	(select-melody c) (terpri)
	(format t "Sampling ...~%~%")
	(select-melody c) (terpri)
	(format t "Sampling ...~%~%")
	(select-melody c) (terpri)
	(setf *select-demo* nil)
	nil
)