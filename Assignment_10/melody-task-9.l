; Task 9: Copy
(setf *copy-demo* nil)

(defconstant *percent-copy* 40)

(defmethod perform-copies((original collection) (new collection))
	(dotimes (i (number-of-copies))
		(perform-one-copy original new)
	)
)

(defmethod number-of-copies()
	(* (/ *percent-copy* 100) *collection-size*)
)

; An melody have a chance to mutate before been copied into next generation
(defmethod perform-one-copy((original collection) (new collection) &aux m mm nm)
	(setf m (select-melody original))
	(setf mm (maybe-mutate m))
	(setf nm (new-melody (+ 1 (size new)) (melody-notes mm)))
	(setf 
		(collection-melodies new) 
		(append (collection-melodies new) (list nm))
	)
	(if *copy-demo* (copy-demo-helper m mm nm))
	nil
)

(defmethod copy-demo-helper((selected melody) (mutant melody) (new melody))
	(terpri)
	(format t "Selected melody = ~%")
	(display selected) (terpri)
	(format t "Possibly mutated melody = ~%")
	(display mutant) (terpri)
	(format t "Renumbered melody = ~%")
	(display new) (terpri)
	nil
)

(defmethod empty-collection((original collection) &aux new)
	(make-instance 'collection
		:melodies (list)
		:generation (+ 1 (collection-generation original))
	)
)

(defmethod perform-copies-demo(&aux old new)
	(setf old (initial-collection))
	(setf new (empty-collection old))
	(format t "---------------------------------------------------------------------------~%~%")
	(display new)
	(format t "~%~%---------------------------------------------------------------------------~%")
	(setf *select-demo* t)
	(setf *copy-demo* t)
	(dotimes(i 10)
		(perform-one-copy old new)
		(format t "---------------------------------------------------------------------------~%~%")
		(display new)
		(format t "~%~%---------------------------------------------------------------------------~%")
	)
	(setf *select-demo* nil)
	(setf *copy-demo* nil)
	nil
)