; Task 2: Mutation
(defmethod mutation((notes list) &aux position otherNotes)
	(setf position (random (length notes)))
	(setf otherNotes (other-elements *notes-list* (nth position notes)))
	(change-element notes (random-element otherNotes) position)
)

(defmethod other-elements((elements list) element)
	(remove element elements)
)

(defmethod change-element((elements list) element position &aux newElements)
	(setf newElements (copy-list elements))
	(setf (nth position newElements) element)
	newElements
)