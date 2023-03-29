; File: CLOS_Coding_Exercises.l

(defclass coin () 
	((face 	:accessor coin-face
			:initarg :face
			:initform 'h)
	(history	:accessor coin-history
				:initform ())
	)
)

; Modeling a Water Jug
(defclass jug ()
	(
		(capacity	:accessor jug-capacity
					:initarg :capacity
					:initform 0)
		(level	:accessor jug-level
				:initarg :level
				:initform 0)
	)
)

; Creating Water Jugs
(setf blue-jug (make-instance 'jug :capacity 7))
(setf red-jug (make-instance 'jug :capacity 8 :level 8))

; emptyp
(defmethod emptyp ((object jug))
	(equal (jug-level object) 0)
)

; fullp
(defmethod fullp ((object jug))
	(equal (jug-level object) (jug-capacity object))
)

; equal-jug-p
(defmethod equal-jug-p ((this jug) (other jug))
	(equal (jug-level this) (jug-level other))
)

; jug-space-available
(defmethod jug-space-available ((object jug))
	(- (jug-capacity object) (jug-level object))
)