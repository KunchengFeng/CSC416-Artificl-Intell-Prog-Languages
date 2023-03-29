; File: mc_operator.l
;---------------------------------------------------------------------------------
; MODELING A STATE SPACE OPERATOR
(defclass operator ()
	(
		(name :accessor operator-name :initarg :name)
		(precondition :accessor operator-precondition :initarg :precondition)
		(description :accessor operator-description :initarg :description)
	)
)

(defmethod display((op operator))
	(format t "Operator name: ~A~% " (operator-name op))
	(format t "Precondition: ~A~% " (operator-precondition op))
	(format t "Description: ~A~%" (operator-description op))
)

; Total of 5 operators:
; *move-c* *move-c-c* *move-m* *move-m-m* *move-c-m*
; They will be concatenated into *operator-list*
(defmethod establish-operators()
	(setf *move-c*
		(make-instance 'operator
			:name 'move-c
			:precondition "Current bank has at least 1 cannibal and a boat."
			:description "Move (b c) to the other bank."
		)
	)
	(setf *move-c-c*
		(make-instance 'operator
			:name 'move-c-c
			:precondition "Current bank has at least 2 cannibals and a boat."
			:description "Move (b c c) to the other bank."
		)
	)
	(setf *move-m*
		(make-instance 'operator
			:name 'move-m
			:precondition "Current bank has at least 1 missionary and a boat."
			:description "Move (b m) to the other bank."
		)
	)
	(setf *move-m-m*
		(make-instance 'operator
			:name 'move-m-m
			:precondition "Current bank has at least 2 missionaries and a boat."
			:description "Move (b m m) to the other bank."
		)
	)
	(setf *move-c-m*
		(make-instance 'operator
			:name 'move-c-m
			:precondition "Current bank has at least 1 cannibal, at least 1 missionary, and a boat."
			:description "Move (b c m) to the other bank."
		)
	)
	(setf *operator-list*
		(list *move-c* *move-c-c* *move-m* *move-m-m* *move-c-m*)
	)
	nil
)


; --------------------------------------------------------------------------------------------
; Check for applicability

; Check if the state satisfies the precondition of the operator
(defmethod applicable-operator((o operator) (s state) &aux name)
	(setf name (operator-name o))

	(cond
		((equal name 'move-c)
			(applicable-state s '(c))
		)
		((equal name 'move-c-c)
			(applicable-state s '(c c))
		)
		((equal name 'move-m)
			(applicable-state s '(m))
		)
		((equal name 'move-m-m)
			(applicable-state s '(m m))
		)
		((equal name 'move-c-m)
			(applicable-state s '(c m))
		)
		(t
			(format t "ERROR! Invalid operator! ~%")
		)
	)
)


; --------------------------------------------------------------------------------------------
; Applying the operators

; Return a new state that has been operated on.
(defmethod apply-operator((o operator) (s state) &aux name this-bank other-bank)
	; Define variables for easier access
	(setf name (operator-name o))

	; Match the operator name and apply
	(cond
		((equal name 'move-c)
			(move-state s '(c))
		)
		((equal name 'move-c-c)
			(move-state s '(c c))
		)
		((equal name 'move-m)
			(move-state s '(m))
		)
		((equal name 'move-m-m)
			(move-state s '(m m))
		)
		((equal name 'move-c-m)
			(move-state s '(c m))
		)
		(t
			(format t "ERROR! Invalid operator! ~%")
		)
	)
)