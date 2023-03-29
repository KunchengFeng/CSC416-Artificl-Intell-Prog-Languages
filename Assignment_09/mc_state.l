; File: mc_state.l
;------------------------------------------------------------------
;
; MODELING A STATE

(defclass state() 
	(
		(left-bank :accessor state-left-bank :initarg :left-bank)
		(right-bank :accessor state-right-bank :initarg :right-bank)
	)
)

(defmethod display ((s state))
	(format t "Current bank: ~A~%" (state-current-bank s))

	(format t "Left  bank: ")
	(display (state-left-bank s))

	(format t "Right bank: ")
	(display (state-right-bank s))

	nil
)

(defmethod equal-state ((this state) (other state))
	(and
		(equal-bank (state-left-bank this) (state-left-bank other))
		(equal-bank (state-right-bank this) (state-right-bank other))
	)
)

(defmethod goalp((s state))
	(and
		(empty-bank (state-left-bank s))
		(full-bank (state-right-bank s))
	)
)

(defmethod feast-state-p((s state))
	(or
		(feast-bank (state-left-bank s))
		(feast-bank (state-right-bank s))
	)
)

(defmethod exploredp((s state) explored-list)
;	### best to use member with two key word args -- :key and :test
	(member s explored-list :test #'equal-state :key #'node-state)
)

; Returns either 'left or 'right
(defmethod state-current-bank((s state))
	(if (has-boat (state-left-bank s))
		'left
		'right
	)
)

; Return a new state instance with the same values
(defmethod copy-state((s state))
	(make-instance 'state
		:left-bank (copy-bank (state-left-bank s))
		:right-bank (copy-bank (state-right-bank s))
	)
)

; Check if the current state supports the upcoming operation
(defmethod applicable-state((s state) crew &aux this-bank)
	(if (equal (state-current-bank s) 'left)
		(setf this-bank (state-left-bank s))
		(setf this-bank (state-right-bank s))
	)

	(and
		(<= (count 'm crew) (length (bank-missionaries this-bank)))
		(<= (count 'c crew) (length (bank-cannibals this-bank)))
	)
)

; Move the crews into the other side of the bank, the boat as well
(defmethod move-state((s state) crew &aux this-bank other-bank m_List c_List)
	; Define variables for easier access
	(cond
		((equal (state-current-bank s) 'left)
			(setf this-bank (state-left-bank s))
			(setf other-bank (state-right-bank s))
		)
		(t
			(setf this-bank (state-right-bank s))
			(setf other-bank (state-left-bank s))
		)
	)
	(setf m_List (remove 'c crew))
	(setf c_List (remove 'm crew))

	(remove-missionaries this-bank m_List)
	(add-missionaries other-bank m_List)

	(remove-cannibals this-bank c_List)
	(add-cannibals other-bank c_List)

	(remove-boat this-bank)
	(add-boat other-bank)
)