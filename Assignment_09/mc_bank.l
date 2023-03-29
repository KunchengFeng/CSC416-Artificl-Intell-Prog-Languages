; File: mc_bank.l
;---------------------------------------------------------------------------------
;
; MODELING A BANK

(defclass bank()
	(
		(missionaries :accessor bank-missionaries :initarg :missionaries)
		(cannibals :accessor bank-cannibals :initarg :cannibals)
		(boat :accessor bank-boat :initarg :boat)
	)
)

(defmethod display((b bank))
	(format t "missionaries: ~A    " (bank-missionaries b))
	(format t "cannibals: ~A    " (bank-cannibals b))
	(format t "boat: ~A~%" (bank-boat b))
	nil
)

(defmethod empty-bank((b bank))
	(and
		(equal (bank-missionaries b) nil)
		(equal (bank-cannibals b) nil)
		(equal (bank-boat b) nil)
	)
)

(defmethod full-bank((b bank))
	(and
		(equal (bank-missionaries b) '(m m m))
		(equal (bank-cannibals b) '(c c c))
		(equal (bank-boat b) 'b)
	)
)

(defmethod feast-bank((b bank) &aux mNum cNum)
	(setf mNum (length (bank-missionaries b)))
	(setf cNum (length (bank-cannibals b)))
	(and
		(> mNum 0)
		(> cNum mNum)
	)
)

(defmethod equal-bank((this bank) (other bank))
	(and
		(equal (length (bank-missionaries this)) (length (bank-missionaries other)))
		(equal (length (bank-cannibals this)) (length (bank-cannibals other)))
		(equal (bank-boat this) (bank-boat other))
	)
)

(defmethod copy-bank((b bank))
	(make-instance 'bank
		:missionaries (bank-missionaries b)
		:cannibals (bank-cannibals b)
		:boat (bank-boat b)
	)
)

(defmethod has-boat((b bank))
	(equal (bank-boat b) 'b)
)

(defmethod add-missionaries((b bank) missionaries)
	(setf (bank-missionaries b) (append (bank-missionaries b) missionaries))
)

(defmethod add-cannibals((b bank) cannibals)
	(setf (bank-cannibals b) (append (bank-cannibals b) cannibals))
)

(defmethod add-boat((b bank))
	(setf (bank-boat b) 'b)
)

(defmethod remove-missionaries((b bank) missionaries)
	(setf (bank-missionaries b) (remove 'm (bank-missionaries b) :count (length missionaries)))
)

(defmethod remove-cannibals((b bank) cannibals)
	(setf (bank-cannibals b) (remove 'c (bank-cannibals b) :count (length cannibals)))
)

(defmethod remove-boat((b bank))
	(setf (bank-boat b) nil)
)