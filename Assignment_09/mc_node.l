; File: mc_node.l
;------------------------------------------------------------------
;
; MODELING A NODE

(defclass node()
	(
		(name :accessor node-name :initarg :name)
		(state :accessor node-state :initarg :state)
		(parent :accessor node-parent :initarg :parent)
		(operator :accessor node-operator :initarg :operator)
	)
)

; Altered slightly
(defmethod display ((n node))
	(format t "~A " (node-name n))
	(if (not (rootp n))
		(let ()
			(format t "~A " (node-name (node-parent n)))
			(format t "~A " (operator-name (node-operator n)))
		)
	)
	(terpri)
	(display (node-state n))
	nil
)

(defmethod rootp((n node))
	(equal (node-name n) 'root)
)