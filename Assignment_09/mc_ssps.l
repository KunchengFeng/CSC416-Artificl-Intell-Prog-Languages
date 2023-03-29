; File: mc_ssps.l
;
;-------------------------------------------------------------------
; Description: 
;
; This program is a state space problem solver for a classic missionaries and cannibls problem.
; A state space tree is grown in concert with breadth first search for a solution
;
; ------------------------------------------------------------------
; REPRESENTATIONAL NOTES
;
; Banks are represented as a 3-slot class consisting of
; missionaries, cannibals, and a boat.
;
; States are represented as a 2-slot class consisting of
; left-bank (object), right-bank (object).
;
; Operators are represented as a 3-slot class consisting of
; a name, a precondition, and a description.
;
; Nodes are represented as a 4-slot class consisting of
; a name, a state, a parent node, and a move (state space operator)


(load "mc_bank.l")
(load "mc_state.l")
(load "mc_node.l")
(load "mc_operator.l")

;---------------------------------------------------------------------------------
; THE MAIN PROGRAM - argument values of e u x eu ex ux eux will cause tracing
(defmethod mc ((trace-instruction symbol))
	(setf *trace-instruction* trace-instruction)
	(establish-operators)
	(setup)
	(solve)
)


;---------------------------------------------------------------------------------
; SOLVE PERFORMS BREADTH FIRST SEARCH
; Exploredp is modified slightly
(defmethod solve (&aux kids e-node)
	(if (member *trace-instruction* '(u eu ux eux)) (display-the-unexplored-list))
	(if (member *trace-instruction* '(x ex ux eux)) (display-the-explored-list))
	(cond
		((null *unexplored*)
			(format t ">>> THERE IS NO SOLUTION.~%")
			(return-from solve NIL)
		)
	)

	(setf e-node (pop *unexplored*))

	(if (member *trace-instruction* '(e ex eu eux)) (display-the-e-node e-node))

	(cond
		((goalp (node-state e-node))
			(format t "~%>>> GOT A SOLUTION!")
			(display-solution e-node)
		)
		((feast-state-p (node-state e-node))
			(solve)
		)
		((exploredp (node-state e-node) *explored*)
			(solve)
		)
		(t
			(push e-node *explored*)
			(setf kids (children-of e-node))
			(setf *unexplored* (append *unexplored* kids))
			(solve)
		)
	)
	nil
)

(defmethod display-the-unexplored-list()
	(format t "~%>>> Unexplored list~%")
	(mapcar #'display *unexplored*)
	nil
)

(defmethod display-the-explored-list()
	(format t "~%>>> Explored list~%")
	(mapcar #'display *explored*)
	nil
)

(defmethod display-the-e-node((n node))
	(format t "~%>>> E-node~%")
	(display n)
)

(defmethod display-solution((n node))
	(cond
		((rootp n)
			(terpri)
		)
		(t
			(display-solution (node-parent n))
			(format t "~A~%" (operator-description (node-operator n)))
		)
	)
	nil
)

;------------------------------------------------------------------
; THE SETUP
(defmethod setup (&aux root lb rb istate)
	;; establish root node
	(setf lb (make-instance 'bank :missionaries '(m m m) :cannibals '(c c c) :boat 'b))
	(setf rb (make-instance 'bank :missionaries '() :cannibals '() :boat nil))
	(setf istate (make-instance 'state :left-bank lb :right-bank rb))
	(setf root (make-instance 'node :state istate :name 'root))

	;; initialize list of unexplored nodes
	(setf *unexplored* (list root))

	;; initialize list of explored nodes
	(setf *explored* ())

	; get ready to create good names
	(setf *ng* (make-instance 'name-generator :prefix "N"))
)


;------------------------------------------------------------------
; GENERATING CHILDREN
(defmethod children-of ((n node) &aux kids e-state)
	(setf kids (list))
	(setf e-state (node-state n))

	(if (applicable-operator *move-c* e-state)
		(push (child-of n *move-c*) kids)
	)
	(if (applicable-operator *move-c-c* e-state)
		(push (child-of n *move-c-c*) kids)
	)
	(if (applicable-operator *move-m* e-state)
		(push (child-of n *move-m*) kids)
	)
	(if (applicable-operator *move-m-m* e-state)
		(push (child-of n *move-m-m*) kids)
	)
	(if (applicable-operator *move-c-m* e-state)
		(push (child-of n *move-c-m*) kids)
	)

	kids
)

; Since the new state is an instance, it's values are modified inside "apply-operator"
(defmethod child-of ((n node) (o operator) &aux new-state)
	(setf new-node (make-instance 'node))
	(setf (node-name new-node) (next *ng*))
	(setf (node-parent new-node) n)
	(setf (node-operator new-node) o)
	(setf new-state (copy-state (node-state n)))
	(apply-operator o new-state)
	(setf (node-state new-node) new-state)
	new-node
)


;------------------------------------------------------------------
; MODELLING A NAME-GENERATOR
(defclass name-generator ()
	(
		(prefix :accessor name-generator-prefix :initarg :prefix :initform "name")
		(nr :accessor name-generator-nr :initform 0)
	)
)

(defmethod next ((ng name-generator))
	(setf (name-generator-nr ng) (+ 1 (name-generator-nr ng)))
	(concatenate 'string
		(name-generator-prefix ng)
		(write-to-string (name-generator-nr ng))
	)
)