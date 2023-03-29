; Original Code -----------------------------------------------
( defun mc ()
	( establish-world )
	( init-move-list )
	( make-moves )
)

( defun make-moves ()
	( display-world )
	( cond
		( ( goalp )
			( write-line "Good work!" )
			nil
		)
		( ( feast-state-p )
			( write-line "Yummy yummy yummy, I got Good in my tummy!!" )
			nil
		)
		( t
			( let ( m )
				( format t ">>> " ) ( setf m ( read ) )
				( if ( applicable-p m )
					( let () ( perform-move m ) ( make-moves ) )
					( let () ( write-line "Move inapplicable" ) nil )
				)
			)
		)
	)
)

( defun perform-move ( move )
	( setf *move-list* ( snoc move *move-list* ) )
	( if ( equal (current-bank) *left-bank* )
		( move-lr move )
		( move-rl move )
	)
)

( defun move-lr ( ml )
	( if ( null ml ) ( return-from move-lr ) )
	( move-lr-1 ( first ml ) )
	( move-lr ( rest ml ) )
)

( defun move-rl ( ml )
	( if ( null ml ) ( return-from move-rl ) )
	( move-rl-1 ( first ml ) )
	( move-rl ( rest ml ) )
)

; Added Code -------------------------------------------------
; Functions :-
;	snoc
;	establish-world
;	init-move-list
;	display-world
;	display-solution
;		print-list
;	goalp
;	feast-state-p
;		feast-bank
;	applicable-p
;		check-length
;		check-boat
;		check-content
;		check-exist
;			check-member
;	move-lr-1
;	move-rl-1
;	current-bank

; Variables :-
;	*move-list*
;	*left-bank*
;	*right-bank*

; Copied from notes
( defun snoc ( o l )
	( cond
		( ( null l )
			( list o ))
		( t
			( cons ( car l ) ( snoc o ( cdr l ) ) )
		)
	)
)


; Global variables
(setf *left-bank* '())
(setf *right-bank* '())
(setf *move-list* '())

; Functions called at the beginning --------------------------------
(defun establish-world ()
	(setf *left-bank* '(m m m c c c b))
	(setf *right-bank* '())
)

(defun init-move-list ()
	(setf *move-list* '())
)
; ------------------------------------------------------------------


; Functions that show the state of the world -------------------------
(defun display-world ()
	(format t "*left-bank*     ~A~%" *left-bank*)
	(format t "*right-bank*    ~A~%" *right-bank*)
)

(defun display-solution ()
	(print-list *move-list*)
)

(defun print-list (*list* &aux *first*)
	(setf *first* (car *list*))
	(if (equal *first* nil)
		(return-from print-list)
	)
	(format t "~A~%" *first*)
	(print-list (cdr *list*))
)

(defun current-bank ()
	(if (equal (find 'b *left-bank*) nil)
		(return-from current-bank *right-bank*)
		(return-from current-bank *left-bank*)
	)
)
; --------------------------------------------------------------------


; Functions that check the state of the world --------------------------
(defun goalp ()
	(equal *left-bank* '())
)

(defun feast-state-p ()
	(or (feast-bank *left-bank*)
		(feast-bank *right-bank*)
	)
)

(defun feast-bank (*bank* &aux m c)
	(setf m (count 'm *bank*))
	(setf c (count 'c *bank*))
	(and (> m 0) (> c m))
)
; -------------------------------------------------------------------


; Functions that check the validity of the input ----------------------
(defun applicable-p (*move*)
	(and 
		(check-length *move*) 
		(check-boat *move*) 
		(check-content *move*)
		(check-exist *move*)
	)
)

; Min : boat and a person
; Max : boat and two people
; Therefore list length is either 2 or 3.
(defun check-length (*move*)
	(or 
		(equal (length *move*) 2) 
		(equal (length *move*) 3)
	)
)

; Boat must be contained in the input
(defun check-boat (*move*)
	(not (equal (find 'b *move*) nil))
)

; Input elements must either be either b, c, or m.
(defun check-content (*move* &aux *first*)
	(setf *first* (car *move*))
	(if (equal *first* nil)
		(return-from check-content t)
		(and 
			(or 
				(equal 'b *first*) 
				(equal 'm *first*) 
				(equal 'c *first*)
			) 
			(check-content (cdr *move*))
		)
	)
)

; Can't move the things that don't exist
(defun check-exist (*move* &aux *temp-list*)
	(setf *temp-list* (current-bank))
	(check-member *move* *temp-list*)
)
; Extension of (check-exist)
(defun check-member (*things-to-be-moved* *bank* &aux thing)
	(setf thing (car *things-to-be-moved*))
	(if (equal thing nil)
		(return-from check-member t)
	)
	(if (not (equal (find thing *bank*) nil))
		(check-member (cdr *things-to-be-moved*) (remove thing *bank* :count 1))
		(return-from check-member nil)
	)
)
; -----------------------------------------------------------------------


; Functions that move elements between lists -----------------------------
(defun move-lr-1 (element)
	(setf *left-bank* (remove element *left-bank* :count 1))
	(setf *right-bank* (cons element *right-bank*))
)

(defun move-rl-1 (element)
	(setf *right-bank* (remove element *right-bank* :count 1))
	(setf *left-bank* (cons element *left-bank*))
)
; ------------------------------------------------------------------------