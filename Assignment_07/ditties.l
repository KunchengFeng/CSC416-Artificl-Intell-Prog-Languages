; File ditties.l
; This file holds the code for CSC 416 Assignment 7, task 11

; Lisp Exercise 1
(defun replace-lcr (location element *list*)
	(cond
		((equal location 'left)
			(list element (second *list*) (third *list*))
		)
		((equal location 'center)
			(list (first *list*) element (third *list*))
		)
		((equal location 'right)
			(list (first *list*) (second *list*) element)
		)
		(t
			nil
		)
	)
)

; Lisp Exercise 2
(defun uniform-p (*list*)
	(cond
		((equal *list* nil)
			t
		)
		((equal (length *list*) 1)
			t
		)
		((equal (first *list*) (second *list*))
			(uniform-p (cdr *list*))
		)
		(t
			nil
		)
	)
)

; Lisp Exercise 3
(defun flush-p (*list*)
	(uniform-p (mapcar #'cdr *list*))
)