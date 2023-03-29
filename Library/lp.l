( defun singleton-p ( l )
	( cond
		( ( null l ) nil )
		( ( null ( cdr l ) ) t )
		( t nil )
	)
)

( defun rac ( l )
	( cond
		( ( singleton-p l )
			( car l )
		)
		( t
			( rac ( cdr l ) )
		)
	)
)

( defun rdc ( l )
	( cond
		( ( singleton-p l )
			()
		)
		( t
			( cons ( car l ) ( rdc ( cdr l ) ) )
		)
	)
)

( defun snoc ( o l )
	( cond
		( ( null l )
			( list o )
		)
		( t
			( cons ( car l ) ( snoc o ( cdr l ) ) )
		)
	)
)

( defun palindrome-p ( l )
	( cond
		( ( null l )
			t
		)
		( ( singleton-p l )
			t
		)
		( ( equal ( car l ) ( rac l ) )
			( palindrome-p ( cdr ( rdc l ) ) )
		)
		( t
			nil
		)
	)
)

( defun sum ( l )
	( cond
		( ( null l )
			0
		)
		( t
			( + ( car l ) ( sum ( cdr l ) ) )
		)
	)
)

(defun product (*list*)
	(cond 
		((null *list*)
			1
		)
		(t
			(* (car *list*) (product (cdr *list*)))
		)
	)
)

( defun iota ( n )
	( cond
		((= n 0)
			()
		)
		( t
			( snoc n ( iota ( - n 1 ) ) )
		)
	)
)

(defun duplicate (n lo)
	(cond 
		((> 0 n)
			nil
		)
		((= 0 n)
			()
		)
		(t
			(cons lo (duplicate (- n 1) lo))
		)
	)
)

(defun select (number *list*)
	(cond
		((> 0 number)
			nil
		)
		((equal nil *list*)
			nil
		)
		((equal 0 number)
			(car *list*)
		)
		(t
			(select (- number 1) (cdr *list*))
		)
	)
)

(defun pick (*list* &aux number)
	(if (equal nil *list*) 
		(return-from pick nil)
	)
	(setf number (random (length *list*)))
	(select number *list*)
)

(defun factorial (number &aux *list*)
	(setf *list* (iota number))
	(product *list*)
)

(defun power (number exponent &aux *list*)
	(setf *list* (duplicate exponent number))
	(product *list*)
)

(defun filter-in (predicate *list* &aux this function)
	(setf this (car *list*))
	(setf function (append predicate (list this)))
	(cond
		((equal this nil)
			nil
		)
		((eval function)
			(cons this (filter-in predicate (cdr *list*)))
		)
		(t
			(filter-in predicate (cdr *list*))
		)
	)
)

(defun filter-out (predicate *list* &aux this function)
	(setf this (car *list*))
	(setf function (append predicate (list this)))
	(cond 
		((equal this nil)
			nil
		)
		((not (eval function))
			(cons this (filter-out predicate (cdr *list*)))
		)
		(t
			(filter-out predicate (cdr *list*))
		)
	)
)

(defun take-from (object *list* &aux this)
	(setf this (car *list*))
	(cond 
		((equal this nil)
			nil
		)
		((equal this object)
			(take-from object (cdr *list*))
		)
		(t
			(cons this (take-from object (cdr *list*)))
		)
	)
)

(defun random-permutation (*list* &aux element *rest*)
	(if (equal *list* nil) 
		(return-from random-permutation nil)
	)
	(setf element (pick *list*))
	(setf *rest* (remove element *list* :count 1))
	(cons element (random-permutation *rest*))
)

(defun check-uniform (l)
	(if (<= (length l) 1)
		t
		(and 
			(equal (first l) (second l))
			(check-uniform (rest l))
		)
	)
)