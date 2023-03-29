; Task 1: notes
; Since lisp is not case sensitive, I have to store them differently.
(setf *note-limit* 20)

(setf *notes-list* '(
	low-big-c mid-big-c high-big-c 
	low-big-d mid-big-d high-big-d 
	low-big-e mid-big-e high-big-e 
	low-big-f mid-big-f high-big-f 
	low-big-g mid-big-g high-big-g 
	low-big-a mid-big-a high-big-a 
	low-big-b mid-big-b high-big-b 
	low-small-c mid-small-c high-small-c 
	low-small-d mid-small-d high-small-d 
	low-small-e mid-small-e high-small-e 
	low-small-f mid-small-f high-small-f 
	low-small-g mid-small-g high-small-g 
	low-small-a mid-small-a high-small-a 
	low-small-b mid-small-b high-small-b
	)
)

; A display method is needed to accommodate the upper and lower case needs
(defmethod display((notes list))
	(dotimes (i (length notes))
		(display (nth i notes))
		(format t " ")
	)
)

(defmethod display(note)
	(cond
		((equal note 'low-big-c)
			(format t "~A" 'LC)
		)
		((equal note 'mid-big-c)
			(format t "~A" 'MC)
		)
		((equal note 'high-big-c)
			(format t "~A" 'HC)
		)
		((equal note 'low-big-d)
			(format t "~A" 'LD)
		)
		((equal note 'mid-big-d)
			(format t "~A" 'MD)
		)
		((equal note 'high-big-d)
			(format t "~A" 'HD)
		)
		((equal note 'low-big-e)
			(format t "~A" 'LE)
		)
		((equal note 'mid-big-e)
			(format t "~A" 'ME)
		)
		((equal note 'high-big-e)
			(format t "~A" 'HE)
		)
		((equal note 'low-big-f)
			(format t "~A" 'LF)
		)
		((equal note 'mid-big-f)
			(format t "~A" 'MF)
		)
		((equal note 'high-big-f)
			(format t "~A" 'HF)
		)
		((equal note 'low-big-g)
			(format t "~A" 'LG)
		)
		((equal note 'mid-big-g)
			(format t "~A" 'MG)
		)
		((equal note 'high-big-g)
			(format t "~A" 'HG)
		)
		((equal note 'low-big-a)
			(format t "~A" 'LA)
		)
		((equal note 'mid-big-a)
			(format t "~A" 'MA)
		)
		((equal note 'high-big-a)
			(format t "~A" 'HA)
		)
		((equal note 'low-big-b)
			(format t "~A" 'LB)
		)
		((equal note 'mid-big-b)
			(format t "~A" 'MB)
		)
		((equal note 'high-big-b)
			(format t "~A" 'HB)
		)
		((equal note 'low-small-c)
			(format t "~A" '|Lc|)
		)
		((equal note 'mid-small-c)
			(format t "~A" '|Mc|)
		)
		((equal note 'high-small-c)
			(format t "~A" '|Hc|)
		)
		((equal note 'low-small-d)
			(format t "~A" '|Ld|)
		)
		((equal note 'mid-small-d)
			(format t "~A" '|Md|)
		)
		((equal note 'high-small-d)
			(format t "~A" '|Hd|)
		)
		((equal note 'low-small-e)
			(format t "~A" '|Le|)
		)
		((equal note 'mid-small-e)
			(format t "~A" '|Me|)
		)
		((equal note 'high-small-e)
			(format t "~A" '|He|)
		)
		((equal note 'low-small-f)
			(format t "~A" '|Lf|)
		)
		((equal note 'mid-small-f)
			(format t "~A" '|Mf|)
		)
		((equal note 'high-small-f)
			(format t "~A" '|Hf|)
		)
		((equal note 'low-small-g)
			(format t "~A" '|Lg|)
		)
		((equal note 'mid-small-g)
			(format t "~A" '|Mg|)
		)
		((equal note 'high-small-g)
			(format t "~A" '|Hg|)
		)
		((equal note 'low-small-a)
			(format t "~A" '|La|)
		)
		((equal note 'mid-small-a)
			(format t "~A" '|Ma|)
		)
		((equal note 'high-small-a)
			(format t "~A" '|Ha|)
		)
		((equal note 'low-small-b)
			(format t "~A" '|Lb|)
		)
		((equal note 'mid-small-b)
			(format t "~A" '|Mb|)
		)
		((equal note 'high-small-b)
			(format t "~A" '|Hb|)
		)
	)
)

(defmethod random-element((elements list))
	(nth (random (length elements)) elements)
)

(defmethod new-note()
	(random-element *notes-list*)
)

(defmethod new-notes()
	(generate-notes *note-limit*)
)

(defmethod generate-notes(number)
	(if (= number 0)
		(list)
		(cons (random-element *notes-list*) (generate-notes (- number 1)))
	)
)