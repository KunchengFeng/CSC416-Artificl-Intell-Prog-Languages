; Task 5: The Fitness Metric

; Krumhansl stuff -----------------------------------------
; Returns the correlation coefficient of the set of notes
(defmethod krumhansl((notes list) &aux x-mean y-mean x-and-y-sum x-sum y-sum)
	(setf x-mean (get-x-mean notes))
	(setf y-mean (get-y-mean notes))
	(setf x-and-y-sum (get-x-and-y-summation notes x-mean y-mean))
	(setf x-sum (get-x-squared-summation notes x-mean))
	(setf y-sum (get-y-squared-summation notes y-mean))
	(/ x-and-y-sum (sqrt (* x-sum y-sum)))
)

(defun get-x-mean((notes list))
	(/ (length notes) 2.0)
)

(defun get-y-mean((notes list) &aux sum num)
	(setf sum 0.0)
	(setf num (length notes))
	(dotimes (i num)
		(setf sum (+ sum (position (nth i notes) *notes-list*)))
	)
	(/ sum num)
)

(defun get-x-squared-summation((notes list) x-mean &aux n sum i x)
	(setf n (length notes))
	(setf sum 0.0)
	(dotimes (a n)
		(setf i (+ a 1.0))
		(setf x (- i x-mean))
		(setf sum (+ sum (* x x)))
	)
	sum
)

(defun get-y-squared-summation((notes list) y-mean &aux n sum i y)
	(setf n (length notes))
	(setf sum 0.0)
	(dotimes (a n)
		(setf i (+ a 1.0))
		(setf y (- (position (nth a notes) *notes-list*) y-mean))
		(setf sum (+ sum (* y y)))
	)
	sum
)

(defun get-x-and-y-summation((notes list) x-mean y-mean &aux n sum i x y)
	(setf n (length notes))
	(setf sum 0.0)
	(dotimes (a n)
		(setf i (+ a 1.0))
		(setf x (- i x-mean))
		(setf y (- (position (nth a notes) *notes-list*) y-mean))
		(setf sum (+ sum (* x y)))
	)
	sum
)
; ---------------------------------------------------------


; Fitness stuff-----------------------------------------------
; Fitness defined here does not guarantee good music,
; it's just what I think will be a good music

; Coefficient can only range from -1.0 to 1.0
; Remember to set the desired coefficient before calling the fitness function!
(setf desired-coefficient 0.0)

(defmethod *fitness*((notes list) &aux k-score distance score)
	(setf k-score (krumhansl notes))
	(setf distance (abs (- k-score desired-coefficient)))
	(setf score (* (- 1.00 distance) 100.00))
	score
)

(defmethod fitness-demo(&aux notes)
	(setf notes (new-notes))
	(format t "notes = ")
	(display notes)

	(format t "~%Krumhansl score: ~A~%" (krumhansl notes))
	(setf desired-coefficient -1.0)
	(format t "Fitness test with desired coefficient -1.0: ~A~%" (*fitness* notes))
	(setf desired-coefficient 0.0)
	(format t "Fitness test with desired coefficient 0.00: ~A~%" (*fitness* notes))
	(setf desired-coefficient 1.0)
	(format t "Fitness test with desired coefficient +1.0: ~A~%" (*fitness* notes))

	(setf desired-coefficient 0.0)
	nil
)
; ------------------------------------------------------------