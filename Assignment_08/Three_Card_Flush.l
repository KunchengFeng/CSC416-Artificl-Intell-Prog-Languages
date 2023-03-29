; File: Three_Card_Flush.l
; This file holds the code for assignment "Three_Card_Flush"
; Use the codes that was already written
(load "../library/lp.l")

; Part 1 The Deck and the Shuffle ----------------------------------------

; 1). make-deck
(defun make-deck ()
	(mapcan (function make-rank-suit) '(club diamond heart spade))
)

(defun make-rank-suit (suit &aux ranks suit-duplicates)
	(setf ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace))
	(setf suit-duplicates (duplicate (length ranks) suit))
	(mapcar (function cons) ranks suit-duplicates)
)

(defun demo--make-deck (&aux deck)
	(format t ">>> Testing: make-deck~%")
	(setf deck (make-deck))
	(format t "--- deck = ~A~%" deck)
	(format t "--- number of cards in deck = ~A~%" (length deck))
	nil
)

; 2). establish-shuffle-deck
; Special care for global variable
(setf *deck* (list))

(defun establish-shuffled-deck ()
	(setf *deck* (shuffle (make-deck)))
	nil
)

(defun shuffle (deck &aux card)
	(cond
		((null deck)
			deck
		)
		(t
			(setf card (pick deck))
			(setf deck (remove card deck :count 1))
			(cons card (shuffle deck))
		)
	)
)

(defun demo--establish-shuffled-deck ()
	(format t ">>> Testing: establish-shuffled-deck~%")
	(establish-shuffled-deck)
	(format t "--- *deck* ... ~A~%" *deck*)
	(format t "--- number of cards in *deck* = ~A~%" (length *deck*))
	nil
)


; Part 2: The Hands, the Deal and the Discard --------------------------
; Global variables
(setf *hand1* (list))
(setf *hand2* (list))

; 1). deal-hands
(defun deal-hands ()
	(establish-shuffled-deck)
	(setf *hand1* ())
	(setf *hand2* ())
	(deal-card-to-hand1)
	(deal-card-to-hand2)
	(deal-card-to-hand1)
	(deal-card-to-hand2)
	(deal-card-to-hand1)
	(deal-card-to-hand2)
	nil
)

(defun deal-card-to-hand1 (&aux card)
	(setf card (car *deck*))
	(setf *deck* (cdr *deck*))
	(setf *hand1* (snoc card *hand1*))
)

(defun deal-card-to-hand2 ()
	(setf card (car *deck*))
	(setf *deck* (cdr *deck*))
	(setf *hand2* (snoc card *hand2*))
)

(defun demo--deal-hands () 
	(format t ">>> Testing: deal-hands~%")
	(deal-hands)
	(format t "--- *hand1* = ~A~%" *hand1*)
	(format t "--- *hand2* = ~A~%" *hand2*)
	(format t "--- number of cards in *deck* = ~A~%" (length *deck*))
	nil
)

; 2). Randomly-discard-cards
(defun randomly-discard-cards ()
	(randomly-discard-card-from-hand1)
	(randomly-discard-card-from-hand2)
)

(defun randomly-discard-card-from-hand1 (&aux number)
	(setf number (random (length *hand1*)))
	(setf (nth number *hand1*) (list))
)

(defun randomly-discard-card-from-hand2 (&aux number)
	(setf number (random (length *hand2*)))
	(setf (nth number *hand2*) (list))
)

(defun demo--randomly-discard-cards ()
	(format t ">>> Testing: randomly-discard-cards~%")
	(deal-hands)
	(format t "Before:~%")
	(format t "--- *hand1* = ~A~%" *hand1*)
	(format t "--- *hand2* = ~A~%" *hand2*)
	(randomly-discard-cards)
	(format t "After:~%")
	(format t "--- *hand1* = ~A~%" *hand1*)
	(format t "--- *hand2* = ~A~%" *hand2*)
	nil
)


; Part 3: Replacing Cards in Hands, Taking Turns -------------------------------------

; 1). replace-cards
(defun replace-cards()
	(replace-card-in-hand1)
	(replace-card-in-hand2)
	nil
)

(defun replace-card-in-hand1 (&aux newCard)
	(setf newCard (car *deck*))
	(setf *deck* (cdr *deck*))
	(setf (nth (position nil *hand1*) *hand1*) newCard)
)

(defun replace-card-in-hand2 (&aux newCard)
	(setf newCard (car *deck*))
	(setf *deck* (cdr *deck*))
	(setf (nth (position nil *hand2*) *hand2*) newCard)
)

(defun demo--replace-cards ()
	(format t ">>> Testing: replace-cards~%")
	(deal-hands)
	(format t "Dealt cards:~%")
	(format t "--- *hand1* = ~A~%" *hand1*)
	(format t "--- *hand2* = ~A~%" *hand2*)
	(randomly-discard-cards)
	(format t "Cards randomly discarded:~%")
	(format t "--- *hand1* = ~A~%" *hand1*)
	(format t "--- *hand2* = ~A~%" *hand2*)
	(replace-cards)
	(format t "Cards replaced:~%")
	(format t "--- *hand1* = ~A~%" *hand1*)
	(format t "--- *hand2* = ~A~%" *hand2*)
	nil
)

; 2). players-each-take-a-turn
(defun players-each-take-a-turn ()
	(randomly-discard-cards)
	(replace-cards)
	nil
)

(defun demo--players-each-take-a-turn ()
	(format t ">>> Testing: players-each-take-a-turn~%")
	(deal-hands)
	(format t "--- The hands ...~%")
	(format t "--- *hand1* = ~A~%" *hand1*)
	(format t "--- *hand2* = ~A~%" *hand2*)
	(players-each-take-a-turn)
	(format t "--- Each player takes a turn ...~%")
	(format t "--- *hand1* = ~A~%" *hand1*)
	(format t "--- *hand2* = ~A~%" *hand2*)
	(players-each-take-a-turn)
	(format t "--- Each player takes a turn ...~%")
	(format t "--- *hand1* = ~A~%" *hand1*)
	(format t "--- *hand2* = ~A~%" *hand2*)
	(players-each-take-a-turn)
	(format t "--- Each player takes a turn ...~%")
	(format t "--- *hand1* = ~A~%" *hand1*)
	(format t "--- *hand2* = ~A~%" *hand2*)
	(players-each-take-a-turn)
	(format t "--- Each player takes a turn ...~%")
	(format t "--- *hand1* = ~A~%" *hand1*)
	(format t "--- *hand2* = ~A~%" *hand2*)
	nil
)



; Part 4: Hand Analysis --------------------------------------------------------------
; 1). flush-p
(defun flush-p (hand &aux suits)
	(setf suits (mapcar (function cdr) hand))
	(check-uniform suits)
)

( defun demo--flush-p ( &aux hand )
	( format t ">>> Testing: flush-p~%" )
	( setf hand '( ( 2 . club ) ( ace . club ) ( 10 . club ) ) )
	( format t "~A " hand )
	( if ( flush-p hand )
		( format t "is a flush~%" )
		( format t "is not a flush~%" )
	)
	( setf hand '( ( jack . diamond ) ( 9 . diamond ) ( 5 . diamond ) ) )
	( format t "~A " hand )
	( if ( flush-p hand )
		( format t "is a flush~%" )
		( format t "is not a flush~%" )
	)
	( setf hand '( ( jack . heart ) ( 10 . heart ) ( 9 . heart ) ) )
	( format t "~A " hand )
	( if ( flush-p hand )
		( format t "is a flush~%" )
		( format t "is not a flush~%" )
	)
	( setf hand '( ( 2 . spade) ( 3 . spade ) ( ace . spade ) ) )
	( format t "~A " hand )
	( if ( flush-p hand )
		( format t "is a flush~%" )
		( format t "is not a flush~%" )
	)
	( setf hand '( ( 10 . spade) ( 5 . diamond ) ( ace . spade ) ) )
	( format t "~A " hand )
	( if ( flush-p hand )
		( format t "is a flush~%" )
		( format t "is not a flush~%" )
	)
	( setf hand '( ( 8 . club) ( 9 . diamond ) ( 10 . heart ) ) )
	( format t "~A " hand )
	( if ( flush-p hand )
		( format t "is a flush~%" )
		( format t "is not a flush~%" )
	)
)

; 2). high-card
; Set as global variable in case of future use
(setf *ranks* '(2 3 4 5 6 7 8 9 10 jack queen king ace))
(setf *suits* '(club diamond heart spade))

(defun high-card (hand)
	(highest-card (car hand) (cdr hand))
)

(defun highest-card (high hand &aux this)
	(setf this (car hand))
	(cond 
		((equal nil this)
			high
		)
		((> (position (car high) *ranks*) (position (car this) *ranks*))
			(highest-card high (cdr hand))
		)
		((< (position (car high) *ranks*) (position (car this) *ranks*))
			(highest-card this (cdr hand))
		)
		((> (position (cdr high) *suits*) (position (cdr this) *suits*))
			(highest-card high (cdr hand))
		)
		(t
			(highest-card this (cdr hand))
		)
	)
)

(defun demo--high-card ()
	(format t ">>> Testing: high-card~%")
	(setf hand '((10 . heart) (5 . club) (queen . spade) (7 . heart)))
	(format t "~A is the high card of ~A~%" (high-card hand) hand)
	(setf hand '((2 . diamond) (2 . club) (10 . heart) (4 . diamond) (ace . club)))
	(format t "~A is the high card of~% ~A~%" (high-card hand) hand)
	(setf hand '((ace . diamond) (ace . club) (5 . spade)))
	(format t "~A is the high card of~% ~A~%" (high-card hand) hand)
	nil
)

; 3). straight-p
(defun straight-p (hand &aux hand-ranks)
	(setf hand-ranks (mapcar #'car hand))
	(setf hand-ranks (mapcar #'rank-to-number hand-ranks))
	(setf hand-ranks (sort hand-ranks '<))
	(check-increment hand-ranks)
)

(defun rank-to-number (rank)
	(position rank *ranks*)
)

(defun check-increment (hand-ranks)
	(if (<= (length hand-ranks) 1)
		t
		(and 
			(equal (+ (first hand-ranks) 1) (second hand-ranks))
			(check-increment (rest hand-ranks))
		)
	)
)

(defun demo--straight-p ()
	(format t ">>> Testing: straight-p~%")
	(setf hand '((5 . spade) (3 . diamond) (4 . spade) (6 . club)))
	(format t "~A " hand)
	(if (straight-p hand)
		(format t "is a straight~%")
		(format t "is not a straight~%")
	)
	(setf hand '((5 . spade) (7 . diamond) (4 . spade) (8 . club)))
	(format t "~A " hand)
	(if (straight-p hand)
		(format t "is a straight~%")
		(format t "is not a straight~%")
	)
	(setf hand '((king . heart) (queen . diamond) (ace . spade) (10 . club) (jack . diamond)))
	(format t "~A " hand)
	(if (straight-p hand)
		(format t "is a straight~%")
		(format t "is not a straight~%")
	)
	(setf hand '((ace . club) (2 . diamond) (3 . spade)))
	(format t "~A " hand)
	(if (straight-p hand)
		(format t "is a straight~%")
		(format t "is not a straight~%")
	)
	nil
)

; 4). analyze-hand
(defun analyze-hand (hand &aux high rank suit)
	(cond 
		((not (flush-p hand))
			'bust
		)
		(t
			(setf high (high-card hand))
			(setf rank (car high))
			(setf suit (cdr high))
			(if (straight-p hand)
				(setf result (list rank 'high 'straight suit 'flush))
				(setf result (list rank 'high suit 'flush))
			)
		)
	)
)

(defun demo--analyze-hand ()
	(format t ">>> Testing: analyze-hand~%")
	(setf hand '((5 . spade) (3 . diamond) (4 . spade )))
	(format t "~A is a ~A~%" hand (analyze-hand hand))
	(setf hand '((5 . club) (9 . club) (4 . club)))
	(format t "~A is a ~A~%" hand (analyze-hand hand))
	(setf hand '((queen . heart) (ace . heart) (king . heart)))
	(format t "~A is a ~A~%" hand (analyze-hand hand))
	nil
)


; Part 5: Game State and End of Game Reporting----------------------------------------

; 1). analyze-hand
; More global variables
(setf *game-state* (list))
(defun analyze-game ()
	(setf *game-state* (list (analyze-hand *hand1*) (analyze-hand *hand2*)))
)

(defun demo--analyze-game ()
	(format t ">>> Testing: analyze-game~%")
	; a couple of busts
	(format t "Game 1 ... ~%" )
	(setf *hand1* '((2 . diamond) (4 . diamond) (jack . heart)))
	(setf *hand2* '((10 . spade) (king . heart) (queen . heart)))
	(analyze-game )
	(format t "*hand1* = ~A~%" (write-to-string *hand1*))
	(format t "*hand2* = ~A~%" *hand2*)
	(format t "*game-state* = ~A~%" *game-state*)
	; an ordinary flush and a straight flush
	(format t "Game 2 ... ~%")
	(setf *hand1* '((10 . diamond) (jack . diamond) (2 . diamond)))
	(setf *hand2* '((3 . spade) (5 . spade) (4 . spade)))
	(analyze-game)
	(format t "*hand1* = ~A~%" (write-to-string *hand1*))
	(format t "*hand2* = ~A~%" *hand2*)
	(format t "*game-state* = ~A~%" *game-state*)
	nil
)

; 2). report-the-result
(defun report-the-result ()
	(cond
		((equal *game-state* '(bust bust))
			(increment '*draw-count*)
			(format t "--> The game is a draw. The deck is dead.~%")
		)
		((and 	
			(not ( equal (first *game-state*) 'bust))
			(equal (second *game-state*) 'bust))
				(increment '*win1-count*)
				(format t "--> Player 1 wins with ~A~%" (first *game-state*))
		)
		((and 	
			(equal (first *game-state*) 'bust)
			(not (equal (second *game-state*) 'bust)))
				(increment '*win2-count*)
				(format t "--> Player 2 wins with ~A~%" (second *game-state*))
		)
		((and 
			(straight-p *hand1*) 
			(not (straight-p *hand2*)))
				(increment '*win1-count*)
				(format t "!!! Both players found their way to a flush~%")
				(format t "--> Player 1 wins with ~A~%" (first *game-state*))
		)
		((and
			(not (straight-p *hand1*))
			(straight-p *hand2*))
				(increment '*win2-count*)
				(format t "!!! Both players found their way to a flush~%")
				(format t "--> Player 2 wins with ~A~%" (second *game-state*))
		)
		((card-greater (high-card *hand1*) (high-card *hand2*))
			(increment '*f1f2-count*)
			(increment '*win1-count*)
			( format t "!!! Both players found their way to a flush~%")
			( format t "--> Player 1 wins with ~A~%" (first *game-state*))
		)
		((card-greater (high-card *hand2*) (high-card *hand1*))
			(increment '*f1f2-count*)
			(increment '*win2-count*)
			(format t "!!! Both players found their way to a flush~%")
			(format t "--> Player 2 wins with ~A~%" (second *game-state*))
		)
	)
	nil
)

; Returns true is thisCard is greater than otherCard
(defun card-greater (thisCard otherCard)
	(if (equal (car thisCard) (car otherCard))
		(> (position (cdr thisCard) *suits*) (position (cdr otherCard) *suits*))
		(> (position (car thisCard) *ranks*) (position (car otherCard) *ranks*))
	)
)

(defun demo--report-the-result ()
	(format t ">>> Testing: report-the-result ~%")

	(format t "~%Testing: (bust bust) ~%")
	(setf *hand1* '((2 . club) (3 . diamond) (4 . heart)))
	(setf *hand2* '((2 . diamond) (3 . heart) (4 . spade)))
	(analyze-game)
	(format t "*hand1*: ~A~%" *hand1*)
	(format t "*hand2*: ~A~%" *hand2*)
	(format t "Game state: ~A ~%" *game-state*)
	(report-the-result)

	(format t "~%Testing: (not-bust bust) ~%")
	(setf *hand1* '((2 . club) (4 . club) (6 . club)))
	(setf *hand2* '((2 . diamond) (3 . heart) (4 . spade)))
	(analyze-game)
	(format t "*hand1*: ~A~%" *hand1*)
	(format t "*hand2*: ~A~%" *hand2*)
	(format t "Game state: ~A ~%" *game-state*)
	(report-the-result)

	(format t "~%Testing: (bust not-bust) ~%")
	(setf *hand1* '((2 . club) (4 . diamond) (6 . heart)))
	(setf *hand2* '((2 . diamond) (3 . diamond) (4 . diamond)))
	(analyze-game)
	(format t "*hand1*: ~A~%" *hand1*)
	(format t "*hand2*: ~A~%" *hand2*)
	(format t "Game state: ~A ~%" *game-state*)
	(report-the-result)

	(format t "~%Testing: (straight not-straight) ~%")
	(setf *hand1* '((2 . club) (3 . club) (4 . club)))
	(setf *hand2* '((2 . diamond) (4 . diamond) (5 . diamond)))
	(analyze-game)
	(format t "*hand1*: ~A~%" *hand1*)
	(format t "*hand2*: ~A~%" *hand2*)
	(format t "Game state: ~A ~%" *game-state*)
	(report-the-result)

	(format t "~%Testing: (not-straight straight) ~%")
	(setf *hand1* '((2 . club) (4 . club) (6 . club)))
	(setf *hand2* '((2 . diamond) (3 . diamond) (4 . diamond)))
	(analyze-game)
	(format t "*hand1*: ~A~%" *hand1*)
	(format t "*hand2*: ~A~%" *hand2*)
	(format t "Game state: ~A ~%" *game-state*)
	(report-the-result)

	(format t "~%Testing: (greater-flush flush) ~%")
	(setf *hand1* '((2 . club) (4 . club) (6 . club)))
	(setf *hand2* '((2 . diamond) (3 . diamond) (5 . diamond)))
	(analyze-game)
	(format t "*hand1*: ~A~%" *hand1*)
	(format t "*hand2*: ~A~%" *hand2*)
	(format t "Game state: ~A ~%" *game-state*)
	(report-the-result)

	(format t "~%Testing: (flush greater-flush) ~%")
	(setf *hand1* '((2 . club) (4 . club) (6 . club)))
	(setf *hand2* '((2 . diamond) (3 . diamond) (6 . diamond)))
	(analyze-game)
	(format t "*hand1*: ~A~%" *hand1*)
	(format t "*hand2*: ~A~%" *hand2*)
	(format t "Game state: ~A ~%" *game-state*)
	(report-the-result)
)


; Part 6: Play Game ------------------------------------------------------------------------
; 1). hand-rep
(defun map-rank (rank)
	(cond
		((equal rank 10)
			'x)
		((equal rank 'jack)
			'j)
		((equal rank 'queen)
			'q)
		((equal rank 'king)
			'k)
		((equal rank 'ace)
			'a)
		(t
			rank)
	)
)

(defun map-suit (suit)
	(cond
		((equal suit 'club)
			'c)
		((equal suit 'diamond)
			'd)
		((equal suit 'heart)
			'h)
		((equal suit 'spade)
			's)
		(t
			suit)
	)
)

(defun map-card (card)
	(cons (map-rank (car card)) (map-suit (cdr card)))
)

(defun hand-rep (hand)
	(mapcar #'map-card hand)
)

(defun demo--hand-rep (&aux hand)
	(establish-shuffled-deck)
	(setf internal (list (pop *deck*) (pop *deck*) (pop *deck*)))
	(setf external (hand-rep internal))
	(format t "~A --> ~A~%" internal external)
	nil
)

; 2). play-game, make-moves, game-over-p
(defun play-game () 
	(increment '*game-count*)
	(deal-hands)
	(make-moves)
	(report-the-result)
)

(defun make-moves ()
	(increment '*turn-count*)
	(format t "~A    ~A~%" (hand-rep *hand1*) (hand-rep *hand2*))
	(if (not (game-over-p))
		(let ()
			(players-each-take-a-turn)
			(make-moves))
	)
	nil
)

(defun game-over-p ()
	(analyze-game)
	(or
		(not (equal *game-state* '(bust bust)))
		(null *deck*))
)

(defun demo--play-game ()
	(format t ">>> Testing: play-game~%")
	(play-game)
)


; Part 7: Computing Statistics ---------------------------------------------------------
; More global variables.
; Counter initialization -- initialize once so the game can be played regardless of
; whether or not statistics are being computed
(defun init-counters ()
	(setf *game-count* 0)
	(setf *turn-count* 0)
	(setf *win1-count* 0)
	(setf *win2-count* 0)
	(setf *draw-count* 0)
	(setf *f1f2-count* 0)
	nil
)

(init-counters)

; Flexible counter incrementation
(defun increment (name)
	(set name (+ (eval name) 1))
)

; The main statistics computation program
(defun compute-statistics (n)
	(init-counters)
	(play-game-n-times n)
	(format t "*game-count* = ~A~%" *game-count*)
	(format t "*turn-count* = ~A~%" *turn-count*)
	(format t "*win1-count* = ~A~%" *win1-count*)
	(format t "*win2-count* = ~A~%" *win2-count*)
	(format t "*draw-count* = ~A~%" *draw-count*)
	(format t "*f1f2-count* = ~A~%" *f1f2-count*)
	nil
)

; Program to play the game n times
(defun play-game-n-times (n)
	(cond
		((> n 0)
			(play-game)
			(play-game-n-times (- n 1)))
	)
)


; Part 8: The Heuristic Player --------------------------------------------------------
; 1). players-each-take-a-turn
(defun players-each-take-a-turn ()
	(randomly-heuristically-discard-cards)
	(replace-cards)
)

(defun randomly-heuristically-discard-cards ()
	(randomly-discard-card-from-hand1)
	(heuristic-discard-card-from-hand2)
	nil
)

; 2). heuristic-discard-card-from-hand2
; I would first try to discard the card with the lone suit
; If all three cards have different suit, I would discard card with smallest number
; It is assumes that hand 2 will always have three cards, otherwise this won't work.
(defun heuristic-discard-card-from-hand2 (&aux rank1 rank2 rank3 suit1 suit2 suit3)
	(setf rank1 (position (car (first *hand2*)) *ranks*))
	(setf suit1 (cdr (first *hand2*)))

	(setf rank2 (position (car (second *hand2*)) *ranks*))
	(setf suit2 (cdr (second *hand2*)))

	(setf rank3 (position (car (third *hand2*)) *ranks*))
	(setf suit3 (cdr (third *hand2*)))

	(cond
		; (same same different), discard the thrid card
		((and 
			(equal suit1 suit2)
			(not (equal suit1 suit3)))
				(setf (third *hand2*) nil)
		)
		; (same different same), discard the second card
		((and
			(equal suit1 suit3)
			(not (equal suit1 suit2)))
				(setf (second *hand2*) nil)
		)
		; (different same same), discard the first card
		((and 
			(equal suit2 suit3)
			(not (equal suit1 suit2)))
				(setf (first *hand2*) nil)
		)
		; (lowest higer higer), discard the first card
		((and
			(<= rank1 rank2)
			(<= rank1 rank3))
				(setf (first *hand2*) nil)
		)
		; (higer lowest higher), discard the second card
		((and
			(<= rank2 rank1)
			(<= rank2 rank3))
				(setf (second *hand2*) nil)
		)
		; (higher higher lowest), discard the third card
		((and
			(<= rank3 rank1)
			(<= rank3 rank2))
				(setf (third *hand2*) nil)
		)
	)
	nil
)

(defun demo--randomly-heuristically-discard-cards ()
	(format t ">>> Testing: randomly-discard-cards~%")
	(deal-hands)
	(format t "--- *hand1* = ~A~%" *hand1*)
	(format t "--- *hand2* = ~A~%" *hand2*)
	(randomly-heuristically-discard-cards)
	(format t "--- *hand1* = ~A~%" *hand1*)
	(format t "--- *hand2* = ~A~%" *hand2*)
	nil
)