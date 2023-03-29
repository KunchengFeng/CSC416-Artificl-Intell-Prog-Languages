; File Task_4.l
; This file holds the code for CSC 416 Assignment 02 Task 4

; Facts:
	(setf rope 88)
	(setf length_barn 62)
	(setf width_barn 44)

; Deductions:
	(setf length_remain (- rope length_barn))
	(setf width_remain (- rope width_barn))

; Calculations:
	(setf area_big (* pi rope rope 0.75))
	(setf area_length_remain (* pi length_remain length_remain 0.25))
	(setf area_width_remain (* pi width_remain width_remain 0.25))

; Conclusion:
	(setf graze_area (+ area_big area_width_remain area_length_remain))