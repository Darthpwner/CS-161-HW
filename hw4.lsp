; CS 161 HW 4
; N Queens Solver

; Calls helper functions to solve the N Queens problem on a board (N * N size)
(defun solver(N)

)

; Solver helper functions
(defun construct-board(N)
	(cond ((= N 0) NIL) 
		(t (append '(0) (construct-board (- N 1) ) ) )
	)
)

(construct-board 0)
(construct-board 1)
(construct-board 2)


(defun place-queen(N)

)



(defun next-state(N)

)