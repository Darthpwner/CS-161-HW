; CS 161 HW 4
; N Queens Solver

; Calls helper functions to solve the N-Queens problem on a board (N * N size)
(defun QUEENS(N)

)

; QUEENS helper functions
; Constructs a board diagram with N number of rows
; _ _ _ _ _ _ _
; 1 2 3 ...	  N
(defun construct-board(N)
	(cond ((= N 0) NIL) 
		(t (append '(0) (construct-board (- N 1) ) ) );	TODO: CHANGE '(0) to the added queen later
	)
)

("CONSTRUCT BOARD")
(construct-board 0)
(construct-board 1)
(construct-board 2)
(construct-board 8)
("END OF CONSTRUCT BOARD")

; Constraint check helper functions
; These assume that previous states were valid and the added element Q is the next free column on your list

; N is a list representing the rows of your board, Q checks for the column matches in the queens
; Returns t if the row is valid, nil if a queen shares the same column
; Algorithm, look for repeated member states in the list and see if it equals Q
(defun check-row(N Q)
	(cond ((null N) t);	You went through the entire list and did not find a match, so it is safe to place a Queen
		((equal Q (first N) ) nil); Found a match in the list, don't place a queen
		(t (check-row (rest N) Q) ); Check the next row
	)
)

("CHECK ROW")
(check-row '(1 2 3) 4); t
(check-row '(0 1 2) 2); nil
(check-row '() 2); t because it is empty list CHECK THIS CONDITION
("END CHECK ROW")

; N is a list representing the rows of your board, Q checks for the column matches in the queens
; Returns t if the diagonal going left is valid, nil if another queen is on the same diagonal
(defun check-diagonal-left(N Q)
	(cond ); Checked all the diagonals
		();	Check the current left diagnoal
		();	Move to next left diagonal
)

; N is a list representing the rows of your board, Q checks for the column matches in the queens
; Returns t if the diagonal going right is valid, nil if another queen is on the same diagonal
(defun check-diagonal-right(N Q)
	(cond ); Checked all the right diagonals
		();	Check the current right diagnoal
		(); Move to the next right diagonal
)

(defun place-queen(N)

)



(defun next-state(N)

)

