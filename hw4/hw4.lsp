; CS 161 HW 4
; N Queens Solver

; Calls helper functions to solve the N-Queens problem on a board (N * N size)
(defun QUEENS(N)

)

; QUEENS helper functions
; Constructs a board diagram with N number of rows and cols
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
; Returns t if the added column is valid, nil if a queen shares the same column
; Algorithm, look for repeated member states in the list and see if it equals Q
(defun check-column(N Q)
	(cond ((null N) t);	You went through the entire list and did not find a match, so it is safe to place a Queen
		((equal Q (first N) ) nil); Found a match in the list, don't place a queen
		(t (check-column (rest N) Q) ); Check the next row
	)
)

("CHECK COLUMN")
(check-column '(1 2 3) 4); t
(check-column '(0 1 2) 2); nil
(check-column '() 2); t because it is empty list CHECK THIS CONDITION
("END CHECK COLUMN")

; Takes the absolute value of the "number" input
(defun absolute-value(number)
	(cond ((< number 0) (- 0 number) ); If number is negative, then return it as positive
		(t number); else, return the number's original value
	)
)

(abs -69); 69
(abs 0); 0
(abs 102); 102

(defun getCurrentRow(startRow)
	(startRow)
)

(defun check-column(N Q)
	(cond ((null N) t);	You went through the entire list and did not find a match, so it is safe to place a Queen
		((equal Q (first N) ) nil); Found a match in the list, don't place a queen
		(t (check-column (rest N) Q) ); Check the next row
	)
)

; |X_i - X_j| != |i - j|
(defun check-diagonal(N startRow row); Always pass in startRow as 1  
	
	(cond (> startRow (length N) );	Stop if the startRow is greater than N
		; Look at rows above "row"
		(cond (< startRow row)	

		)

		;
		((equal (absolute-value(- row startRow) ) (absolute-value(- <column> <startColumn>) ) ) nil)
		;

		; Look at rows below "row"
		(cond (> startRow row)

		)
	)

	; Look at rows below "row"

	(cond ((> row N) t);	You went through the entire list and did not find a match in a diagonal, so it is safe to place a Queen
		();	Found a match in a diagonal, don't place the queen
		(t (check-diagonal (rest N) (+ row 1) Q) );	Check remaining diagonals
	)
)

(defun check-diagonal '(4 1 2 1) 1 2)

(defun check-column(N Q)
	(cond ((null N) t);	You went through the entire list and did not find a match in a column, so it is safe to place a Queen
		((equal Q (first N) ) nil); Found a match in a column, don't place a queen
		(t (check-column (rest N) Q) ); Check the next row
	)
)

; N is a list representing the rows of your board, Q checks for the column matches in the queens
; Returns t if the diagonal going left is valid, nil if another queen is on the same diagonal
(defun check-left-diagonal-move-left(N Q)
	(cond (and () () ) t); Checked all the diagonals
		();	Check the current left diagnoal
		(); Move to next left diagonal on a column to the right
)

(defun check-left-diagonal-move-right(N Q)
	(cond (and () () ) t); Checked all the diagonals
		();	Check the current left diagnoal
		();	Move to next left diagonal on a column to the left
)

; N is a list representing the rows of your board, Q checks for the column matches in the queens
; Returns t if the diagonal going left is valid, nil if another queen is on the same diagonal
(defun check-diagonal-left(N Q)
	(cond (and (check-left-diagonal-move-left N Q) (check-left-diagonal-move-right N Q) ) t); Checked all the left diagonals
		(t nil); else, there 
)

; N is a list representing the rows of your board, Q checks for the column matches in the queens
; Returns t if the diagonal going right is valid, nil if another queen is on the same diagonal
(defun check-diagonal-right(N Q)
	(cond ); Checked all the right diagonals
		();	Check the current right diagnoal
		(); Move to the next right diagonal on a column to the left
		(); Move to the next right diagonal on a column to the right
)

(defun place-queen(N)

)



(defun next-state(N)

)

