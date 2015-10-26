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

; (abs -69); 69
; (abs 0); 0
; (abs 102); 102

; |X_i - X_j| == |i - j|
; (defun check-diagonal(N startRow row Q); Always pass in startRow as 1  
	
; 	(cond ((> startRow (length N) ) t);	Safe to place a queen if the startRow is greater than N
; 		((and (not (equal startRow row) ) (equal (absolute-value(- row startRow) ) (absolute-value(- Q (first N) ) ) ) ) nil);	 Check if |X_i - X_j| == |i - j|. This means you have a diagonal match, so don't place queen. NOTE: This check also enforces that you cannot check startRow with row	
; 		(t (check-diagonal (rest N) (+ startRow 1) row Q) )
; 	)
; )

; |X_i - X_j| == |i - j|
(defun check-diagonal(N)
	(check-diagonal-helper N (rest N) 1);	n1 and n2 are offset initially by a differnece of 1
)

; n1 and n2 will initially be the same
(defun check-diagonal-helper(n1 n2 rowIndex);	Always pass in rowIndex as 1
	(cond ((equal 1 (length n1) ) t); You went through the entire list and did not find a match, so it is safe to place a Queen
		((equal (absolute-value(- (first n1) (first n2) ) ) rowIndex) nil); Found a diagonal match, don't place a queen
		((not (equal (rest n2) nil) ) (check-diagonal-helper n1 (rest n2) (+ rowIndex 1) ) ); If you are not at the last row, check next row and column against the current row and column in the list
		(t (check-diagonal-helper (rest n1) (rest n1) (setq rowIndex 1) ) ); Move to next row and column (item) in the list and reset rowIndex
		; ^^^ THIS CONDITION FUCKS UP BECAUSE n1 == n2 after the first run
	)
)

("CHECK DIAGONAL")
("2 x 2")
(check-diagonal '(1 2) ); nil
(check-diagonal '(1 2) ); nil
(check-diagonal '(2 1) );	nil
(check-diagonal '(2 1) );	nil
("END 2 x 2")

("3 x 3")
(check-diagonal '(1 2 3) ); nil
(check-diagonal '(2 1 3) ); nil
(check-diagonal '(3 1 1) ); nil
(check-diagonal '(1 1 1) ); t
(check-diagonal '(2 2 2) ); t


("END 3 x 3")

(check-diagonal '(4 1 2 1) );	nil
(check-diagonal '(3 1 4 2) );	t


("END CHECK DIAGONAL")

; N is a list representing the rows of your board, Q checks for the column matches in the queens
; Returns t if the diagonal going left is valid, nil if another queen is on the same diagonal
; (defun check-left-diagonal-move-left(N Q)
; 	(cond (and () () ) t); Checked all the diagonals
; 		();	Check the current left diagnoal
; 		(); Move to next left diagonal on a column to the right
; )

; (defun check-left-diagonal-move-right(N Q)
; 	(cond (and () () ) t); Checked all the diagonals
; 		();	Check the current left diagnoal
; 		();	Move to next left diagonal on a column to the left
; )

; ; N is a list representing the rows of your board, Q checks for the column matches in the queens
; ; Returns t if the diagonal going left is valid, nil if another queen is on the same diagonal
; (defun check-diagonal-left(N Q)
; 	(cond (and (check-left-diagonal-move-left N Q) (check-left-diagonal-move-right N Q) ) t); Checked all the left diagonals
; 		(t nil); else, there 
; )

; ; N is a list representing the rows of your board, Q checks for the column matches in the queens
; ; Returns t if the diagonal going right is valid, nil if another queen is on the same diagonal
; (defun check-diagonal-right(N Q)
; 	(cond ); Checked all the right diagonals
; 		();	Check the current right diagnoal
; 		(); Move to the next right diagonal on a column to the left
; 		(); Move to the next right diagonal on a column to the right
; )

(defun place-queen(N)

)



(defun next-state(N)

)

