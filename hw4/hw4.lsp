; CS 161 HW 4
; N Queens Solver

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

; This function ADDS THEN CHECKS
; N is a list representing the rows of your board
; Returns t if all the columns are valid, nil if a queen shares the same column
; Algorithm, look for repeated member states in the list
(defun check-column(N)
	(check-column-helper N (rest N) );	Compare your current column against the remaining columns
)

; N is a list representing the rows of your board, Q checks for the column matches in the queens
; Returns t if the added column is valid, nil if a queen shares the same column
; Algorithm, look for repeated member states in the list and see if it equals Q

; This function CHECKS BEFORE ADDING!
; (defun check-column(N Q)
; 	(cond ((null N) t);	You went through the entire list and did not find a match, so it is safe to place a Queen
; 		((equal Q (first N) ) nil); Found a match in the list, don't place a queen
; 		(t (check-column-helper(rest N) Q) ); Check the next row
; 	)
; )

; check-column helper function
; Search for all possible duplicates up to the length of n1
; n1 is your original list, n2 is the modified list
(defun check-column-helper(n1 n2)
	(cond ((equal 1 (length n1) ) t);	You went through the entire list and did not find a match, so it was safe to place a Queen
		((equal (first n1) (first n2) ) nil); Found a match in the list, don't place a queen
		((not (equal (rest n2) nil) ) (check-column-helper n1 (rest n2) ) ); Check the next row for any columns mismatches
		(t (check-column-helper (rest n1) (nthcdr 1 (rest n1) ) ) ); Move to the next row (item) in the list and check the remaining items
	)
)

("CHECK COLUMN")
(check-column '(1 2 3) ); t
(check-column '(0 1 1) ); nil
(check-column '() ); nil because it is empty list CHECK THIS CONDITION
(check-column '(1 2 3 4 5) ); t
(check-column '(5 4 3 2 1) ); t
(check-column '(5 4 3 2 2) ); nil
(check-column '(5 4 3 2 5) ); nil
(check-column '(1 4 3 2 9 8 7 6 5 10 11 12 13 14 15 16 17 18 19 20 1) ); nil
(check-column '(1 4 3 2 9 8 7 6 5 10 11 12 13 14 15 16 17 18 19 20 201) ); t
("END CHECK COLUMN")

; (abs -69); 69
; (abs 0); 0
; (abs 102); 102

; This function ADDS THEN CHECKS
(defun check-diagonal(N)
	(check-diagonal-helper N (rest N) 1);	n1 and n2 are offset initially by a differnece of 1, always pass in rowIndex as 1
)

; check-diagonal helper functions
; Takes the absolute value of the "number" input
(defun absolute-value(number)
	(cond ((< number 0) (- 0 number) ); If number is negative, then return it as positive
		(t number); else, return the number's original value
	)
)

; Resets the row index to 1 when we move to the next row to compare (used instead of setq)
(defun rowIndex-reset()
	1
)

; Checking diagonals uses the following algorithm: if |X_i - X_j| == |i - j|, return nil (diagonal match); else, return t at the end
(defun check-diagonal-helper(n1 n2 rowIndex);	Always pass in rowIndex as 1
	(cond ((NULL n1) nil);	Returns nil if given an empty list
		((equal 1 (length n1) ) t); You went through the entire list and did not find a match, so it is safe to place a Queen
		((equal (absolute-value(- (first n1) (first n2) ) ) rowIndex) nil); Found a diagonal match, don't place a queen
		((not (equal (rest n2) nil) ) (check-diagonal-helper n1 (rest n2) (+ rowIndex 1) ) ); If you are not at the last row, check next row and column against the current row and column in the list
		(t (check-diagonal-helper (rest n1) (nthcdr 1 (rest n1)) (rowIndex-reset) ) ); Move to next row and column (item) in the list and reset rowIndex
	)
)
; End of check diagonal helper functions

("CHECK DIAGONAL")
("EMPTY")
(check-diagonal '())

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

("4 x 4")
(check-diagonal '(4 1 2 1) );	nil
(check-diagonal '(3 1 4 2) );	t

("5 x 5")
(check-diagonal '(1 3 5 2 4) ); t
(check-diagonal '(2 5 1 3 4) ); nil
(check-diagonal '(4 1 3 5 2) ); t
(check-diagonal '(1 2 4 5 3) ); nil

("6 x 6")
(check-diagonal '(5 3 1 6 4 2) ); t
(check-diagonal '(5 4 1 3 2 1) ); nil
(check-diagonal '(1 3 5 4 6 1) ); nil
(check-diagonal '(2 2 3 1 4 6) ); nil
(check-diagonal '(1 6 5 2 4 3) ); nil

("7x7")
(check-diagonal '(6 4 2 7 5 3 1)) ;true
(check-diagonal '(6 5 2 7 5 3 1)) ;nil
(check-diagonal '(6 4 2 3 5 3 1)) ;nil
(check-diagonal '(6 4 2 7 5 3 3)) ;nil
(check-diagonal '(6 4 2 7 6 3 1)) ;nil
("8x8")
(check-diagonal '(4 2 7 3 6 8 5 1)) ;true
(check-diagonal '(4 2 7 1 6 8 5 1)) ;nil
(check-diagonal '(4 2 7 3 6 5 5 1)) ;nil
(check-diagonal '(4 2 3 2 6 8 5 1)) ;nil
(check-diagonal '(4 2 7 3 6 8 3 1)) ;nil
(check-diagonal '(6 4 7 1 8 2 5 3)) ;true
("10x10")
(check-diagonal '(7 4 2 9 5 10 8 6 3 1)) ;true
(check-diagonal '(7 4 2 9 5 10 9 6 3 1)) ;nil
(check-diagonal '(7 4 2 9 5 10 8 6 3 10)) ;nil
(check-diagonal '(7 6 2 9 5 10 8 4 3 1)) ;nil

("END CHECK DIAGONAL")

; Checks if state is valid (satisfies check-column AND check-diagonal conditions)
(defun valid-state(N)
	(cond ((and (check-column N) (check-diagonal N) ) t); State is valid if it passes check-column and check-diagonal, so return t
		(t nil); Otherwise, state is invalid, so return nil
	)
)

("VALID STATE")
(valid-state '(1 2 3 4 5)); nil
(valid-state '(1)); t
(valid-state '(2 1)); nil
(valid-state '(3 1 4 2)); t
(valid-state '(2 4 1 3)); t
("VALID STATE")

; Checks if we reached a valid final state
(defun final-state(N N-size)
	(cond ((and (valid-state N) (equal (length N) N-size) ) t);	Check if the move is valid AND (length N) == N-size. If it is, return t
		(t nil);	Otherwise, return nil
	)
)

("FINAL-STATE")
(final-state '(1) 0); nil
(final-state '(1) 2); nil
(final-state '(1) 1); t
(final-state '(1 2) 2); nil
(final-state '(1 2) 1); nil
(final-state '(1 2) 3); nil
(final-state '(3 1 4 2) 3); nil
(final-state '(3 1 4 2) 4); t
(final-state '(3 1 4 2) 5); nil
("END FINAL-STATE")

; REFACTOR THE SHIT BELOW!!!!

; Performs the add, then calls the check constraints. If the add is invalid, revert to the previous state
(defun place-queen(N Q)
	(cond ((not(valid-state (append N (list Q) ) ) ) N); Check if the move is invalid. If it is, revert back to the previous valid state
		(t (append N (list Q) ) ); Otherwise, return the state of the board after making the move
	)
)

("PLACE QUEEN")
(place-queen '(3 1 4) 2); (3 1 4 2)
(place-queen '(3 1 4) 1); (3 1 4)
(place-queen '(3 1 4) 3); (3 1 4)
(place-queen '(3 1 4) 4); (3 1 4)

("END PLACE QUEEN")

; Boolean version of the place-queen function
(defun placed-queen-successfully(N Q)
	(cond ((not(valid-state (append N (list Q) ) ) ) nil); Check if the move is invalid. If it is, return nil
		(t t); Otherwise, return true because queen was placed successfully
	)
)

; Returns a list of lists (possible game states)
; N is the current state, next-states will return the final list of lists, count is your iterator, N-size is the size of your board
; NOTE: Always pass in next-states as '() and count as 1 to get all the possible moves
(defun possible-moves(N next-states count N-size)
	(cond ((> count N-size) next-states);	If count > N-size, you have tried every possible move from your state N, so return next-states
		((or (> (length n) N-size) (= (length n) N-size) ) nil); Prevents next states when you reach N-size
		((placed-queen-successfully N count) (possible-moves N (append next-states (list (place-queen N count) ) ) (+ count 1) N-size) ); If you can place a queen, add it to the list of possible moves
		(t (possible-moves N next-states (+ count 1) N-size) );	If state is invalid, then just move on to the next column
	)
)

("POSSIBLE MOVES")
(possible-moves '() '() 1 1); ( (1) )
(possible-moves '(1 4 2 5) '() 1 5); ( (1) (2) (3) (4) )
(possible-moves '(3 2 1) '() 1 3); ( (1) (2) (3) (4) )
(possible-moves '(1) '() 1 3); ( (1) (2) (3) (4) )
;(possible-moves '() '() 1 50); ( (1) (2) (3) (4) )

("END OF POSSIBLE MOVES")

(defun previous-state(N)
	(butlast N 1)
)

(previous-state '(1 2 3))

; DFS Algorithm:
; 1) Check if final state, if it is, return t
; 2) Place in the first row, first col
; 3) Call valid state and see if move was okay
; 3a) If valid, go to the next row and place in next col
; 3b) else, backtrack one row and place in the next col
; 4) Repeat steps 1 through 3b
; (defun DFS(N rowIndex colIndex N-size)
; 	(cond ((> rowIndex N-size) NIL);	Return nil if no possible options left on the board
; 		((final-state N N-size) N);	//Return N if we have reached the final state
; 		((placed-queen-successfully N col-index) (DFS (place-queen N colIndex) 1 N-size) ); If the move is valid, keep searching down the tree
; 		(t (DFS (place-queen N (+ rowIndex 1) ) rowIndex N-size) ); If the move is invalid, back track and move to the next sibling node
; 	)
; )


(defun DFS(N N-size)
	(cond ((final-state N N-size) N);	Return N if it is the final state
		((valid-state (first(possible-moves N '() 1 N-size) ) ) (DFS (first(possible-moves N '() 1 N-size) ) N-size) ); Recursively call DFS on the first of possible-moves if it is valid
		((equal (possible-moves N '() 1 N-size) NIL)  (DFS (previous-state N) N-size) ); If there are no more possible moves at the current level, backtrack
		(t (DFS(rest(possible-moves N '() 1 N-size) ) N-size) ); If the path is not valid, call it on the rest of DFS
	)
)

("DFS")
; (DFS '(2 4 1 3) 1 1 4);	 (2 4 1 3)
; (DFS '() 1 1 4);	(2 4 1 3)
; (DFS '() 1 1 1);	(2 4 1 3)
(DFS '() 1); 	(2 4 1 3)
(DFS '(2 4) 4)
(DFS '() 4); 	(2 4 1 3)
("END DFS")


; (defun try-move(N rowIndex N-size)
; 	(cond ((final-state N N-size ) N);	//Return N if we have reached the final state
; 		((placed-queen-successfully N rowIndex) (place-queen N rowIndex) ); Execute move if it is possible
; 		((< rowIndex N-size ) (+ rowIndex 1) );	If rowIndex is less than the length of N, increment row index
; 		(t (try-move N rowIndex-reset) ); After placing a Queen, move on to the rest N
; 	)
; )

; Calls helper functions to solve the N-Queens problem on a board (N * N size)
; N is the size of the board in terms of number of squares
(defun QUEENS(N)
	(try-move '() 1 N)
)

; ("QUEENS")
; (QUEENS 1)
; ("FIX SHIT")
; (QUEENS 2)
; (QUEENS 3)
; (QUEENS 4)
; (QUEENS 5)
; (QUEENS 6)
; (QUEENS 7)
; (QUEENS 8)
; (QUEENS 9)
; (QUEENS 10)
; (QUEENS 11)
; (QUEENS 12)
; (QUEENS 13)
; (QUEENS 14)
; (QUEENS 15)
; (QUEENS 16)
; (QUEENS 17)
; (QUEENS 18)
; (QUEENS 19)
; (QUEENS 20)
; ("END QUEENS")
