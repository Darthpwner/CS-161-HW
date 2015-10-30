; CS 161 HW 4
; N Queens Solver

; Constraint check helper functions
; These assume that previous states were valid and the added element Q is the next free column on your list

; This function ADDS THEN CHECKS
; N is a list representing the rows of your board
; Returns t if all the columns are valid, nil if a queen shares the same column
; Algorithm, look for repeated member states in the list
(defun check-column(N)
	(check-column-helper N (rest N) );	Compare your current column against the remaining columns
)

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

; This function ADDS THEN CHECKS. Tracks if all the diagnols on the gameboard are valid
(defun check-diagonal(N)
	(check-diagonal-helper N (rest N) 1);	n1 and n2 are offset initially by a differnece of 1, always pass in rowIndex as 1
)

; Gets the absolute-value of a number
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

; Checks if state is valid (satisfies check-column AND check-diagonal conditions)
(defun valid-state(N)
	(cond ((and (check-column N) (check-diagonal N) ) t); State is valid if it passes check-column and check-diagonal, so return t
		(t nil); Otherwise, state is invalid, so return nil
	)
)

; Checks if we reached a valid final state
(defun final-state(N N-size)
	(cond ((and (valid-state N) (equal (length N) N-size) ) t);	Check if the move is valid AND (length N) == N-size. If it is, return t
		(t nil);	Otherwise, return nil
	)
)

; Performs the add, then calls the check constraints. If the add is invalid, revert to the previous state
(defun place-queen(N Q)
	(cond ((not(valid-state (append N (list Q) ) ) ) N); Check if the move is invalid. If it is, revert back to the previous valid state
		(t (append N (list Q) ) ); Otherwise, return the state of the board after making the move
	)
)

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

; Performs a DFS to try to solve the N queens problem
; N is the states of the gameboard with previous moves, N-size is the # of rows and cols
(defun DFS (N N-size)
	(cond ((final-state N N-size) N);	Return N if it is the final state 
		(t (DFS-helper (possible-moves N '() 1 N-size) N-size) ); If the path is not valid, call it on the rest of DFS
	)
)

; DFS-helper function that does the actual check and modifying the state of N
; N is the states of the gameboard with previous moves, N-size is the # of rows and cols
(defun DFS-helper(N N-size)
	(cond ((equal NIL N) NIL);	If N equals NIL, you have no possible moves left, so return NIL
		((DFS (first N) N-size) ); Recursively call DFS on the first of possible-moves if it is valid
		(t (DFS-helper (rest N) N-size) ); Otherwise, call DFS-helper on the rest of the current level of the gameboard
	)
)

; Calls helper functions to solve the N-Queens problem on a board (N * N size)
; N is the size of the board in terms of number of squares
(defun QUEENS(N)
	(DFS '() N)
)

;Printing purposes that I found online
(defun gen-row (row-tail q-loc row)
  (cond ((= row q-loc) (gen-row (cons "Q" row-tail) q-loc (- row 1)))
        ((= row 0) row-tail)
        (t (gen-row (cons "." row-tail) q-loc (- row 1)))))
 
(defun gen-field (f n)
  (if (null f) nil
    (cons (gen-row '() (car f) n)
          (gen-field (cdr f) n))))
 
(defun print-queens (l)
  (map 'list
       #'(lambda (l) (format T "~{~a ~}~C" l #\newline))
       (gen-field l (length l))))