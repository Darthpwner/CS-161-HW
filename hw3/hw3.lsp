;
; CS161 HW3: Sokoban
;
; *********************
;    READ THIS FIRST
; *********************
;
; All functions that you need to modify are marked with 'EXERCISE' in their
; header comments. This file also contains many helper functions. You may call
; any of them in your functions.
;
; Do not modify a-star.lsp.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any
; node. That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your
; heuristic functions.  Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on
; memory. So, it may crash on some hard sokoban problems and there is no easy
; fix (unless you buy Allegro). Of course, other versions of Lisp may also crash
; if the problem is too hard, but the amount of memory available will be
; relatively more relaxed. Improving the quality of the heuristic will mitigate
; this problem, as it will allow A* to solve hard problems with fewer node
; expansions. In either case, this limitation should not significantly affect
; your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition
; (which will affect your score).
;  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
; Finds the keeper's column location
(defun getKeeperColumn (r col)	
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;

; Algorithm: Search the game board for a box. If you encounter a box, return nil. Else, return true.
(defun goal-test (s)
	(cond ((NULL s) t);	If the ENTIRE gameboard is NULL, return t
		
		((isBox(first (first s) ) ) nil);	If the element at your current position is a box, return nil
		
		;Traverse right first, then downwards
		(t 
			(cond ((NULL (first (rest (first s) ) ) ) (goal-test (rest s) ) ); Move to the next row if you are on the last element of your current row
				(t (goal-test (append (list (rest (first s) ) ) (rest s) ) ) );	Move to the next column at the current row
			)
		); end t
	); end outer cond
);end defun

; next-states helpers
(defun get-square (S r c)
	(cond ((NULL (first (first s) ) ) wall); Checks for out of bounds numbers and returns walls
		((or (< r 0) (< c 0) ) wall); Checks for negative numbers and returns wall
		((and (= r 0) (= c 0) ) (first (first s) ) ); Return position when you find it
		
		(;	Keep traversing to find your row first
			(> r 0) (get-square (rest s) (- r 1) c) 
		)

		(;	Then traverse to find your column 
			(> c 0) (get-square (append (list (rest (first s) ) ) (rest s) ) r (- c 1) )
		)
	)
)

; Helper functions for set-square
(defun appendSetRow(L v R); Used to append the inner set row
	(append (append L v R) )
)

(defun maxRow(S); Finds max rows for error checking
	(length S)
)

(defun maxCol(S); Finds max columns for error checking
	(length (first S) )
)

;Use (nthcdr i list) to jump to that part of the list
; Use (butlast list j) to exclude the end of the list
(defun set-square(S r c v)
	(cond ((or (< v 0) (> v 6)) nil);	Checks to see that v is valid
		((or (< (- (maxRow S) 1) r) (< (- (maxCol S) 1) c) ) nil); Return nil if the user passes in an out of bounds number
		((or (< r 0) (< c 0) ) nil); Return nil if the user passes in a negative number
		
		; Append the rows at the start using butlast, then make a list consisting of the row containing the set value, and finally add the rows at the end using nthcdr
		((append (butlast S (- (length S) r) ) (list (appendSetRow (butlast (first (nthcdr r (butlast S (- (- (length S) r) 1) ) ) ) (- (length (first (nthcdr r (butlast S (- (- (length S) r) 1) ) ) ) ) c) ) (list v) (nthcdr (+ c 1) (first (nthcdr r (butlast S (- (- (length S) r) 1) ) ) ) ) ) ) (nthcdr (+ r 1) S) ) )	
	)
)

; Helper functions for try-move
(defun left(S); Check the element to immediately to your left
	(get-square S (second (getKeeperPosition S 0) ) (- (first (getKeeperPosition S 0) ) 1) )
)

(defun left2(S); Check the element 2 spots to your left
	(get-square S (second (getKeeperPosition S 0) ) (- (first (getKeeperPosition S 0) ) 2) )
)

(defun right(S); Check the element to immediately to your right
	(get-square S (second (getKeeperPosition S 0) ) (+ (first (getKeeperPosition S 0) ) 1) )
)

(defun right2(S); Check the element 2 spots to your right
	(get-square S (second (getKeeperPosition S 0) ) (+ (first (getKeeperPosition S 0) ) 2) )
)

(defun up(S); Check the element to immediately up
	(get-square S (- (second (getKeeperPosition S 0) ) 1) (first (getKeeperPosition S 0) ) )
)

(defun up2(S); Check the element 2 spots up
	(get-square S (- (second (getKeeperPosition S 0) ) 2) (first (getKeeperPosition S 0) ) )
)

(defun down(S); Check the element to immediately down
	(get-square S (+ (second (getKeeperPosition S 0) ) 1) (first (getKeeperPosition S 0) ) )
)

(defun down2(S); Check the element 2 spots down
	(get-square S (+ (second (getKeeperPosition S 0) ) 2) (first (getKeeperPosition S 0) ) )
)

; Can't move if keeper (3) is next to a wall (1), consecutive boxes (2) (2), or box + weight (2) (1)
(defun invalid-move(S D)
	;Check for walls immediately to the left, right, up, and down
	(cond ((and (isWall (up S) ) (equal D 'up) ) t); Check for wall up
		 ((and (isWall (down S) ) (equal D 'down) ) t); Check for wall down
		 ((and (isWall (left S) ) (equal D 'left) ) t); Check for wall left
		 ((and (isWall (right S) ) (equal D 'right) ) t); Check for wall right

		; Check for consecutive boxes to the left, right, up, and down
		((and (isBox (up S) ) (isBox (up2 S) ) (equal D 'up) ) t); Check for consecutive boxes up
		((and (isBox (down S) ) (isBox (down2 S) ) (equal D 'down) ) t); Check for consecutive boxes down
		((and (isBox (left S) ) (isBox (left2 S) ) (equal D 'left) ) t); Check for consecutive boxes left
		((and (isBox (right S) ) (isBox (right2 S) ) (equal D 'right) ) t); Check for consecutive boxes right

		; Check for consecutive boxes (box then box on top of goal) to the left, right, up, and down
		((and (isBox (up S) ) (isBoxStar (up2 S) ) (equal D 'up) ) t); Check for consecutive boxes (box then box on top of goal) up
		((and (isBox (down S) ) (isBoxStar (down2 S) ) (equal D 'down) ) t); Check for consecutive boxes (box then box on top of goal) down
		((and (isBox (left S) ) (isBoxStar (left2 S) ) (equal D 'left) ) t); Check for consecutive boxes (box then box on top of goal) left
		((and (isBox (right S) ) (isBoxStar (right2 S) ) (equal D 'right) ) t); Check for consecutive boxes (box then box on top of goal) right		

		; Check for consecutive boxes (box on top of goal then box) to the left, right, up, and down
		((and (isBoxStar (up S) ) (isBox (up2 S) ) (equal D 'up) ) t); Check for consecutive boxes (box on top of goal then box) up
		((and (isBoxStar (down S) ) (isBox (down2 S) ) (equal D 'down) ) t); Check for consecutive boxes (box on top of goal then box) down
		((and (isBoxStar (left S) ) (isBox (left2 S) ) (equal D 'left) ) t); Check for consecutive boxes (box on top of goal then box) left
		((and (isBoxStar (right S) ) (isBox (right2 S) ) (equal D 'right) ) t); Check for consecutive boxes (box on top of goal then box) right		

		; Check for consecutive boxes on top of goals to the left, right, up, and down
		((and (isBoxStar (up S) ) (isBoxStar (up2 S) ) (equal D 'up) ) t); Check for consecutive boxes on top of goals up
		((and (isBoxStar (down S) ) (isBoxStar (down2 S) ) (equal D 'down) ) t); Check for consecutive boxes on top of goals down
		((and (isBoxStar (left S) ) (isBoxStar (left2 S) ) (equal D 'left) ) t); Check for consecutive boxes on top of goals left
		((and (isBoxStar (right S) ) (isBoxStar(right2 S) ) (equal D 'right) ) t); Check for consecutive boxes on top of goals right

		; ; Check for box then wall to the left, right, up, and down
		((and (isBox (up S) ) (isWall (up2 S) ) (equal D 'up) ) t); Check for box then wall up
		((and (isBox (down S) ) (isWall (down2 S) ) (equal D 'down) ) t); Check for box then wall down
		((and (isBox (left S) ) (isWall (left2 S) ) (equal D 'left) ) t); Check for box then wall left
		((and (isBox (right S) ) (isWall (right2 S) ) (equal D 'right) ) t); Check for box then wall right

		; ; Check for box on top of a goal then wall to the left, right, up, and down
		((and (isBoxStar (up S) ) (isWall (up2 S) ) (equal D 'up) ) t); Check for box on goal then wall up
		((and (isBoxStar (down S) ) (isWall (down2 S) ) (equal D 'down) ) t); Check for box on goal then wall down
		((and (isBoxStar (left S) ) (isWall (left2 S) ) (equal D 'left) ) t); Check for box on goal then wall left
		((and (isBoxStar (right S) ) (isWall (right2 S) ) (equal D 'right) ) t); Check for box on goal then wall right

		(t nil); Did not find an invalid move
	)
)

; Move the block
(defun move-block(S D)
	(cond ((equal D 'up)
				(cond ((and (or (isBox (up S) ) (isBoxStar (up S) ) ) (isStar(up2 S) ) ) (set-square S (- (second(getKeeperPosition S 0) ) 2) (first(getKeeperPosition S 0) ) boxstar) ) ;Move the block up and set to boxstar if box lands on goal
					((or (isBox (up S) ) (isBoxStar (up S) ) ) (set-square S (- (second(getKeeperPosition S 0) ) 2) (first(getKeeperPosition S 0) ) box) );	Move the block up normally
					(t S); Else, return the original state if no block is not moved
				)
			) 

		((equal D 'down) 
				(cond ((and (or (isBox (down S) ) (isBoxStar (down S) ) ) (isStar(down2 S) ) ) (set-square S (+ (second(getKeeperPosition S 0) ) 2) (first(getKeeperPosition S 0) ) boxstar) ) ;Move the block down and set to boxstar if box lands on goal
					((or (isBox (down S) ) (isBoxStar (down S) ) ) (set-square S (+ (second(getKeeperPosition S 0) ) 2) (first(getKeeperPosition S 0) ) box) );	Move the block down normally
					(t S); Else, return the original state if no block is not moved
				)
		)

		((equal D 'left)
			(cond ((and (or (isBox (left S) ) (isBoxStar (left S) ) ) (isStar(left2 S) ) ) (set-square S (second(getKeeperPosition S 0) ) (- (first(getKeeperPosition S 0) ) 2) boxstar) ) ;Move the block left and set to boxstar if box lands on goal
				((or (isBox (left S) ) (isBoxStar (left S) ) ) (set-square S (second(getKeeperPosition S 0) ) (- (first(getKeeperPosition S 0) ) 2) box) );	Move the block left normally
				(t S); Else, return the original state if no block is not moved
			)
		)

		((equal D 'right)
			(cond ((and (or (isBox (right S) ) (isBoxStar (right S) ) ) (isStar(right2 S) ) ) (set-square S (second(getKeeperPosition S 0) ) (+ (first(getKeeperPosition S 0) ) 2) boxstar) ) ;Move the block right and set to boxstar if box lands on goal
				((or (isBox (right S) ) (isBoxStar (right S) ) ) (set-square S (second(getKeeperPosition S 0) ) (+ (first(getKeeperPosition S 0) ) 2) box) );	Move the block right normally
				(t S); Else, return the original state if no block is not moved
			)
		)

		(t S); Else, return the original state if no block is not moved
	)
)

; Move the keeper
(defun move-keeper(S D)
	(cond ((equal D 'up)
				(cond ((or (isStar (up S) ) (isBoxStar (up S) ) ) (set-square (move-block S D) (- (second(getKeeperPosition S 0) ) 1) (first(getKeeperPosition S 0) ) keeperstar) ); If moving onto a goal or pushing a box on a goal, keeper becomes keeperstar
					(t (set-square (move-block S D) (- (second(getKeeperPosition S 0) ) 1) (first(getKeeperPosition S 0) ) keeper) ); Else, add keeper to the new position
				) 
		 	) 
		
			((equal D 'down)
				(cond ((or (isStar (down S) ) (isBoxStar (down S) ) ) (set-square (move-block S D) (+ (second(getKeeperPosition S 0) ) 1) (first(getKeeperPosition S 0) ) keeperstar) ); If moving onto a goal or pushing a box on a goal, keeper becomes keeperstar
					(t (set-square (move-block S D) (+ (second(getKeeperPosition S 0) ) 1) (first(getKeeperPosition S 0) ) keeper) ); Else, add keeper to the new position
				)	 
			) 

			((equal D 'left)
				(cond ((or (isStar (left S) ) (isBoxStar (left S) ) ) (set-square (move-block S D) (second(getKeeperPosition S 0) ) (- (first(getKeeperPosition S 0) ) 1) keeperstar) ); If moving onto a goal or pushing a box on a goal, keeper becomes keeperstar
					(t (set-square (move-block S D) (second(getKeeperPosition S 0) ) (- (first(getKeeperPosition S 0) ) 1) keeper) ); Else, add keeper to the new position
				) 
			)
		
			((equal D 'right)
				(cond ((or (isStar (right S) ) (isBoxStar (right S) ) ) (set-square (move-block S D) (second(getKeeperPosition S 0) ) (+ (first(getKeeperPosition S 0) ) 1) keeperstar) ); If moving onto a goal or pushing a box on a goal, keeper becomes keeperstar
					(t (set-square (move-block S D) (second(getKeeperPosition S 0) ) (+ (first(getKeeperPosition S 0) ) 1) keeper) ); Else, add keeper to the new position
				) 
			)
	)
)

; Return the state that the keeper was originally on after performing the successful move
(defun state-after-move(S D)
	(cond
		((isKeeperStar (get-square S (second(getKeeperPosition S 0) ) (first(getKeeperPosition S 0) ) ) ) (set-square (move-keeper S D) (second(getKeeperPosition S 0) ) (first(getKeeperPosition S 0) ) star) ); If keeper was on a goal, change it to a goal
		(t (set-square (move-keeper S D) (second(getKeeperPosition S 0) ) (first(getKeeperPosition S 0) ) blank)) ;otherwise, change it to be blank
	)
)

; Attempts the move and returns NIL if invalid or the state after performing the move
(defun try-move(S D)
	(cond ((invalid-move S D) nil);	Return NIL if the move is invalid
		(t (state-after-move S D) ); Get the moves from state-after-move
	)
)

; EXERCISE: Modify this function to return the list of
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
;
; If you want to use it, you will need to set 'result' to be
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
;
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.

(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move S 'up) (try-move S 'down) (try-move S 'left) (try-move S 'right) ) ) ;Run try-moves on all 4 directions
	 )
    (cleanUpList result);end
   );end let
  );

; EXERCISE: Modify this function to compute the trivial
; admissible heuristic.

; This is uniform cost search since we are not assigning weights to each possible move, and uniform cost search is admissible
(defun h0 (s)
 	(cond (t 0) ); Return 0 in all cases
 )

; EXERCISE: Modify this function to compute the
; number of misplaced boxes in s.
;

; Counts the # of 2's in the gameboard
; My h1 heuristic is admissable because the total number of moves to place the boxes in the goal state is at least the number of misplaced boxes
(defun h1 (s)
	(cond ((NULL s) 0); NULL gameboard has no misplaced boxes
		((atom s) 
			(cond ((isBox s) 1); If S is an atom AND it is misplaced, return 1 as a base case
				(t 0); Else, return 0
			)
		) 

		((equal (length s) 1) (h1 (first s) ) ); Check if S has one more atom or list, then look for misplaced boxes in our gameboard
		(t (+ (h1 (first s) ) (h1 (rest s) ) ) ); Else, add up the number of misplaced boxes in our gameboard
	)
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this
; function to compute an admissible heuristic value of s.
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the
; running time of a function call.
;

; Check the distance from keeper to box upwards
(defun min-distance-up(S)
	(cond ((equal (try-move S 'up) nil) 5000); Return 5000 (Max Heuristic value) if move up is invalid (BUGGY)
		((isBox(up S) ) 0); return 0 if you have a box upwards (make this move)
		(t (+ (min-distance-up (try-move S 'up) ) 1) ); else, keep going upwards
	)
)

; Check the distance from keeper to box downwards
(defun min-distance-down(S)
	(cond ((equal (try-move S 'down) nil) 5000); Return 5000 (Max Heuristic Value) if move down is invalid
		((isBox(down S) ) 0); return 0 if you have a box downwards (best move)
		(t (+ (min-distance-down (try-move S 'down) ) 1) ); else, keep going downwards
	)
)

; Check the distance from the keeper to box left
(defun min-distance-left(S)
	(cond ((equal (try-move S 'left) nil) 5000); Return 5000 (Max Heuristic Value) if move left is invalid
		((isBox(left S) ) 0);	return 0 if you have a box left (best move)
		(t (+ (min-distance-left (try-move S 'left) ) 1) ); else, keep going left
	)
)

; Check the distance from the keeper to box right
(defun min-distance-right(S)
	(cond ((equal (try-move S 'right) nil) 5000) ; Return 5000 (Max Heuristic Value) if move right is invalid
		((isBox (right S) ) 0); return 0 if you have a box right (best move)
		(t (+ (min-distance-right (try-move S 'right) ) 1) ); else, keep going right
	)
)

; 	^				^
; 	|				|
; <-  3 ->	or 	<- 6 ->	My heuristic checks in the immediately 
; 	|				|
; 	v 				v

; heuristic goes in order of up, down, left, and right. If we have boxes immediately adjacent in two or more directions, it will evaluate in 
; the order I defined above.
(defun min-distance-from-keeper-to-box(S)
	(cond ((and (< (min-distance-up S) (min-distance-down S) ) 
				(< (min-distance-up S) (min-distance-left S) ) 
				(< (min-distance-up S) (min-distance-right S) ) 
				(< (min-distance-up S) 5000) ) 
			(min-distance-up S) 
		); up is the minimum distance
		
		((and (< (min-distance-down S) (min-distance-up S) ) 
				(< (min-distance-down S) (min-distance-left S) ) 
				(< (min-distance-down S) (min-distance-right S) ) 
				(< (min-distance-down S) 5000) ) 
			(min-distance-down S) 
		); down is the minimum distance
		
		((and (< (min-distance-left S) (min-distance-up S) ) 
				(< (min-distance-left S) (min-distance-down S) ) 
				(< (min-distance-left S) (min-distance-right S) ) 
				(< (min-distance-left S) 5000) ) 
			(min-distance-left S) 
		); left is the minimum distance
		
		((and (< (min-distance-right S) (min-distance-up S) ) 
				(< (min-distance-right S) (min-distance-down S) ) 
				(< (min-distance-right S) (min-distance-left S) ) 
				(< (min-distance-right S) 5000) ) 
			(min-distance-right S) 
		); right is the minimum distance

		(t 0); If there are no valid moves, return 0
	)	
)

; My h904281426 heuristic is admissible because it seeks the shortest path to a box, so it does not overcount the number of steps.
(defun h904281426 (s)
	(min-distance-from-keeper-to-box S); Heuristic returns the minimum distance of the keeper to the box
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.  Each problem can be visualized by calling
 | (printstate <problem>).  Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem: 1) # of
 | nodes expanded by A* using our next-states and h0 heuristic.  2) the depth of
 | the optimal solution.  These numbers are located at the comments of the
 | problems. For example, the first problem below was solved by 80 nodes
 | expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but
 | it should probably give something in the same ballpark. As for the solution
 | depth, any admissible heuristic must make A* return an optimal solution. So,
 | the depths of the optimal solutions provided could be used for checking
 | whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible
 | to solve without a good heuristic!
 |
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun