; Problem 1
; Input: L is a list representation of the tree
; Return Value: list of terminal nodes in the order they were visited
(defun DFS(L)
	(cond ((NULL L) NIL); 
		((atom L) (list L) ) ;
		(t (append (DFS(first L) ) (DFS(rest L) ) ) )
	)
)

; Problem 2
; Input:
; Return Value:
(defun DFID(L MAX_DEPTH)

)

; Problem 3
; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the other.
; There must be at least one person in the boat to cross the river. There can
; never be more cannibals on one side of the river than missionaries. If there
; are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list (MISSIONARIES
; CANNIBALS SIDE). SIDE represents which side the boat is currently on, and is T
; if it is on the east side and NIL if on the west side. MISSIONARIES and
; CANNIBALS represent the number of missionaries and cannibals on the same side
; as the boat. Thus, the initial state for this problem is (3 3 T) (three
; missionaries, three cannibals, and the boat are all on the east side of the
; river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.


;Helper Functions
; FINAL-STATE takes a single argument (S), the current state, and returns T if
; it is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
	(cond ((equal s '(3 3 NIL) ) t)
		(t NIL)
	)
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), a number of
; missionaries to move (M), and a number of cannibals to move (C). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
	(cond ((or (< m 0) (< c 0) ) nil);	Cannot move negative # of missionaries or cannibals
		((or (< (+ m c) 1) (> (+ m c) 2) ) nil); Have to move 1 or 2 people
		( (or (> m (first s) ) (> c (second s) ) ) nil);	Cannot move more missionaries or cannibals than you have on your side
		((> (- (second s) c) (- (first s) m) ) nil); Cannot have less missionaries than cannibals on the side you just moved 
		;(); Cannot have less missionaries than cannibals on your new side
		(t t)
	)
)
;Chseck if moving more than 2 people
; Check the other side

; (second s - c) > (first s - m); More cannibals on your current side
; ()
; (first s) - m > 0
  ;(second s) - s > 0

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (S), which encodes the current state, and
; returns a list of states that can be reached by applying legal operators to
; the current state.
(defun succ-fn (s)

)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by MC-DFS (STATES). It returns T if S is a member of
; STATES and NIL otherwise.
(defun on-path (s states)

)

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: the path
; from from the initial state to the current state (PATH), and the legal
; successor states to the last state on PATH (STATES). PATH is a first-in
; first-out list of states; that is, the first element is the initial state for
; the current search and the last element is the most recent state explored.
; MULT-DFS does a depth-first search on each element of STATES in turn. If any
; of those searches reaches the final state, MULT-DFS returns the complete path
; from the initial state to the goal state. Otherwise, it returns NIL.
(defun mult-dfs (states path)
 
)

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)

)

(DFS '((A (B)) C (D)))
;(DFS '((3 4 5) 6 ((9) 10))

(final-state '(3 3 NIL))
(final-state '(3 3 t))
("YOLO")
(next-state '(3 3 NIL) 1 1); t
(next-state '(3 3 NIL) -1 1); nil
(next-state '(3 3 NIL) 1 -1); nil
("BITCH")
(next-state '(1 3 NIL) 2 1); nil
(next-state '(3 1 NIL) 1 1); t
(next-state '(1 3 NIL) 1 5); nil
(next-state '(3 1 NIL) 1 5); nil


("FUCK")
(next-state '(3 2 NIL) 1 1); t
(next-state '(3 2 NIL) 2 0); nil
(next-state '(2 3 NIL) 1 0); nil
(next-state '(3 3 NIL) 0 0); nil
(next-state '(2 3 NIL) 2 0); nil: THIS CONDITION NEEDS FIXING
(next-state '(3 3 NIL) 0 3); nil

