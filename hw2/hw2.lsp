; Problem 1
; Input: L is a list representation of the tree
; Return Value: list of terminal nodes in the order they were visited
(defun DFS(L)
	(cond ((NULL L) NIL); If L is NULL< return NIL
		((atom L) (list L) ) ;If L is a single atom, return L as a list
		(t (append (DFS(first L) ) (DFS(rest L) ) ) ); else, traverse downwards, then to the right, and append the list together
	)
)

; Problem 2
; Input:
; Return Value:

; (defun DFID(L MAX_DEPTH)
; 	(cond ((or (NULL L) (< MAX_DEPTH 0) ) nil) ; If L is NULL or MAX_DEPTH is negative, return nil
; 		(t (append (LDFS L (- MAX_DEPTH 1) ) (LDFS L MAX_DEPTH) ) )
; 	)
; )

; ; Helper function for DFID
; ; Input:
; ; Return Value: 
; (defun LDFS (L DEPTH)
; 	(cond ((or (NULL L) (< DEPTH 0) ) nil);	If L is NULL or DEPTH is negative, return nil
; 		((atom L) (list L) );	If L is a single atom, return L as a list
; 		(t (append (LDFS(first L) (- DEPTH 1) ) (LDFS (rest L) DEPTH ) ) )
; 	)
; )

(defun DFID (L MAX_DEPTH)
	(cond ((or (NULL L) (< MAX_DEPTH 0) ) nil) ; ;if there is no TREE, return NIL
		(t (append (DFID L (- MAX_DEPTH 1) ) (LDFS L MAX_DEPTH) ) )
	)
) ;otherwise, use limited DFS to append last and current traversals
	
(defun LDFS (L DEPTH)
	(cond ((or (NULL L) (< DEPTH 0) ) nil) ;if there is no TREE left, return NIL
		((atom L) (list L) ) ;if there is an atom left, return it as a list
		(t (append (LDFS (first L) (- DEPTH 1)) (LDFS (rest L) DEPTH) ) )
	)
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
; GOOD
(defun next-state (s m c)
	(cond ((or (< m 0) (< c 0) ) nil);	Cannot move negative # of missionaries or cannibals
		((or (< (+ m c) 1) (> (+ m c) 2) ) nil); Have to move 1 or 2 people
		( (or (> m (first s) ) (> c (second s) ) ) nil);	Cannot move more missionaries or cannibals than you have on your side
		((and (> (- (first s) m) 0) (> (- (second s) c) (- (first s) m) ) ) nil); Cannot have less missionaries than cannibals on the side you just moved UNLESS you have 0 missionaries after moving 		
		((and (> (+ m (- 3 (first s) ) ) 0) (< (+ m (- 3 (first s) ) ) (+ c (- 3 (second s) ) ) ) ) nil); Cannot have less missionaries than cannibals on your new side UNLESS you have 0 missionaries after moving
		(t (list(+ m (- 3 (first s) ) ) (+ c (- 3 (second s) ) ) (not (third s) ) ) ); Return next state 
	)
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (S), which encodes the current state, and
; returns a list of states that can be reached by applying legal operators to
; the current state.
; GOOD
(defun succ-fn (s)
	; Check each of the 5 possible moves and append it if it does NOT return nill
	(append
		(cond ((not (equal (next-state s 1 0) NIL) ) (list (next-state s 1 0) ) ) )
		(cond ((not (equal (next-state s 2 0) NIL) ) (list (next-state s 2 0) ) ) )
		(cond ((not (equal (next-state s 1 1) NIL) ) (list (next-state s 1 1) ) ) )
		(cond ((not (equal (next-state s 0 1) NIL) ) (list (next-state s 0 1) ) ) )
		(cond ((not (equal (next-state s 0 2) NIL) ) (list (next-state s 0 2) ) ) )
	)
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by MC-DFS (STATES). It returns T if S is a member of
; STATES and NIL otherwise.
; GOOD
(defun on-path (s states)
	(cond((NULL s) nil);	Return nil if s is NULL
		((NULL states) nil);	Return nil if states is NULL
		((equal s (first states) ) t);	Return T if S is a member of STATES
		(t (on-path s (rest states) )  ); Check the other states on the stack
	)
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
	;; check all the states, see if any of the states is the end goal
	;; if it is, append the child to the path
	;; otherwise, return NIL
	(cond ((NULL states) NIL);	Return nil if states has no other other possible moves
	      ((mc-dfs (first states) path) ); Calls MC-DFS and returns the value obtained here
	      (t (mult-dfs (rest states) path) ); else, run MULT-DFS on the rest of the states
	)
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
	(cond ((final-state s) (append path (list s) ) ) ; If S is the final state, append S to our path
		  ((not (on-path s path)) (mult-dfs (succ-fn s) (append path (list s)))) ; If S is not part of the current path, call mult-dfs on S and append it to path
		  (t NIL); else, return NIL because this means that the current state has been visited and there is no solution
	)
)

; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (note that NEXT-STATE
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; SUCC-FN returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))

; DAVIDS SHIT REMOVE LATER
(defun testDFS()
	(AND 
		(equal (DFS '((w x) (y z))) '(W X Y Z))
		(equal (DFS '((A (B)) C (D))) '(A B C D))
		(equal (DFS '((A B) C (D E))) '(A B C D E))
		(equal (DFS '(((A B C)) (D E F) G H)) '(A B C D E F G H))
		(equal (DFS '(((A B C) D) (E F) G H)) '(A B C D E F G H))
	)
)

(defun testDFID () 
	(AND
		(equal (dfid 'A 0) '(A))
		(equal (dfid '(A B) 1) '(A B))
		(equal (dfid '(A B C) 1) '(A B C))
		(equal (dfid '((A B) C (D E)) 2) '(C A B C D E))
		(equal (dfid '(((A B C)) (D E F) G H) 3) '(G H D E F G H A B C D E F G H))
	)
)

;("#1")
(DFS '((A (B)) C (D))); A B C D
(testDFS); T

("#2")
(LDFS '((A (B)) C (D)) 0);	()
(LDFS '((A (B)) C (D)) 1);	(C)
(LDFS '((A (B)) C (D)) 2);	(A C D)
(LDFS '((A (B)) C (D)) 3);	(A B C D)
(LDFS '((A (B)) C (D)) 4); 	(A B C D) SHOULD REPEAT AFTER THIS



(dfid '((A (B)) C (D)) 3)
(dfid '((A (B)) C (D)) 3); '(C A C D A B C D))
("BULLShit")
;(testDFID)
(dfid 'A 0) ;'(A))
		(dfid '(A B) 1) ;'(A B))
		(dfid '(A B C) 1) ;'(A B C)
		(dfid '((A B) C (D E)) 2) ;'(C A B C D E))
		(dfid '(((A B C)) (D E F) G H) 3) ;'(G H D E F G H A B C D E F G H))

; ("#3")
; (final-state '(3 3 NIL))
; (final-state '(3 3 t))

; ("TEST CASES FOR NEXT-STATE")
; (next-state '(3 3 t) 1 0)
; (next-state '(3 3 t) 0 1)

; ("NEGATIVE")
; (next-state '(3 3 NIL) 1 1); t
; (next-state '(3 3 NIL) -1 1); nil
; (next-state '(3 3 NIL) 1 -1); nil
; ("MORE PEOPLE")
; (next-state '(1 3 NIL) 2 1); nil
; (next-state '(3 1 NIL) 1 1); t
; (next-state '(1 3 NIL) 1 5); nil
; (next-state '(3 1 NIL) 1 5); nil


; ("CANNIBAL HUNGRY MY SIDE")
; (next-state '(3 2 NIL) 1 1); t
; (next-state '(3 2 NIL) 2 0); nil
; (next-state '(2 3 NIL) 1 0); nil
; (next-state '(3 3 NIL) 0 0); nil
; (next-state '(2 3 NIL) 2 0); t 
; (next-state '(3 3 NIL) 0 3); nil
; ("CANNIBAL HUNGRY OTHER SIDE")
; (next-state '(2 3 NIL) 0 2); nil

; (next-state '(1 3 NIL) 1 0); t 

; (next-state '(3 3 NIL) 0 2); nil

; ("VIR")
; (next-state '(3 2 t) 1 1)
; ("TEST ON-PATH")
; (on-path 1 NIL); nil
; (on-path NIL 1); nil
; (on-path '(3 2 NIL) '((3 2 NIL)) ); t
; (on-path '(3 c2 NIL) '(NIL (3 1 NIL) NIL ) ); nil
; (on-path '(4 2 NIL) '(NIL (4 2 NIL) NIL ) ); nil

; ("TEST SUCC_FN")
; ("WEST SIDE")
; (succ-fn '(0 1 T) )
; (succ-fn '(0 2 T) )

; (succ-fn '(1 0 t) )
; (succ-fn '(1 1 T) )
; (succ-fn '(2 0 T) )
; (succ-fn '(2 1 T) )
; (succ-fn '(2 2 T) )
; (succ-fn '(3 2 T) )
; (succ-fn '(3 3 T) )
; ("EAST SIDE")
; (succ-fn '(0 1 nil) )
; (succ-fn '(1 0 nil) )
; (succ-fn '(1 1 nil) )
; (succ-fn '(2 0 nil) )
; (succ-fn '(2 1 nil) )
; (succ-fn '(2 2 nil) )
; (succ-fn '(3 2 nil) )
; (succ-fn '(3 3 nil) )

; ("TEST MULT-DFS")
; (mult-dfs (succ-fn '(3 3 NIL) ) '((3 3 NIL))); Test initial == final
; (mult-dfs (succ-fn '(3 3 NIL) ) '((3 3 NIL) (2 2 T) ) ); BUG
; (mult-dfs (succ-fn '(3 3 NIL) ) '((3 2 NIL) (2 2 T) ) ); NIL
; (mult-dfs (succ-fn '(3 3 NIL) ) '((3 2 NIL) (2 2 T) (3 3 NIL)) ); NIL
; ("FRANK")
; (mult-dfs (succ-fn '(0 2 t) ) NIL) '((3 2 NIL) (1 1 T) ) 
; ("FRANK BADASS MOTHERFUCKER!")
; (mc-dfs '(0 2 t) NIL) ;'((0 2 T) (3 2 NIL) (1 1 T) (3 3 NIL) )
; (mc-dfs '(3 3 t) NIL) ;'((3 3 T) (1 1 NIL) (3 2 T) (0 3 NIL) (3 1 T) (2 2 NIL) (2 2 T) (3 1 NIL) (0 3 T) (3 2 NIL) (1 1 T) (3 3 NIL)))
; ("ALTERNATES")
; ("WEST SIDE")
; (mc-dfs '(0 1 T) nil)
; (mc-dfs '(0 2 T) nil)

; (mc-dfs '(1 0 t) nil)
; (mc-dfs '(1 1 T) '((3 3 t) ) )
; (mc-dfs '(2 0 T) nil)
; (mc-dfs '(2 1 T) nil)
; (mc-dfs '(2 2 T) '((3 3 t) (1 1 nil) (3 2 t) (0 3 nil) (3 1 t) (2 2 nil) ) )
; (mc-dfs '(3 2 T) '((3 3 t) (1 1 nil) ) )
; (mc-dfs '(3 3 T) NIL)
; ("EAST SIDE")
; (mc-dfs '(0 1 nil) nil)
; (mc-dfs '(1 0 nil) nil)
; (mc-dfs '(1 1 nil) nil)
; (mc-dfs '(2 0 nil) nil)
; (mc-dfs '(2 1 nil) nil)
; (mc-dfs '(2 2 nil) '((3 3 t) (1 1 nil) (3 2 t) (0 3 nil) (3 1 t) ) )
; (mc-dfs '(3 2 nil) '((3 3 t) (1 1 nil) (3 2 t) (0 3 nil) (3 1 t) (2 2 nil) (2 2 t) (3 1 nil) (0 3 t) ) )
; (mc-dfs '(3 3 nil) '((3 3 t) (1 1 nil) (3 2 t) (0 3 nil) (3 1 t) (2 2 nil) (2 2 t) (3 1 nil) (0 3 t) (3 2 t) (1 1 t) ) )
; ("EXPERIMENTAL")
; (mc-dfs '(3 2 nil) nil)
; (mc-dfs '(1 1 T) '((3 3 t) ) )
; (mc-dfs '(1 1 T) nil )


; ("FRANK TEST")
;; Problem 3 sub-problem testing

; (next-state '(3 3 t) 1 0); nil
; (next-state '(3 3 t) 0 1); '((0 1 NIL))
; (next-state '(2 2 t) 0 2); NIL
; (next-state '(3 3 NIL) 1 1); '((1 1 t))
; (next-state '(1 1 NIL) 1 0); '((3 2 t))
