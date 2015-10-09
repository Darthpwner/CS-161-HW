; Problems 1-3 are ordered, so they are already sorted.

; Problem 1
; Input: N is an atom, TREE is an ordered tree
; Return value: A Boolean value if N is in TREE
; Check to see if the TREE contains the atom N
; It is an ordered tree so you can traverse it like a binary search tree
(defun TREE-CONTAINS(N TREE)
	(cond ((NULL TREE) NIL);	Empty tree should return nil
		((atom TREE) (= N TREE) );	Check if TREE is an atom and return true if N = TREE
		((= (second TREE) N) T);	Return true if the node of the TREE = N
		((> (second TREE) N) (TREE-CONTAINS N (first TREE) ) ) ;	Check the left TREE if it is not in 2nd TREE
		((< (second TREE) N) (TREE-CONTAINS N (third TREE) ) );	Check the right TREE if it is not in the 2nd TREE
	)
)

; Problem 2
; Input: TREE is an ordered tree
; Return value: The largest value in the TREE
; Find the biggest node value in TREE
; The maximum will always be the last element since the tree is sorted.
(defun TREE-MAX(TREE)
	(cond ( (NULL TREE) NIL);	Empty tree should return nil
		( (NULL (rest TREE) ) (first TREE) );	Return the last item of the tree since that will be the largest
		(t (TREE-MAX(rest TREE) ) );	Recursively search the rest of the tree
	)
)

; Problem 3
; Input: TREE is an ordered tree
; Return value: The ordered tree as a list
; The tree is sorted so just return a single list
(defun TREE-ORDER(TREE)
	(cond( (NULL TREE) NIL);	Empty tree should return nil
		( (atom TREE) (list TREE) );	If TREE is atom, make it a list
		(t (append (first TREE) (rest TREE) ) );	Otherwise, append to the list to the tree
	)
)


; Problem 4
; Input: L is a List, START is the index the SUB-LIST begins
; Return value: A SUB-LIST from START to LEN. 
; Potential concern is my function will still return a list if the length goes out of bounds
(defun SUB-LIST(L START LEN) 
	(cond ( (NULL L) NIL);	NULL L should return nil 
		( (= LEN 0) NIL) ;	LEN = 0 should return nil
		( (= START 0) (append (list (first L)) (SUB-LIST (rest L) START (- LEN 1) ) ) ); Start counting the SUB-LIST
		( t (SUB-LIST(rest L) (- START 1) LEN) );  Counter has not found START yet
	)
)

; Problem 5
; Input: L is a List
; Return value: A List containing two SUB-LIST's
; Splits the list in half into two sub-lists
; For odd length lists, L2 = L1 + 1
(defun SPLIT-LIST(L)
	(cond ( (NULL L) NIL); NULL L should return nil
		((= (length L) 1) L);	Handles base case where you only have 1 element in the list
		((evenp (length L)); Treats the even case 
			(list (SUB-LIST L 0 (/ (length L) 2) ) (SUB-LIST L (/ (length L) 2) (/ (length L) 2) ) ) 
		)
		(t; Treats the odd case 
			(list (SUB-LIST L 0 (/ (- (length L) 1) 2) ) (SUB-LIST L (/ (- (length L) 1) 2) (/ (+ (length L) 2) 1) ) ) 
		)
	)
)

; Problem 6
; Input: TREE is a Binary Tree
; Return value: The integer height of the Binary Tree
; One sub-tree will be higher, or they will be the same height
(defun BTREE-HEIGHT(TREE)
	(cond ( (NULL TREE) NIL);	Empty tree should return nil
		( (atom TREE) 0);	Return 0 for a number
		( (and (atom (first TREE) ) (atom (second TREE) ) 1) ); Return 1 for any internal nodes
		( t;	Recursively move to next node
			(let* (
				   	(left (BTREE-HEIGHT (first TREE) ) ); Set left sub-tree 
					(right (BTREE-HEIGHT (second TREE) ) ); Set right sub-tree
				  )
				  
				  ;	Keep going down the sub-tree with a greater height
				  (cond ( (> left right) (+ 1 left) )
						(t (+ 1 right) );	
			      )
			)
		)
	)
)

; Problem 7
; Input: LEAVES is a List
; Return value: Binary Tree with sub-list formats from the List
; Convert a list to a binary tree
; LEAVES is a list, so call splitter to get it in the sub-list formats
(defun LIST2BTREE(LEAVES)
	(cond ( (NULL LEAVES) NIL);	Empty List should return nil
		((= (length LEAVES) 1) (first LEAVES));	An atom should return the root
		((= (length LEAVES) 2) (list (first LEAVES) (second LEAVES)) );	A list of two should return a single list
		(t ;	Recursively search the rest of the tree
			(let* (
					(splitter (SPLIT-LIST LEAVES) ); Split the list
						(left (LIST2BTREE(first splitter) ) ); left subtree
						(right (LIST2BTREE(second splitter) ) ); right subtree
				  )
				  (list left right) 
			)
		)
	)
)

; Problem 8
; Input: TREE is a Binary Tree
; Return value: List from the Binary Tree
; Convert a binary tree to a list
; TREE is in the sub-list format and BTREE2LIST is the inverse of LIST2BTREE
(defun BTREE2LIST(TREE)
	(cond ( (NULL TREE) NIL);	Empty tree should return nil
		((atom TREE) (list TREE) );	Return the atom as a list if TREE is just a number
		(t (append(BTREE2LIST(first TREE) ) (BTREE2LIST(rest TREE) ) ) );	Recursively call BTREE2LIST and append it to a main list
	)
)