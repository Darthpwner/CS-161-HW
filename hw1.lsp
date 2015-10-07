; Problems 1-3 are ordered, so they are already sorted.
; Problem 1

; FIRST TEST FAILS WTF!!!!
(defun TREE-CONTAINS(N TREE)
	(cond ( (NULL TREE) NIL);	Empty tree should return nil
		( (equal N (first TREE) ) t);	If N is equal to the first element, return true
		(t (TREE-CONTAINS N (rest TREE) ) );	Recursively search the rest of the tree
	)
)

; GOOD!
; Problem 2
; The maximum will always be the last element since the tree is sorted.
(defun TREE-MAX(TREE)
	(cond ( (NULL TREE) NIL);	Empty tree should return nil
		( (NULL (rest TREE) ) (first TREE) );	Return the last item of the tree since that will be the largest
		(t (TREE-MAX(rest TREE) ) );	Recursively search the rest of the tree
	)
)

 ;Problem 3
 ;The tree is sorted so just return a single list
;(;defun TREE-ORDER(TREE)
;	(cond ( (numberp (first TREE) ) (list TREE) );	Return a list if given a number
;		(listp (first TREE) ());	Construct the list
;			;	Add nodes to the list
;		(t (TREE-ORDER (rest TREE) ) );	Recursively search the rest of the tree
;	)
;)

;Problem 3
(defun TREE-ORDER(TREE)
	(cond ( (numberp TREE) (list TREE) );	Return a list if given a number
		( (listp (first TREE) ) (append (first TREE) TREE) )  ;	Return TREE if TREE is a list
		(t (TREE-ORDER(rest TREE) ) ); Recursively search the rest of the tree
	)
)

;		( (NULL (rest TREE) ) (TREE) );
;		( (cons (first TREE) (rest TREE) ) );	
;		(t (TREE-MAX(rest TREE) ) );	Recursively search the rest of the tree 


; Problem 4
(defun SUB-LIST(L START LEN)
	(cond (< START LEN) NIL);	Return NIL if LEN is greater than START
		(equal (START LEN) list START);	Return just the START list if one element
		(append START (first L) ); Append to the list START
		(t (SUB-LIST(L (+ START 1) (- LEN 1) ) ) );	Go down the list
)

; Problem 5
(defun SPLIT-LIST(L)

)

; Problem 6
(defun BTREE-HEIGHT(TREE)
	(cond ( (NULL TREE) NIL);	Empty tree should return nil

	)
)

; Problem 7
(defun LIST2BTREE(LEAVES)

)

; Problem 8
(defun BTREE2LIST(TREE)

)

;Test cases
(TREE-CONTAINS 3 '((1 2 3) 7 8))
(TREE-CONTAINS 4 '((1 2 3) 7 8))
(TREE-MAX '((1 2 3) 7 8))
(TREE-ORDER 3)
(TREE-ORDER '((1 2 3) 7 8))
;(SUB-LIST '(a b c d) 0 3)
;(SUB-LIST '(a b c d) 3 1)
;(SUB-LIST '(a b c d) 2 0)
;(SPLIT-LIST '(a b c d))
;(SPLIT-LIST '(a b c d e))
;(SPLIT-LIST '(a b c d e f))
;(BTREE-HEIGHT 1)
;(BTREE-HEIGHT '(1 2))
;(BTREE-HEIGHT '(1 (2 3)))
;(BTREE-HEIGHT '((1 2) (3 4)))
;(BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7))))
;(BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8))))
;(LIST2BTREE '(1))
;(LIST2BTREE '(1 2))
;(LIST2BTREE '(1 2 3))
;(LIST2BTREE '(1 2 3 4))
;(LIST2BTREE '(1 2 3 4 5 6 7))
;(LIST2BTREE '(1 2 3 4 5 6 7 8))
;(BTREE2LIST 1)
;(BTREE2LIST '(1 2))
;(BTREE2LIST '(1 (2 3)))
;(BTREE2LIST '((1 2) (3 4)))
;(BTREE2LIST '((1 (2 3)) ((4 5) (6 7))))
;(BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8))))