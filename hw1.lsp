; Problems 1-3 are ordered, so they are already sorted.
; Problem 1

;FIXME!
(defun TREE-CONTAINS(N TREE)
	(cond ( (NULL TREE) NIL);	Empty tree should return nil
		( (atom TREE); TREE is 1 element
			(cond ((equal N TREE) t);	R
				(t NIL)
			)
		)
		(t;	recurisvely search TREE
			(let* (
					(left(TREE-CONTAINS N (first TREE) ) ) 
					(right(TREE-CONTAINS N (third TREE) ) ) 
				  ) 
				
				  (cond ((> N (second TREE) ) (TREE-CONTAINS N right) ) 
					((< N (second TREE) ) (TREE-CONTAINS N left) )  
					(t t)
				  ) 
			)
		)
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

; GOOD!
;Problem 3
;The tree is sorted so just return a single list
(defun TREE-ORDER(TREE)
	(cond( (NULL TREE) NIL);	Empty tree should return nil
		( (atom TREE) (list TREE) );	If TREE is atom, make it a list
		(t (append (first TREE) (rest TREE) ) );	Otherwise, append to the list to the tree
	)
)

; GOOD!
; Potential concern is if LEN goes out of bound
; Problem 4
(defun SUB-LIST(L START LEN) 
	(cond ( (NULL L) NIL);	NULL L should return nil 
		( (= LEN 0) NIL) ;	LEN = 0 should return nil
		( (= START 0) (append (list (first L)) (SUB-LIST (rest L) START (- LEN 1) ) ) ); Start counting the SUB-LIST
		( t (SUB-LIST(rest L) (- START 1) LEN) );  Counter has not found START yet
	)
)

; Problem 5
(defun SPLIT-LIST(L)
	(cond ( (NULL L) NIL); NULL L should return nil
		( (evenp (length L) ) (SUB-LIST L 0 (/ (length L) 2) ) );	Even lengths will have L1 = L2
		( (oddp (length L) ) (SUB-LIST L 0 (- (/ (length L) 2) 1) ) );	Odd lengths will have L2 = L1 + 1
	)
)

;GOOD
; Problem 6
(defun BTREE-HEIGHT(TREE)
	(cond ( (NULL TREE) NIL);	Empty tree should return nil
		( (atom TREE) 0);	Return 0 for a number
		( (and (atom (first TREE) ) (atom (second TREE) ) 1) ); Return 1 for any internal nodes
		( t;	Recursively move to next node
			(let* (
				   	(fir (BTREE-HEIGHT (first TREE) ) ) 
					(sec (BTREE-HEIGHT (second TREE) ) )
				  )
				  
				  (cond ( (> fir sec) (+ 1 fir) ) 
						(t (+ 1 sec) );	else
			      )
			)
		)
	)
)

; Problem 7
(defun LIST2BTREE(LEAVES)

)

; Problem 8
(defun BTREE2LIST(TREE)
	(cond ( (NULL TREE) NIL);	Empty tree should return nil
		( (NULL (rest TREE) ) (first TREE) );	Return the last item of the tree since that will be the largest
		(t (TREE-MAX(rest TREE) ) );	Recursively search the rest of the tree
	)
)

;Test cases
;(TREE-CONTAINS 3 '((1 2 3) 7 8))
(TREE-CONTAINS 4 '((1 2 3) 7 8))
;GOOD
(TREE-MAX '((1 2 3) 7 8))
(TREE-MAX '((1 2 3) 7 900))
(TREE-MAX '((1 2 3) 7 81))
(TREE-MAX '((1 2 3) 7 9))
;GOOD
(TREE-ORDER 3)
(TREE-ORDER '((1 2 3) 7 8))
;
(SUB-LIST '(a) 0 1)
(SUB-LIST '(a b c d) 0 1)
(SUB-LIST '(a b c d) 0 2)
(SUB-LIST '(a b c d) 0 3)
(SUB-LIST '(a b c d) 0 4)
(SUB-LIST '(a b c d) 1 1)
(SUB-LIST '(a b c d) 1 2)
(SUB-LIST '(a b c d) 1 3)
(SUB-LIST '(a b c d) 1 4)
(SUB-LIST '(a b c d) 2 1)
(SUB-LIST '(a b c d) 2 2)
(SUB-LIST '(a b c d) 2 3)
(SUB-LIST '(a b c d) 2 4)
;
(SUB-LIST '(a b c d) 2 0)
(SUB-LIST '(a b c d) 3 2)
(SUB-LIST '(a b c d) 4 1)


(SPLIT-LIST '(a b c d))
(SPLIT-LIST '(a b c d e))
(SPLIT-LIST '(a b c d e f))
; GOOD
(BTREE-HEIGHT 1)
(BTREE-HEIGHT '(1 2))
(BTREE-HEIGHT '(1 (2 3)))
(BTREE-HEIGHT '((1 2) (3 4)))
(BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7))))
(BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8))))
;(LIST2BTREE '(1))
;(LIST2BTREE '(1 2))
;(LIST2BTREE '(1 2 3))
;(LIST2BTREE '(1 2 3 4))
;(LIST2BTREE '(1 2 3 4 5 6 7))
;(LIST2BTREE '(1 2 3 4 5 6 7 8))
; (BTREE2LIST 1)
; (BTREE2LIST '(1 2))
; (BTREE2LIST '(1 (2 3)))
; (BTREE2LIST '((1 2) (3 4)))
; (BTREE2LIST '((1 (2 3)) ((4 5) (6 7))))
; (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8))))