; Problem 1
(defun TREE-CONTAINS(N TREE)
	(cond ((NULL TREE) NIL);	Empty tree should return nil
		((equal N (first TREE)) t);	If N is equal to the first element, return true
		(t (TREE-CONTAINS(N (rest TREE))));	Recursively search the rest of the tree
	)
)

; Problem 2 HELP
(defun TREE-MAX(TREE)
	(cond (NULL (rest TREE) <max>);	Return max of the tree when you reach the end
		((> first(TREE) <max>) <max> = first(TREE));	Compare the current node to the max
		(t (last (rest TREE)) );	Move to the next node
	)
)

; Problem 3
(defun TREE-ORDER(TREE)

)

; Problem 4
(defun SUB-LIST(L START LEN)

)

; Problem 5
(defun SPLIT-LIST(L)

)

; Problem 6
(defun BTREE-HEIGHT(TREE)

)

; Problem 7
(defun LIST2BTREE(LEAVES)

)

; Problem 8
(defun BTREE2LIST(TREE)

)

(defun main()
	(TREE-CONTAINS 3 '((1 2 3) 7 8))
) 