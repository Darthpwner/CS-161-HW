; The code first checks if there is just one element in the tree and if that is the answer needed. 
;If not then it compares the middle number with the number we are looking for and if the middle element is bigger then we check the left subtree,
;otherwise we check the right
;A1
(defun TREE-CONTAINS (N TREE) ; N is the number we are searching for and T is an ordered TREE
	(cond ((atom TREE) smile emoticon N TREE))
		   ((= (second TREE) N) T)
		   ((> (second TREE) N)(TREE-CONTAINS N (first TREE)))
		   ((< (second TREE) N)(TREE-CONTAINS N (third TREE)))

	)

)



;We check that if there is only one element in the Tree and if so then it is the max
;Otherwise we recursively go to the left most element in the list and the last atom is the max
;A2
(defun TREE-MAX(TREE); TREE is an ordered list being passed
	;
(cond   ((atom TREE) TREE)
		(t (TREE-MAX (third TREE)))
	)
)


;The base case is that if the Tree is just one atom then we construct a list with just that atom.
;The recursive case uses append to consturct a list with the first list, the middle atom and the third list. 
;A3
(defun TREE-ORDER(TREE); TREE is an ordered list we are passing
(cond ((atom TREE)(cons TREE '()))
 	  (t (append (TREE-ORDER (first TREE)) (cons(second TREE) '()) (TREE-ORDER (third TREE))))
)
)



;The base case checks stops the recursion and returns NIl meaning we have reached the end of the construction of our new list.
;If start is 0 then we start constructing a new list and recursively call SUB-LIST with decreasing value of lenght
;We call SUB-LIST without the first element and decreasing value of Start to eventually reach the point to start constructing a new list
;A4
(defun SUB-LIST(L START LEN) ; L is the List being passed, START is the position in the list we start constructing the new list from and LEN is the length of the new list.
	(cond 
		( smile emoticon LEN 0) NIL) 
		( smile emoticon START 0) (append (cons (car L) '()) (SUB-LIST (cdr L) START (- LEN 1) ) ) ); Start counting the SUB-LIST
		( t (SUB-LIST(cdr L) (- START 1) LEN) )
	)
)



;If there is only element at a certain level then i's height doesn't matter
;If there are 2 atoms at a level then you return 1 
;we set 2 variables to either branches of the tree and compare them and only the branch that goes deeper is whose height matters and we increment the variables accordingly
;A6
(defun BTREE-HEIGHT (TREE); TREE is a binary Tree being passed
	(cond ((atom TREE) 0)
			((and (atom (first TREE)) (atom (second TREE))) 1)
			(t (let* ((h1 (BTREE-HEIGHT ( second TREE ))  )
					  (h2 (BTREE-HEIGHT ( first TREE  ))  )

					)
					  (cond ((> h1 h2)( + 1 h1))
		              (t (+ 1 h2))
					)		
				)
			)
	)

)
;The first check is to avoid NIL elements
;If we come across an atom we construct a list with it as the only element
;We append the first element of the list to the rest of the list 
(defun BTREE2LIST (TREE)
(cond  ((NULL TREE) NIL)
	   ((atom TREE) (cons TREE '()))
	   (t (append (BTREE2LIST ( first TREE)) (BTREE2LIST (rest TREE))))
)
)