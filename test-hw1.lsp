(load 'hw1.lsp)
(defmacro unit-test (expr result)
  `(unless (equal ,expr ,result)
     (error (format 'nil "Unit test failed: ~A => ~A, expected ~A" ',expr ,expr, result))))
;; Problem 1
;(unit-test (TREE-CONTAINS 3 '((1 2 3) 7 8)) t)
(unit-test (TREE-CONTAINS 4 '((1 2 3) 7 8)) nil)
;; Problem 2 GOOD
(unit-test (TREE-MAX '((1 2 3) 7 8)) 8)
;; Problem 3 GOOD
(unit-test (TREE-ORDER 3) '(3))
(unit-test (TREE-ORDER '((1 2 3) 7 8)) '(1 2 3 7 8))
;; Problem 4
;(unit-test (SUB-LIST '(a b c d) 0 3) '(a b c))
;(unit-test (SUB-LIST '(a b c d) 3 1) '(d))
;(unit-test (SUB-LIST '(a b c d) 2 0) NIL)
;; Problem 5
;(unit-test (SPLIT-LIST '(a b c d))     '((a b) (c d)))
;(unit-test (SPLIT-LIST '(a b c d e))   '((a b) (c d e)))
;(unit-test (SPLIT-LIST '(a b c d e f)) '((a b c) (d e f)))
;; Problem 6 GOOD
(unit-test (BTREE-HEIGHT 1)  0)
(unit-test (BTREE-HEIGHT '(1 2))  1)
(unit-test (BTREE-HEIGHT '(1 (2 3)))  2)
(unit-test (BTREE-HEIGHT '((1 2) (3 4)))  2)
(unit-test (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7))))  3)
(unit-test (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8))))  3)
;; Problem 7
;(unit-test (LIST2BTREE '(1))  1)
;(unit-test (LIST2BTREE '(1 2))  '(1 2))
;(unit-test (LIST2BTREE '(1 2 3))  '(1 (2 3)))
;(unit-test (LIST2BTREE '(1 2 3 4))  '((1 2) (3 4)))
;(unit-test (LIST2BTREE '(1 2 3 4 5 6 7))  '((1 (2 3)) ((4 5) (6 7))))
;(unit-test (LIST2BTREE '(1 2 3 4 5 6 7 8))  '(((1 2) (3 4)) ((5 6) (7 8))))
;; Problem 8
;(unit-test (BTREE2LIST 1)  '(1))
;(unit-test (BTREE2LIST '(1 2))  '(1 2))
;(unit-test (BTREE2LIST '(1 (2 3)))  '(1 2 3))
;(unit-test (BTREE2LIST '((1 2) (3 4)))  '(1 2 3 4))
;(unit-test (BTREE2LIST '((1 (2 3)) ((4 5) (6 7))))  '(1 2 3 4 5 6 7))
;(unit-test (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8))))  '(1 2 3 4 5 6 7 8))
(print "All tests passed!")