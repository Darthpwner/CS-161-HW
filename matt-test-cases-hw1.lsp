(load 'hw1.lsp)

(defmacro unit-test (expr result)
  `(unless (equal ,expr ,result)
     (error (format 'nil "Unit test failed: ~A => ~A, expected ~A" ',expr ,expr, result))))

;; Problem 1
(unit-test (TREE-CONTAINS 3 '((1 2 3) 7 8)) t)
(unit-test (TREE-CONTAINS 4 '((1 2 3) 7 8)) nil)

;; Problem 2
(unit-test (TREE-MAX '((1 2 3) 7 8)) 8)
(unit-test (TREE-MAX '((1 2 6) 7 (8 9 14))) 14)

;; Problem 3
(unit-test (TREE-ORDER '()) NIL)
(unit-test (TREE-ORDER 3) '(3))
(unit-test (TREE-ORDER '((1 2 3) 7 8)) '(1 2 3 7 8))

;; Problem 4
(unit-test (SUB-LIST '(a b c d) 0 3) '(a b c))
(unit-test (SUB-LIST '(a b c d) 3 1) '(d))
(unit-test (SUB-LIST '(a b c d e f g) 1 0) NIL)
(unit-test (SUB-LIST '(a b c d) 2 0) NIL)

;; Problem 5
(unit-test (SPLIT-LIST '(a b c d))     '((a b) (c d)))
(unit-test (SPLIT-LIST '(a b c d e))   '((a b) (c d e)))
(unit-test (SPLIT-LIST '(a b c d e f)) '((a b c) (d e f)))

;; Problem 6
(unit-test (BTREE-HEIGHT 1)  0)
(unit-test (BTREE-HEIGHT '(1 2))  1)
(unit-test (BTREE-HEIGHT '(1 (2 3)))  2)
(unit-test (BTREE-HEIGHT '((1 2) (3 4)))  2)
(unit-test (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7))))  3)
(unit-test (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8))))  3)

;; Problem 7
(unit-test (LIST2BTREE '(1))  1)
(unit-test (LIST2BTREE '(1 2))  '(1 2))
(unit-test (LIST2BTREE '(1 2 3))  '(1 (2 3)))
(unit-test (LIST2BTREE '(1 2 3 4))  '((1 2) (3 4)))
(unit-test (LIST2BTREE '(1 2 3 4 5 6 7))  '((1 (2 3)) ((4 5) (6 7))))
(unit-test (LIST2BTREE '(1 2 3 4 5 6 7 8))  '(((1 2) (3 4)) ((5 6) (7 8))))

;; Problem 8

(unit-test (BTREE2LIST 1)  '(1))
(unit-test (BTREE2LIST '(1 2))  '(1 2))
(unit-test (BTREE2LIST '(1 (2 3)))  '(1 2 3))
(unit-test (BTREE2LIST '((1 2) (3 4)))  '(1 2 3 4))
(unit-test (BTREE2LIST '((1 (2 3)) ((4 5) (6 7))))  '(1 2 3 4 5 6 7))
(unit-test (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8))))  '(1 2 3 4 5 6 7 8))

;Test cases
(unit-test (TREE-CONTAINS 3 '((1 2 3) 7 8)) t)
(unit-test (TREE-CONTAINS 4 '((1 2 3) 7 8)) NIL)
(unit-test (TREE-CONTAINS 4 '((1 2 3) 7 8)) NIL)
(unit-test (TREE-CONTAINS 99 '((1 2 9) 8 99)) t)

(unit-test (TREE-MAX '((1 2 3) 7 8)) 8)
(unit-test (TREE-MAX '((1 2 3) 7 900)) 900)
(unit-test (TREE-MAX '((1 2 3) 7 81)) 81)
(unit-test (TREE-MAX '((1 2 3) 7 9)) 9)
(unit-test (TREE-MAX '((1 2 3) 7 (10 11 12))) 12)

(unit-test (TREE-ORDER 3) '(3))
(unit-test (TREE-ORDER NIL) NIL)
;(unit-test (TREE-ORDER '(((0 9 11) 12 15) 16 (18 19 20)) ) '(0 9 11 12 15 16 18 19 20)); POTENTIAL BUG

(unit-test (SUB-LIST '(a) 0 1) '(a))
(unit-test (SUB-LIST '(a b c d) 0 1) '(a))
(unit-test (SUB-LIST '(a b c d) 0 2) '(a b))
(unit-test (SUB-LIST '(a b c d) 0 3) '(a b c))
(unit-test (SUB-LIST '(a b c d) 0 4) '(a b c d))
(unit-test (SUB-LIST '(a b c d) 1 1) '(b))
(unit-test (SUB-LIST '(a b c d) 1 2) '(b c))
(unit-test (SUB-LIST '(a b c d) 1 3) '(b c d))
(unit-test (SUB-LIST '(a b c d) 1 4) '(b c d))
(unit-test (SUB-LIST '(a b c d) 2 1) '(c))
(unit-test (SUB-LIST '(a b c d) 2 2) '(c d))
(unit-test (SUB-LIST '(a b c d) 2 3) '(c d))
(unit-test (SUB-LIST '(a b c d) 2 4) '(c d))
(unit-test (SUB-LIST '(a b c d) 2 0) NIL)
(unit-test (SUB-LIST '(a b c d) 3 2) '(d))
(unit-test (SUB-LIST '(a b c d) 4 1) NIL)

(unit-test (SPLIT-LIST '(a))               '(() (a)))
(unit-test (SPLIT-LIST '(a b c d))         '((a b) (c d)))
(unit-test (SPLIT-LIST '(a b c d e))        '((a b) (c d e)))
(unit-test (SPLIT-LIST '(a b c d e f))      '((a b c) (d e f)))     
(unit-test (SPLIT-LIST '(a b c d e f g))    '((a b c) (d e f g)))
(unit-test (SPLIT-LIST '(a b c d e f g h))  '((a b c d) (e f g h)))
(unit-test (SPLIT-LIST '(a b c d e f g 1 )) '((a b c d) (e f g 1)))
(unit-test (SPLIT-LIST '(a b c d e f g 0 9)) '((a b c d) (e f g 0 9)))
(unit-test (SPLIT-LIST '(a b c d e f g 0 9 69))     '((a b c d e) (f g 0 9 69)))
(unit-test (SPLIT-LIST '(a b c d 9 f g ij ab 11 0 9 69))   '((a b c d 9 f)(g ij ab 11 0 9 69)))

(unit-test (BTREE-HEIGHT 1) 0)
(unit-test (BTREE-HEIGHT '(1 2)) 1)
(unit-test (BTREE-HEIGHT '(1 (2 3))) 2)
(unit-test (BTREE-HEIGHT '((1 2) (3 4))) 2)
(unit-test (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7)))) 3)
(unit-test (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8)))) 3)

(print "All tests passed!")