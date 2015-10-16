(load 'hw2.lsp)

(defmacro unit-test (expr result)
  `(unless (equal ,expr ,result)
     (error (format 'nil "Unit test failed: ~A => ~A, expected ~A" ',expr ,expr, result))))

;; Problem 1
(unit-test (DFS '((A (B)) C (D))) '(A B C D))
(unit-test (DFS '((3 4 5) 6 ((9) 10))) '(3 4 5 6 9 10))
(unit-test (DFS '((A (B)) C (D))) '(A B C D))
(unit-test (DFS '(A B)) '(A B))
(unit-test (DFS '((W X) (Y Z))) '(W X Y Z))
(unit-test (dfs NIL) NIL)
(unit-test (dfs '((((A))))) '(A))
(unit-test (dfs '((A) (((B))) (C (D (E F (G)))))) '(A B C D E F G))

;; Problem 2
(unit-test (DFID '((A (B)) C (D)) 3) '(C A C D A B C D))
(unit-test (ldfs '((A (B)) C (D)) 0) NIL)
(unit-test (ldfs '((A (B)) C (D)) 1) '(C))
(unit-test (ldfs '((A (B)) C (D)) 2) '(A C D))
(unit-test (ldfs '((A (B)) C (D)) 3) '(A B C D))
(unit-test (dfid '((A (B)) C (D)) 0) NIL)
(unit-test (dfid '((A (B)) C (D)) 1) '(C))
(unit-test (dfid '((A (B)) C (D)) 2) '(C A C D))
(unit-test (dfid '((A (B)) C (D)) 3) '(C A C D A B C D))
(unit-test (dfid '((W X) (Y Z)) 1) NIL)
(unit-test (dfid '((W X) (Y Z)) 2) '(W X Y Z))
(unit-test (dfid '((W X) (Y Z)) 3) '(W X Y Z W X Y Z))

;; Problem 3 sub-problem testing
(unit-test (final-state '(3 3 NIL)) t)
(unit-test (final-state '(a)) NIL)
(unit-test (final-state '(3 3 T)) NIL)
(unit-test (final-state '(3 2 NIL)) NIL)
(unit-test (final-state '(3 3 NIL)) T)

(unit-test (next-state '(3 3 t) 1 0) NIL)
(unit-test (next-state '(3 3 t) 0 1) '((0 1 NIL)))
(unit-test (next-state '(2 2 t) 0 2) NIL)
(unit-test (next-state '(3 3 NIL) 1 1) '((1 1 t)))
(unit-test (next-state '(1 1 NIL) 1 0) '((3 2 t)))

(unit-test (on-path '(1 1 t) '((1 1 t))) t)
(unit-test (on-path '(1 1 t) '((1 1 NIL))) NIL)
(unit-test (on-path '(3 3 t) '((1 1 NIL) (0 0 t) (3 3 t) (2 2 NIL))) T)

(unit-test (mult-dfs (succ-fn '(0 2 t)) NIL) '((3 2 NIL) (1 1 T) (3 3 NIL)))

(unit-test (mc-dfs '(0 2 t) NIL) '((0 2 T) (3 2 NIL) (1 1 T) (3 3 NIL)))
(unit-test (mc-dfs '(3 3 t) NIL) '((3 3 T) (1 1 NIL) (3 2 T) (0 3 NIL) (3 1 T) (2 2 NIL) (2 2 T) (3 1 NIL) (0 3 T) (3 2 NIL) (1 1 T) (3 3 NIL)))


(print "All tests passed!")