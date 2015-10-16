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