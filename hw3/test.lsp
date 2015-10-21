(load 'hw3.lsp)

(defmacro unit-test (expr result)
  `(unless (equal ,expr ,result)
     (error (format 'nil "Unit test failed: ~A => ~A, expected ~A" ',expr ,expr, result))))

(unit-test (set-square '((1 1 1 1 1 1 1 1 1)
	   					 (1 0 0 0 1 0 0 0 1)
	   					 (1 0 0 0 2 0 3 4 1)
	   					 (1 0 0 0 1 0 0 0 1)
	   					 (1 0 0 0 1 0 0 0 1)
	   					 (1 1 1 1 1 1 1 1 1)) 2 2 1) '((1 1 1 1 1 1 1 1 1)
								   					   (1 0 0 0 1 0 0 0 1)
								   					   (1 0 1 0 2 0 3 4 1)
								   					   (1 0 0 0 1 0 0 0 1)
								   					   (1 0 0 0 1 0 0 0 1)
								   					   (1 1 1 1 1 1 1 1 1)))

(print "All tests passed!")