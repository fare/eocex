(import
  :std/sugar
  :std/test
  ./primitives
  )

(export primitives-test)

(def primitives-test
  (test-suite "primitives"
    (test-case "add_test"
      (parameterize ((Primitives (hash)))
	(defprim add 2 (lambda (x y) (modulo (+ x y) 10)))
	(check (call-prim 'add [7 8]) => 5)
	(check-exception (call-prim 'add [7 6 5]) true)
	(check-exception (call-prim 'sub [4 3]) true)
	(parameterize ((Primitives (hash)))
	  (check-exception (call-prim 'add [7 8]) true))
	(check (call-prim 'add [7 8]) => 5)
	))
    (test-case "match-args_test"
      (check (match-args 'a 2 [3 4]) => [3 4])
      (check (match-args 'a [2 . 4] [9 8 7]) => [9 8 7])
      (check-exception (match-args 'a "X" [0]) true)
      (check-exception (match-args 'a [2 . 4] [0]) true)
      (check (match-args 3 1 [1]) => [1]) 
      )
    ))
