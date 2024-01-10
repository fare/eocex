(import :std/test)
(import ./ex ./ch01)
(export ch01-test)

(def ch01-test
  (test-suite "ch01"
    (test-case "Lint<-sexp"
      (check (Lint<-sexp '(+ (read) (- 10 (- 5)))) =>
             ($Program #f '() ($Prim #f '+
                                     [($Prim #f 'read '())
                                      ($Prim #f '- [($Fixnum #f 10)
                                                    ($Prim #f '- [($Fixnum #f 5)])])])))
      (check (Lint<-sexp '(+ (read) (- 8))) =>
             ($Program #f '() ($Prim #f '+
                                     [($Prim #f 'read '())
                                      ($Prim #f '- [($Fixnum #f 8)])]))))
    (test-case "Lint-sexp?"
      (check (Lint-sexp? '(+ 4 5)) => #t)
      (check (Lint-sexp? '(- 4 5)) => #t)
      (check (Lint-sexp? '(+ (read) (- 8))) => #t)

      (check (Lint-sexp? '(+ (read 1) (- 8))) => #f)
      (check (Lint-sexp? '(* (read) (- 8))) => #f)
      (check (Lint-sexp? '(+ 4 5 6)) => #f)
      (check (Lint-sexp? '(- 4 5 7)) => #f))
    (test-case "Lint/eval"
      (check (Lint/eval '(+ 10 32)) => 42)
      (check (Lint/eval '(+ 10 (- (+ 12 20)))) => -22)
      (def n 999999999999999999)
      (check-exception (Lint/eval `(+ (+ (+ ,n ,n) (+ ,n ,n)))) true))
    (test-case "Lint/pe"
      (check (Lint/pe '(+ 10 (- (+ 5 3)))) => 2)
      (check (Lint/pe '(+ 1 (+ 3 1))) => 5)
      (check (Lint/pe '(- (+ 3 (- 5)))) => 2)
      (check (Lint/pe '(- (+ 3 (- 5)) (read))) => '(- -2 (read))))))
