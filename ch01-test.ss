(import
  :std/sugar
  :std/test
  ./ex
  ./ch01)
(export ch01-test)

(def ch01-test
  (test-suite "ch01"
    (test-case "Lint<-stx"
      (check (Lint<-stx 42) => (Program #f '() (Fixnum #f 42)))
      (check (Lint<-stx '(+ (read) (- 10 (- 5)))) =>
             (Program #f '() (Prim #f '+
                                     [(Prim #f 'read '())
                                      (Prim #f '- [(Fixnum #f 10)
                                                    (Prim #f '- [(Fixnum #f 5)])])])))
      (check (Lint<-stx '(+ (read) (- 8))) =>
             (Program #f '() (Prim #f '+
                                     [(Prim #f 'read '())
                                      (Prim #f '- [(Fixnum #f 8)])]))))
    (test-case "Lint?"
      (check (Lint? '(+ 4 5)) => #t)
      (check (Lint? '(- 4 5)) => #t)
      (check (Lint? '(+ (read) (- 8))) => #t)

      (check (Lint? '(+ (read 1) (- 8))) => #f)
      (check (Lint? '(* (read) (- 8))) => #f)
      (check (Lint? '(+ 4 5 6)) => #f)
      (check (Lint? '(- 4 5 7)) => #f))
    (test-case "Lint/eval and Lint/pe"
      (check (Lint/pe '(- (+ 3 (- 5)) (read))) => '(- -2 (read)))
      (def n 999999999999999999)
      (def overflow `(+ (+ (+ ,n ,n) (+ ,n ,n)) (+ (+ ,n ,n) (+ ,n ,n))))
      (check-exception (Lint/eval Lint-eval '() overflow) true)
      (check (Lint/pe overflow) => '(+ (+ 1999999999999999998 1999999999999999998) (+ 1999999999999999998 1999999999999999998)))
      (defrule (begin/s body ...)
        (call-with-input-string "10 22 42" (lambda (p) (parameterize ((current-input-port p)) body ...))))
      (defrule (Lint/e exp) (begin/s (Lint/eval Lint-eval '() exp)))
      (defrule (checks (expr chk ...) ...)
        (begin
          (check (Lint? 'expr) => #t) ...
          (check (sexp<-Lint (Lint<-stx 'expr)) => 'expr) ...
          (check (Lint/e 'expr) chk ...) ...
          (check (Lint/e (Lint/pe 'expr)) chk ...) ...))
      (checks ((+ 10 32) => 42)
              ((+ 10 (- (+ 12 20))) => -22)
              ((+ 10 (- (+ 5 3))) => 2)
              ((+ 1 (+ 3 1)) => 5)
              ((- (+ 3 (- 5))) => 2)
              ((+ (read) (- 8)) => 2)
              ((- (+ (read) (read)) (read)) => -10)))))
