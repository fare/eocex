;; Trivial test

(import :std/test)
(import :eocex/ex)

(check (Lint-Sexp? '(+ (read) (- 8))) => #t)
(check (Lint-Sexp? '(+ 4 5)) => #t)
(check (Lint-Sexp? '(- 4 5)) => #t)

(check (Lint-Sexp? '(+ (read 1) (- 8))) => #f)
(check (Lint-Sexp? '(* (read) (- 8))) => #f)
(check (Lint-Sexp? '(+ 4 5 6)) => #f)
(check (Lint-Sexp? '(- 4 5 7)) => #f)

(check (EvalSexp '(+ 10 32)) => 9)
(check (EvalSexp '(+ 10 (- (+ 12 20)))) => -22)

(check-exception (EvalSexp

#|
(check (match foo (['Int n] n)) => 4)

(displayln (syntax a))
(def x (syntax a))
(check (syntax->datum x) => 'a)
(check (AST? x) => #t)
(check (AST-e x) => 'a)
|#

