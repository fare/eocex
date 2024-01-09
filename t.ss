;; Trivial test

(import :std/test)
(import :eocex/ex)

(check (EvalSexp '(+ 4 5)) => 9)

#|
(check (match foo (['Int n] n)) => 4)

(displayln (syntax a))
(def x (syntax a))
(check (syntax->datum x) => 'a)
(check (AST? x) => #t)
(check (AST-e x) => 'a)
|#

