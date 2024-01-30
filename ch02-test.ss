(import
  :std/sugar
  :std/test
  ./ex
  ./ch02)
(export ch02-test)

(def ch02-test
  (test-suite "ch02"
    #;
    (test-case "Lvar<-stx"
      (check (Lvar<-stx 42) => (Program #f '() (Fixnum #f 42)))
      (check (Lvar<-stx '(+ (read) (- 10 (- 5)))) =>
             (Program #f '() (Prim #f '+
                                     [(Prim #f 'read '())
                                      (Prim #f '- [(Fixnum #f 10)
                                                    (Prim #f '- [(Fixnum #f 5)])])])))
      (check (Lvar<-stx '(+ (read) (- 8))) =>
             (Program #f '() (Prim #f '+
                                     [(Prim #f 'read '())
                                      (Prim #f '- [(Fixnum #f 8)])]))))
    #;
    (test-case "Lvar?"
      (check (Lvar? '(+ 4 5)) => #t)
      (check (Lvar? '(- 4 5)) => #t)
      (check (Lvar? '(+ (read) (- 8))) => #t)

      (check (Lvar? '(+ (read 1) (- 8))) => #f)
      (check (Lvar? '(* (read) (- 8))) => #f)
      (check (Lvar? '(+ 4 5 6)) => #f)
      (check (Lvar? '(- 4 5 7)) => #f))
    #;
    (test-case "Lvar/eval and Lvar/pe"
      (check (Lvar/pe '(- (+ 3 (- 5)) (read))) => '(- -2 (read)))
      (def n 999999999999999999)
      (def overflow `(+ (+ (+ ,n ,n) (+ ,n ,n)) (+ (+ ,n ,n) (+ ,n ,n))))
      (check-exception (Lvar/eval overflow) true)
      (check (Lvar/pe overflow) => '(+ (+ 1999999999999999998 1999999999999999998) (+ 1999999999999999998 1999999999999999998)))
      (defrule (begin/s body ...)
        (call-with-input-string "10 22 42" (lambda (p) (parameterize ((current-input-port p)) body ...))))
      (defrule (Lvar/e exp) (begin/s (Lvar/eval exp)))
      (defrule (checks (expr chk ...) ...)
        (begin
          (check (Lvar? 'expr) => #t) ...
          (check (sexp<-Lvar (Lvar<-stx 'expr)) => 'expr) ...
          (check (Lvar/e 'expr) chk ...) ...
          (check (Lvar/e (Lvar/pe 'expr)) chk ...) ...))
      (checks ((let ((x 32)) (+ (let ((x 10)) x) x)) => 42)
              ((let ((x (read))) (+ (let ((y (read))) (+ x ( - y))))) => -12)
              ((+ 10 32) => 42)
              ((+ 10 (- (+ 12 20))) => -22)
              ((+ 10 (- (+ 5 3))) => 2)
              ((+ 1 (+ 3 1)) => 5)
              ((- (+ 3 (- 5))) => 2)
              ((+ (read) (- 8)) => 2)
              ((- (+ (read) (read)) (read)) => -10)))))
