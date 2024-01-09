(import
  :std/misc/list
  :std/sugar
  ./ex)

(export #t)

;; From SEXP to AST
(def (Lint/exp<-sexp x)
  (match x
    ((? fixnum?) ($Fixnum #f x))
    ([(? symbol? s) . args]
     ($Prim #f s (map Lint<-sexp args)))
    (else (error "invalid sexp" x))))

(def (Lint<-sexp s) ($Program #f '() (Lint/exp<-sexp s)))

;; From our AST to SEXP
(def (sexp<-Lint/exp a)
  (match a
    (($Fixnum _ (? fixnum? n)) n)
    (($Prim _ op args) `(,op ,@(map sexp<-Lint/exp args)))
    (else (error "invalid Lint exp ast" a))))

(def (sexp<-Lint a)
  (match a
    (($Program _ _ exp) (sexp<-Lint/exp exp))
    (else (error "invalid Lint ast" a))))

;; Recognizer for Lint Ast and Sexp
(def (Lint? a)
  (def l?
    (match <>
      (($Fixnum (? loc?) (? fixnum?)) #t)
      (($Prim (? loc?) op args)
       (alet (af (hash-get Primitives op))
         (and (with-catch false (cut match-args op (car af) args))
              (andmap Lint? args))))
      (else #f)))
  (match a (($Program (? loc?) [] exp) (l? exp))
         (else #f)))

(def (Lint-sexp? s)
  (with-catch false (cut Lint? (Lint<-sexp s))))


;;; Table of Primitives
(def Primitives (hash))
(defrule (defprim name arity fun) (hash-put! Primitives 'name (cons arity fun)))

;; List of Primitives with which to compute
;; Arity is fixed number of arguments (or in the future something richer?)
(defprim - [1 . 2] fx-/o) ;; overflow
(defprim + 2 fx+/o) ;; overflow
(defprim read 0 read-fixnum)


;;; Evaluation

(def (call-prim op args)
  (match (hash-get Primitives op)
    ([arity :: fun]
     (apply fun (match-args op arity args)))
    (#f (error "Unknown primitive" op))))

(def (Lint-eval-ast a)
  (match a
    (($Fixnum _ n) (unless (fixnum? n) (error "not a fixnum" n)) n)
    (($Prim _ op args) (call-prim op (map Lint-eval-ast args)))
    (($Program _ _ exp) (Lint-eval-ast exp))))

(def (Lint-eval s)
  (Lint-eval-ast (Lint<-sexp s)))
