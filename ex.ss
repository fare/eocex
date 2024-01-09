(import
  :std/misc/list
  :std/sugar)

(export #t) ;; for now

;;; Our take on Chapter 1 -- move to one file per chapter, plus files for reusable stuff?

;; Our AST representation
(defstruct Ast
  (loc)
  transparent: #t) ;; #f or Gambit-style location

;; Our AST representation for an immediate fixnum
;; fixnum? from ##min-fixnum to ##max-fixnum
(defstruct ($Fixnum Ast) (value) transparent: #t)

;; Our AST representation for a primitive operation
(defstruct ($Prim Ast) (op args) transparent: #t)

;; From SEXP to AST
(def (sexp->Ast x)
  (match x
    ((? fixnum?) ($Fixnum #f x))
    ([(? symbol? s) . args]
     ($Prim #f s (map sexp->Ast args)))))

;; From our AST to SEXP
(def (Ast->sexp a)
  (match a
    (($Fixnum _ n) n)
    (($Prim _ op args) `(,op ,@(map Ast->sexp args)))))

;; List of Primitives with which to compute
;; Arity is fixed number of arguments (or in the future something richer?)
(def Primitives (hash))
(defrule (defprim name arity fun)
  (hash-put! Primitives 'name (cons arity fun)))
(defprim - 2 ##fx-)
(defprim + 2 ##fx+)

(def (call-prim name args)
  (match (hash-get Primitives name)
    ([arity :: fun]
     (unless (= arity (length args)) ;; TODO: richer arities
       (error "Bad arity for primitive" name (length args)))
     (apply fun args))
    (#f (error "Unknown primitive" name))))

(def (EvalAst a)
  (match a
    (($Fixnum _ n) n)
    (($Prim _ op args) (call-prim op (map EvalAst args)))))

(def (EvalSexp s)
  (EvalAst (sexp->Ast s)))
