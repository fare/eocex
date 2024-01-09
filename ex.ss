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

;; Our AST representation for a whole program
(defstruct ($Program Ast) (info exp) transparent: #t)

;; Is the value a valid source-location, or #f ?
(def (loc? l)
  (or (eq? l #f) (source-location? l)))

(def (read-fixnum (input (current-input-port)))
  (def a (read input))
  (unless (fixnum? a) (error "Not a fixnum" a))
  a)

(def (fx+/o x y)
  (let (z (+ x y))
    (if (fixnum? z) z (error "fixnum + overflow" x y))))
(def fx-/o
  (case-lambda
    ((x y) (let (z (- x y)) (if (fixnum? z) z (error "fixnum - overflow" x y))))
    ((x) (fx-/o 0 x))))

(def (match-args name arity args)
  (def (errlen)
    (error "Bad arity for primitive" name (length args)))
  (match arity
    ((? fixnum?)
     (unless (= arity (length args)) (errlen))
     args)
    ([(? fixnum? min) :: (? fixnum? max)]
     (unless (<= min (length args) max) (errlen))
     args)
    (else (error "Invalid arity" name arity))))
