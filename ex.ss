(import
  <expander-runtime>
  (phi: -1 :gerbil/core)
  :gerbil/core
  :gerbil/expander
  :std/debug/DBG
  :std/misc/func
  :std/misc/list
  :std/srfi/1
  :std/srfi/141
  :std/sugar)

(export #t) ;; for now

;;; Primitives
(def Primitives (hash))
(defrule (defprim name args ...)
  (hash-put! Primitives 'name (list args ...)))

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

(def (call-prim op args)
  (match (hash-get Primitives op)
    ([arity fun]
     (apply fun (match-args op arity args)))
    (#f (error "Unknown primitive" op))))

;; Our AST representation
(defstruct Ast
  (loc) ;; #f or Gambit-style location
  transparent: #t)

(defclass Constructor ;; syntax with compile-time type C for runtime type T
  (name ;; Symbol
   syntax-type ;; TypeDescriptor<C>
   make-syntax ;; (Fun C <- Inputs ...)
   syntax? ;; (Fun Bool <- Any)
   fields ;; (List Symbol)
   parse-head ;; (Or Symbol (Fun Bool <- Stx))
   parse-rec ;; (Fun A <- (Fun A <- Fields ...) (Fun Field <- Stx) Stx ...)
   unparse ;; (Fun Sexp <- (Fun Sexp <- Field) Fields ...)
   runtime-inputs-valid? ;; (Fun Bool <- Inputs ...)
   kons ;; (Fun T <- ValidInputs ...)
   runtime-value? ;; (Or (Fun Bool <- Any) TypeDescriptor)
   dekons) ;; (Fun A <- (Fun A <- Fields ...) T)
  transparent: #t)
(def Constructors (hash))

(def (register-constructor name . args)
  (hash-put! Constructors name (apply make-Constructor name: name args)))

(defrule (defconstructor (Name fields ...)
           parse-head ;; (Or Symbol (Fun Bool <- Sexp))
           parse-rec ;; (Fun A <- (Fun A <- Fields ...) (Fun Field <- Sexp) Sexp ...)
           unparse ;; (Fun Sexp <- Fields ...)
           runtime-inputs-valid? ;; (Fun Bool <- Inputs ...)
           kons ;; (Fun T <- ValidInputs ...)
           runtime-value? ;; (Or (Fun Bool <- Any) TypeDescriptor)
           dekons) ;; (Fun A <- (Fun A <- Fields ...) T)
  (with-id defconstructor
      ((Name::t #'Name "::t")
       (Name? #'Name "?")
       (runtime "runtime-" #'Name)
       (make-Name "make-" #'Name))
    (begin
      (defstruct (Name Ast) (fields ...) transparent: #t)
      (def (apply-f e f) (with ((Name ast fields ...) e) (f fields ...)))
      (defmethod {:apply Name} apply-f)
      (defmethod {:unparse Name}
        (lambda (x) (make-AST (apply-f x unparse) (Ast-loc x))))
      (register-constructor 'Name
        syntax-type: Name::t
        make-syntax: make-Name
        syntax?: Name?
        fields: '(fields ...)
        parse-head: parse-head
        parse-rec: parse-rec
        unparse: unparse
        runtime-inputs-valid?: runtime-inputs-valid?
        kons: kons
        runtime-value?: runtime-value?
        dekons: dekons))))

(def (Schema<-stx/constructor K)
  (lambda (r x)
    (and ((Constructor-parse-head K) (stx-e x))
         ((Constructor-parse-rec K)
          (lambda a (apply (Constructor-make-syntax K) (stx-source x) a))
          r x))))

(def (Schema<-stx constructors)
  (def fs (map (compose Schema<-stx/constructor (cut hash-get Constructors <>))
               constructors))
  (def (S x) (ormap (cut <> S x) fs))
  S)

(def (Program<-stx constructors)
  (let (S (Schema<-stx constructors))
    (lambda (s) (let (exp (S s)) (Program (stx-source s) '() exp)))))

(def (stx<-ast x) {:unparse x})

;; Our AST representation for a whole program
(defstruct (Program Ast) (info exp) transparent: #t)

;; Is the value a valid source-location, or #f ?
(def (loc? l)
  (or (eq? l #f) (source-location? l)))

;; #f is unknown (defaults to the other)
(def (minpos p1 p2)
  (cond
    ((not p1) p2)
    ((not p2) p1)
    ((and (negative? p1) (negative? p2)) (max p1 p2))
    ((negative? p1) p2)
    ((negative? p2) p1)
    (else (let-values (((c1 l1) (floor/ p1 65536))
                       ((c2 l2) (floor/ p2 65536)))
            (if (or (< l1 l2) (and (= l1 l2) (<= c1 c2)))
              l1 l2)))))

;; #f is unknown (defaults to the other)
(def (maxpos p1 p2)
  (cond
    ((not p1) p2)
    ((not p2) p1)
    ((and (negative? p1) (negative? p2)) (min p1 p2))
    ((negative? p1) p1)
    ((negative? p2) p2)
    (else (let-values (((c1 l1) (floor/ p1 65536))
                       ((c2 l2) (floor/ p2 65536)))
            (if (or (< l1 l2) (and (= l1 l2) (<= c1 c2)))
              l2 l1)))))

;; Join locations
(def (join-loc l1 l2)
  (match l1
    (#f l2)
    ((vector src1 start1 end1)
     (match l2
       (#f l1)
       ((vector src2 start2 end2)
        (if (equal? src1 src2)
          (vector src1 (minpos start1 start2) (maxpos end1 end2))
          (vector src1 start1 end1)))))))

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
