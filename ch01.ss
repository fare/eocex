;;; Our take on EOC Chapter 1
(import
  <expander-runtime>
  (phi: -1 :gerbil/core)
  :gerbil/expander
  :gerbil/core
  :std/misc/list
  :std/srfi/1
  :std/sugar
  ;;:clan/poo/object :clan/poo/brace
  :eocex/ex)

(export #t)

;;; Primitives

;; List of Primitives with which to compute
;; Arity is fixed number of arguments (or in the future something richer?)
(defprim - [1 . 2] fx-/o) ;; overflow
(defprim + 2 fx+/o) ;; overflow
(defprim read 0 read-fixnum)

;;; Syntactic Constructors

(def (kons-immediate kons _ e) (kons (stx-e e)))
(def Fixnum-args? (list?<- fixnum?))
(def (dekons-immediate kons v) (kons v))

(defconstructor (Fixnum value)
  fixnum? ;; parse-head ;; (Or Symbol (Fun Bool <- Stx))
  kons-immediate ;; parse-rec ;; (Fun A <- (Fun A <- Fields ...) (Fun Field <- Stx) Stx ...)
  identity ;; unparse ;; (Fun Sexp <- Fields ...)
  Fixnum-args? ;; runtime-inputs-valid? ;; (Fun Bool <- Inputs ...)
  identity ;; kons ;; (Fun T <- ValidInputs ...)
  fixnum? ;; runtime-value? ;; (Or (Fun Bool <- Any) TypeDescriptor)
  dekons-immediate) ;; dekons ;; (Fun A <- (Fun A <- Fields ...) T)

(defconstructor (Prim op args)
  (match <> ([o . _] (hash-key? Primitives (stx-e o))) (else #f))
  (lambda (kons rec e)
    (syntax-case e ()
      ((o . a) (identifier? #'o)
       (let (op (stx-e #'o))
         (alet (af (hash-get Primitives op))
           (kons op (match-args op (car af) (map rec (syntax->list #'a)))))))
      (else #f)))
  (lambda (op args) (cons op (map unparse args)))
  true ;; TODO: have input validator in Primitives
  (lambda (o a) (apply (second (hash-get Primitives o)) a))
  false
  false)

;;; Parsing

;; From SEXP to our AST
(def Lint<-stx (Program<-stx '(Fixnum Prim)))

;; Need all dependencies defined at the right Phi:
;;(defsyntax (Lint s) (datum->syntax s (Lint<-stx s)))

;; From our AST to SEXP
(def (stx<-Lint a)
  (match a
    ((Program _ _ exp) (unparse exp))
    (else (error "invalid Lint ast" a))))

(def (sexp<-Lint a) (syntax->datum (stx<-Lint a)))

;; Recognizer for Lint Ast and Sexp
(def (Lint-ast? a)
  (def l?
    (match <>
      ((Fixnum (? loc?) (? fixnum?)) #t)
      ((Prim (? loc?) op args)
       (alet (af (hash-get Primitives op))
         (and (with-catch false (cut match-args op (car af) args))
              (andmap l? args))))
      (else #f)))
  (match a ((Program (? loc?) [] exp) (l? exp))
         (else #f)))

(def (Lint-sexp? s)
  (with-catch false (cut Lint? (Lint<-stx s))))

(def (Lint? x)
  (if (Ast? x) (Lint-ast? x) (Lint-sexp? x)))

;;; Evaluation
(def (Lint-eval eval env a)
  (match a
    ((Fixnum _ n) (unless (fixnum? n) (error "not a fixnum" n)) n)
    ((Prim _ op args) (call-prim op (map (cut eval eval env <>) args)))
    ((Program _ _ exp) (eval eval env exp))))

(def (Lint/eval eval env s)
  (Lint-eval eval env (Lint<-stx s)))


;;; Partial Evaluation
(def (Lint-pe a)
  (match a
    ((Fixnum _ _) a)
    ((Prim loc op args)
     (let (pargs (map Lint-pe args))
       (cond
        ((and (not (eq? op 'read)) (andmap Fixnum? pargs)
              (ignore-errors (Fixnum loc (call-prim op (map Fixnum-value pargs))))) => identity)
        ((andmap eq? args pargs)
         a)
        (else
         (Prim loc op pargs)))))
    ((Program loc info exp)
     (Program loc info (Lint-pe exp)))))

(def (Lint/pe s)
  (sexp<-Lint (Lint-pe (Lint<-stx s))))
