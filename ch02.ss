;;; Our take on EOC Chapter 2
(import
  <expander-runtime>
  (phi: -1 :gerbil/core)
  :gerbil/expander
  :gerbil/core
  :std/misc/list
  :std/sugar
  :eocex/ex
  :eocex/ch01)

(export #t)

;;; Primitives

;;; Syntactic Constructors

(def Var-args? (list?<- symbol?))

(defconstructor (Var name) ;; Var id ?
  symbol? ;; parse-head ;; (Or Symbol (Fun Bool <- Sexp))
  kons-immediate ;; parse-rec ;; (Fun A <- (Fun A <- Fields ...) (Fun Field <- Sexp) Sexp ...)
  identity ;; unparse ;; (Fun Sexp <- Fields ...)
  Var-args? ;; runtime-inputs-valid? ;; (Fun Bool <- Inputs ...)
  identity ;; kons ;; (Fun T <- ValidInputs ...)
  symbol? ;; runtime-value? ;; (Or (Fun Bool <- Any) TypeDescriptor)
  dekons-immediate) ;; dekons ;; (Fun A <- (Fun A <- Fields ...) T)

(defconstructor (Let1 x v body)
  (lambda (e) (syntax-case e (let) ((let (x v) body ...) (identifier? #'x) #t) (else #f)))
  (lambda (kons rec e)
    (syntax-case e (let)
      ((let (x v) body ...) (identifier? #'x)
       (kons (rec #'x) (rec #'v) (stx-map rec #'(body ...))))))
  (lambda (x v body) `(let (,(unparse x) ,(unparse v))) ,@(map unparse body))
  false false)

(defconstructor (LetN bindings body)
  (lambda (e) (syntax-case e (let) ((let ((x v) ...) body ...) #t) (else #f)))
  (lambda (kons rec e)
    (syntax-case e (let)
      ((let ((x v) ...) body ...)
       (kons (stx-map (cut stx-map rec <>) #'(x ...) #'(v ...)) (stx-map rec #'(body ...))))))
  (lambda (x v body) `(let (,(unparse x) ,(unparse v))) ,@(map unparse body))
  false false)

;;; Parsing

;; From SEXP to our AST
(def Lvar<-sexp (Program<-sexp '(Fixnum Prim Var Let1 LetN)))

;; Need all dependencies defined at the right Phi:
;;(defsyntax (Lvar s) (datum->syntax s (Lvar<-sexp s)))

;; From our AST to SEXP
(def (stx<-Lvar a)
  (match a
    ((Program _ _ exp) (unparse exp))
    (else (error "invalid Lvar ast" a))))

(def (sexp<-Lvar a) (syntax->datum (stx<-Lvar a)))

;; Recognizer for Lvar Ast and Sexp
(def (Lvar-ast? a)
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

(def (Lvar-sexp? s)
  (with-catch false (cut Lvar? (Lvar<-sexp s))))

(def (Lvar? x)
  (if (Ast? x) (Lvar-ast? x) (Lvar-sexp? x)))

;;; Evaluation
(def (call-prim op args)
  (match (hash-get Primitives op)
    ([arity fun]
     (apply fun (match-args op arity args)))
    (#f (error "Unknown primitive" op))))

(def (Lvar-eval a)
  (match a
    ((Fixnum _ n) (unless (fixnum? n) (error "not a fixnum" n)) n)
    ((Prim _ op args) (call-prim op (map Lvar-eval args)))
    ((Program _ _ exp) (Lvar-eval exp))))

(def (Lvar/eval s)
  (Lvar-eval (Lvar<-sexp s)))


;;; Partial Evaluation
(def (Lvar-pe a)
  (match a
    ((Fixnum _ _) a)
    ((Prim loc op args)
     (let (pargs (map Lvar-pe args))
       (cond
        ((and (not (eq? op 'read)) (andmap Fixnum? pargs)
              (ignore-errors (Fixnum loc (call-prim op (map Fixnum-value pargs))))) => identity)
        ((andmap eq? args pargs)
         a)
        (else
         (Prim loc op pargs)))))
    ((Program loc info exp)
     (Program loc info (Lvar-pe exp)))))

(def (Lvar/pe s)
  (sexp<-Lvar (Lvar-pe (Lvar<-sexp s))))
