(import
  :std/misc/list
  :std/sugar
  :eocex/ex)

(export #t)

;;; Primitives
(def Primitives (hash))
(defrule (defprim name arity fun) (hash-put! Primitives 'name (cons arity fun)))

;; List of Primitives with which to compute
;; Arity is fixed number of arguments (or in the future something richer?)
(defprim - [1 . 2] fx-/o) ;; overflow
(defprim + 2 fx+/o) ;; overflow
(defprim read 0 read-fixnum)


;;; Parsing

;; From SEXP to AST
(def (Lint/exp<-sexp x)
  (match x
    ((? fixnum?) ($Fixnum #f x))
    ([(? symbol? op) . args]
     (cond ((hash-get Primitives op)
            => (lambda (af)
                 (if (with-catch false (cut match-args op (car af) args))
                   ($Prim #f op (map Lint/exp<-sexp args))
                   (error "invalid arity" x))))
           (else (error "invalid primitive" x))))
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
              (andmap l? args))))
      (else #f)))
  (match a (($Program (? loc?) [] exp) (l? exp))
         (else #f)))

(def (Lint-sexp? s)
  (with-catch false (cut Lint? (Lint<-sexp s))))

;;; Evaluation
(def (call-prim op args)
  (match (hash-get Primitives op)
    ([arity :: fun]
     (apply fun (match-args op arity args)))
    (#f (error "Unknown primitive" op))))

(def (Lint-eval a)
  (match a
    (($Fixnum _ n) (unless (fixnum? n) (error "not a fixnum" n)) n)
    (($Prim _ op args) (call-prim op (map Lint-eval args)))
    (($Program _ _ exp) (Lint-eval exp))))

(def (Lint/eval s)
  (Lint-eval (Lint<-sexp s)))


;;; Partial Evaluation
(def (Lint-pe a)
  (match a
    (($Fixnum _ _) a)
    (($Prim loc op args)
     (let (pargs (map Lint-pe args))
       (cond
        ((and (not (eq? op 'read)) (andmap $Fixnum? pargs))
         ($Fixnum loc (call-prim op (map $Fixnum-value pargs))))
        ((andmap eq? args pargs)
         a)
        (else
         ($Prim loc op pargs)))))
    (($Program loc info exp)
     ($Program loc info (Lint-pe exp)))))

(def (Lint/pe s)
  (sexp<-Lint (Lint-pe (Lint<-sexp s))))
