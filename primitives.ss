(export #t)

(import
  ;; :std/debug/DBG
  :std/misc/func
  :std/misc/list
  :std/srfi/1
  (only-in :std/sugar defrule hash))

;;; Primitives
(def Primitives (make-parameter (hash)))
(defrule (defprim name args ...)
  (hash-put! (Primitives) 'name (list args ...)))

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
  (match (hash-get (Primitives) op)
    ([arity fun]
     (apply fun (match-args op arity args)))
    (#f (error "Unknown primitive" op))))

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
