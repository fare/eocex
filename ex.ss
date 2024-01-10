(import
  :std/misc/list
  :std/srfi/141
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
