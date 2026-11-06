#lang racket/base

;; ============================================================
;; opt: SSA operand access on GIMPLE statements (tree-ssa-operands.cc
;; iterators, read-only, over the persistent statements)
;; ============================================================
;;
;; Real operands are the SSA_NAME nodes inside the operand trees; the
;; virtual ones are the 'vuse / 'vdef statement flags (component/gimple/
;; ssa.rkt).  SSA_OP_USE = real uses, SSA_OP_DEF = the SSA lhs,
;; SSA_OP_ALL_DEFS = lhs + vdef.
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../ssa.rkt")

(provide ssa-name? ssa-name=? ssa-default-def? ssa-virtual?
         tree-ssa-uses stmt-use-trees stmt-real-uses stmt-get-lhs stmt-ssa-lhs stmt-all-defs
         stmt-map-uses subst-ssa-uses phi-ssa-args phi-arg-for-pred phi-set-args
         stmt-has-volatile-ops? stmt-has-side-effects? call-fndecl call-fndecl-builtin?
         gassign-single-p)

(define (ssa-name? t) (and (Tree? t) (eq? (tree-code t) 'ssa_name)))
(define (ssa-name=? a b) (and (ssa-name? a) (ssa-name? b) (= (ssa-name-version a) (ssa-name-version b))))
(define (ssa-default-def? t) (and (tree-attr t 'default-def #f) #t))
(define (ssa-virtual? t) (virtual-operand? (ssa-name-var t)))

;; every SSA_NAME inside T, in operand order (one entry per occurrence)
(define (tree-ssa-uses t)
  (cond
    [(not t) '()]
    [(ssa-name? t) (list t)]
    [(pvector-empty? (tree-ops t)) '()]
    [else (append* (for/list ([o (in-pvector (tree-ops t))]) (tree-ssa-uses o)))]))

;; replace every SSA_NAME inside T through F
(define (subst-ssa-uses t f)
  (cond
    [(not t) #f]
    [(ssa-name? t) (f t)]
    [(pvector-empty? (tree-ops t)) t]
    [else (tree-set-ops t (for/pvector ([o (in-pvector (tree-ops t))]) (subst-ssa-uses o f)))]))

;; gimple_get_lhs
(define (stmt-get-lhs g)
  (cond [(gassign? g) (gassign-lhs g)] [(gcall? g) (gcall-lhs g)] [else #f]))
(define (stmt-ssa-lhs g) (let ([l (stmt-get-lhs g)]) (and (ssa-name? l) l)))

;; operand trees in use position (a memory lhs counts: its address is used)
(define (stmt-use-trees g)
  (define (mem-lhs l) (if (or (not l) (ssa-name? l)) '() (list l)))
  (cond
    [(gassign? g) (append (mem-lhs (gassign-lhs g)) (list (gassign-rhs1 g) (gassign-rhs2 g) (gassign-rhs3 g)))]
    [(gcall? g) (append (mem-lhs (gcall-lhs g)) (cons (gcall-fn g) (pvector->list (gcall-args g))))]
    [(gcond? g) (list (gcond-lhs g) (gcond-rhs g))]
    [(gswitch? g) (list (gswitch-index g))]
    [(greturn? g) (list (greturn-retval g))]
    [else '()]))

(define (stmt-real-uses g) (append* (for/list ([t (in-list (stmt-use-trees g))]) (tree-ssa-uses t))))
(define (stmt-all-defs g)
  (append (let ([l (stmt-ssa-lhs g)]) (if l (list l) '()))
          (let ([v (stmt-vdef g)]) (if v (list v) '()))))

(define (gassign-single-p st g) (and (gassign? g) (eq? (gimple-rhs-class st (gassign-code g)) 'single)))

;; F : ssa-name -> tree, applied to every real use (the SSA lhs is kept).
;; A GIMPLE_SINGLE_RHS assignment keeps gimple_assign_rhs_code = TREE_CODE (rhs1).
(define (stmt-map-uses st g f)
  (define (u t) (subst-ssa-uses t f))
  (cond
    [(gassign? g)
     (define lhs (gassign-lhs g))
     (define rhs1 (u (gassign-rhs1 g)))
     (define code (if (gassign-single-p st g) (tree-code rhs1) (gassign-code g)))
     (gassign (gassign-loc g) (gassign-flags g) (if (ssa-name? lhs) lhs (u lhs)) code rhs1 (u (gassign-rhs2 g)) (u (gassign-rhs3 g)))]
    [(gcall? g)
     (define lhs (gcall-lhs g))
     (gcall (gcall-loc g) (gcall-flags g) (if (or (not lhs) (ssa-name? lhs)) lhs (u lhs)) (u (gcall-fn g))
            (for/pvector ([x (in-pvector (gcall-args g))]) (u x)) (gcall-fntype g) (gcall-internal? g) (gcall-ecf g))]
    [(gcond? g) (gcond (gcond-loc g) (gcond-flags g) (gcond-code g) (u (gcond-lhs g)) (u (gcond-rhs g))
                       (gcond-true-label g) (gcond-false-label g))]
    [(gswitch? g) (gswitch (gswitch-loc g) (gswitch-flags g) (u (gswitch-index g)) (gswitch-default-case g) (gswitch-cases g))]
    [(greturn? g) (greturn (greturn-loc g) (greturn-flags g) (u (greturn-retval g)))]
    [else g]))

;; PHI arguments that are SSA names: list of (cons pred name)
(define (phi-ssa-args ph) (for/list ([a (in-pvector (gphi-args ph))] #:when (ssa-name? (cdr a))) a))
(define (phi-arg-for-pred ph pred) (for/first ([a (in-pvector (gphi-args ph))] #:when (= (car a) pred)) (cdr a)))
(define (phi-set-args ph args) (gphi (gphi-loc ph) (gphi-flags ph) (gphi-var ph) (gphi-result ph) args))

;; gimple_has_volatile_ops: an operand tree with TREE_THIS_VOLATILE
(define (tree-volatile? t)
  (and t (or (and (tree-this-volatile t) #t)
             (for/or ([o (in-pvector (tree-ops t))]) (tree-volatile? o)))))
(define (stmt-has-volatile-ops? g)
  (cond
    [(gassign? g) (or (tree-volatile? (gassign-lhs g)) (tree-volatile? (gassign-rhs1 g))
                      (tree-volatile? (gassign-rhs2 g)) (tree-volatile? (gassign-rhs3 g)))]
    [(gcall? g) (or (tree-volatile? (gcall-lhs g)) (for/or ([a (in-pvector (gcall-args g))]) (tree-volatile? a)))]
    [else #f]))

;; gimple_has_side_effects (gimple.cc)
(define (stmt-has-side-effects? g)
  (cond
    [(stmt-has-volatile-ops? g) #t]
    [(gcall? g)
     (define f (gcall-ecf g))
     (or (zero? (bitwise-and f (bitwise-ior ECF-CONST ECF-PURE)))
         (not (zero? (bitwise-and f ECF-LOOPING))))]
    [else #f]))

;; gimple_call_fndecl: the FUNCTION_DECL behind `&f`, or #f
(define (call-fndecl g)
  (define fn (gcall-fn g))
  (and fn (eq? (tree-code fn) 'addr_expr)
       (let ([d (tree-operand fn 0)]) (and d (eq? (tree-code d) 'function_decl) d))))

;; fndecl_built_in_p: the dump carries no DECL_BUILT_IN_CLASS yet, so a
;; declaration is a built-in when it says so or by its reserved name
(define (call-fndecl-builtin? d)
  (and d (or (tree-attr d 'built-in #f)
             (let ([n (decl-name d)]) (and (string? n) (regexp-match? #rx"^__builtin_" n))))))
