#lang racket/base

;; ============================================================
;; gimple: statement builders and predicates (pure)
;; ============================================================
;;
;; Statements are the prefab values of kernel/ir/gimple/types.rkt; a
;; sequence (gimple_seq) is a pvector.  Builders that need the tree
;; code table take the state first and return the statement only.
;; ============================================================

(require "../../kernel/data/data.rkt"
         "../../kernel/ir/gimple/types.rkt"
         "state.rkt"
         "tree.rkt")

(provide gimple-build-assign gimple-build-call-from-tree gimple-build-cond gimple-build-label gimple-build-goto
         gimple-build-return gimple-build-switch gimple-build-bind gimple-build-predict gimple-build-try gimple-build-nop
         extract-ops-from-tree gimple-cond-get-ops-from-tree
         gimple-call-noreturn-p gimple-stmt-may-fallthru gimple-seq-may-fallthru
         annotate-one-with-location annotate-all-with-location-after stmt-set-block
         stmt-do-not-emit-location? stmt-set-do-not-emit-location
         ECF-CONST ECF-PURE ECF-LOOPING ECF-NORETURN ECF-RETURNS-TWICE
         seq-last seq-first seq-replace-last)

(define ECF-CONST 1) (define ECF-PURE 2) (define ECF-LOOPING 4) (define ECF-NORETURN 8) (define ECF-RETURNS-TWICE 128)

(define no-flags (hasheq))

;; ---- sequences ----------------------------------------------------------

(define (seq-last s) (if (pvector-empty? s) #f (pvector-last s)))
(define (seq-first s) (if (pvector-empty? s) #f (pvector-first s)))
(define (seq-replace-last s g) (pvector-set s (sub1 (pvector-length s)) g))

;; ---- builders -----------------------------------------------------------

;; extract_ops_from_tree: split an RHS tree into (code op0 op1 op2)
(define (extract-ops-from-tree st e)
  (define code (tree-code e))
  (case (gimple-rhs-class st code)
    [(ternary) (values code (tree-operand e 0) (tree-operand e 1) (tree-operand e 2))]
    [(binary) (values code (tree-operand e 0) (tree-operand e 1) #f)]
    [(unary) (values code (tree-operand e 0) #f #f)]
    [(single) (values code e #f #f)]
    [else (error 'extract-ops-from-tree "invalid GIMPLE rhs: ~a" code)]))

(define (gimple-build-assign st lhs rhs [loc #f])
  (define-values (code a b c) (extract-ops-from-tree st rhs))
  (gassign loc no-flags lhs code a b c))

;; gimple_build_call_from_tree: fn is the (already gimplified) function operand;
;; the statement takes the CALL_EXPR's location
(define (gimple-build-call-from-tree call fntype [lhs #f])
  (gcall (make-loc (tree-loc call)) no-flags lhs (call-expr-fn call) (call-expr-args call) fntype
         (call-expr-internal? call) (tree-attr call 'flags 0)))

;; gimple_cond_get_ops_from_tree (gcc/gimple.cc)
(define (gimple-cond-get-ops-from-tree st c)
  (define code (tree-code c))
  (cond
    [(memq code '(lt_expr le_expr gt_expr ge_expr eq_expr ne_expr unordered_expr ordered_expr
                  unlt_expr unle_expr ungt_expr unge_expr uneq_expr ltgt_expr))
     (values code (tree-operand c 0) (tree-operand c 1))]
    [(eq? code 'truth_not_expr)
     (values 'eq_expr (tree-operand c 0) (build-zero-cst st (tree-type (tree-operand c 0))))]
    [else (values 'ne_expr c (build-zero-cst st (tree-type c)))]))

(define (gimple-build-cond code lhs rhs tl fl [loc #f]) (gcond loc no-flags code lhs rhs tl fl))
(define (gimple-build-label l [loc #f]) (glabel loc no-flags l))
(define (gimple-build-goto l [loc #f]) (ggoto loc no-flags l))
(define (gimple-build-return v [loc #f]) (greturn (if (or (not loc) (gloc? loc)) loc (make-loc loc)) no-flags v))
(define (gimple-build-switch index default-case cases [loc #f]) (gswitch loc no-flags index default-case cases))
(define (gimple-build-bind vars body block [loc #f]) (gbind loc no-flags vars (or body (pvector-empty)) block))
(define (gimple-build-predict p o) (gpredict #f no-flags p o))
(define (gimple-build-try eval cleanup kind) (gtry #f no-flags eval cleanup kind))
(define (gimple-build-nop) (gnop #f no-flags))

(define (gimple-call-noreturn-p g) (not (zero? (bitwise-and (gcall-ecf g) ECF-NORETURN))))

;; ---- locations ------------------------------------------------------------

(define (stmt-do-not-emit-location? g) (stmt-flag g 'do-not-emit #f))
(define (stmt-set-do-not-emit-location g) (stmt-set-flag g 'do-not-emit #t))
;; gimple_set_block
(define (stmt-set-block g block) (stmt-set-loc g (set-block-loc (stmt-loc g) block)))
;; annotate_one_with_location (should_carry_location_p is false for labels)
(define (annotate-one-with-location g l)
  (if (and (not (stmt-has-location? g)) (not (stmt-do-not-emit-location? g)) (not (glabel? g)))
      (stmt-set-loc g l)
      g))
;; annotate_all_with_location_after: statements of SEQ from index FROM on
(define (annotate-all-with-location-after seq from l)
  (for/fold ([s seq]) ([i (in-range (min from (pvector-length seq)) (pvector-length seq))])
    (pvector-set s i (annotate-one-with-location (pvector-ref s i) l))))

;; ---- fallthrough (gcc/gimple.cc gimple_stmt_may_fallthru) -----------------

(define (gimple-stmt-may-fallthru g)
  (cond
    [(or (ggoto? g) (greturn? g) (gswitch? g) (gcond? g)) #f]
    [(gbind? g) (gimple-seq-may-fallthru (gbind-body g))]
    [(gtry? g) (and (gimple-seq-may-fallthru (gtry-eval g)) (gimple-seq-may-fallthru (gtry-cleanup g)))]
    [(gcall? g) (not (gimple-call-noreturn-p g))]
    [else #t]))

(define (gimple-seq-may-fallthru s)
  (define l (seq-last s))
  (or (not l) (gimple-stmt-may-fallthru l)))
