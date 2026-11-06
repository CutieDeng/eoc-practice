#lang racket/base

;; ============================================================
;; GIMPLE statements and sequences (working representation)
;; ============================================================
;;
;; Mirrors the gimple tuples the gimplifier emits for the C subset:
;;   gassign  lhs = rhs1 [op rhs2 [, rhs3]]   (code = rhs tree code)
;;   gcall    [lhs =] fn (args...)
;;   gcond    if (lhs code rhs) goto t; else goto f;
;;   glabel / ggoto / greturn / gswitch / gbind / gpredict / gnop
;;
;; Operands are T nodes (decls, SSA names, constants, MEM_REF trees).
;; A `seq` (gimple_seq) is a mutable cell around a pvector; gbind vars,
;; gcall args and gswitch cases are pvectors.
;; ============================================================

(require racket/list "../../../kernel/data/data.rkt" "tree.rkt")

(provide (struct-out gassign) (struct-out gcall) (struct-out gcond) (struct-out glabel)
         (struct-out ggoto) (struct-out greturn) (struct-out gswitch) (struct-out gbind)
         (struct-out gpredict) (struct-out gnop) (struct-out gtry) gimple-build-try
         gimple-build-assign gimple-build-call-from-tree gimple-build-cond gimple-build-label
         gimple-build-goto gimple-build-return gimple-build-switch gimple-build-bind gimple-build-predict
         gimple-cond-get-ops-from-tree extract-ops-from-tree gimple-call-noreturn-p
         gimple-seq-may-fallthru gimple-stmt-may-fallthru
         (struct-out seq) make-seq seq-add! seq-add-seq! seq->list in-seq seq-empty? seq-last seq-first seq-length seq-replace-last!)

(struct gassign ([lhs #:mutable] code [rhs1 #:mutable] [rhs2 #:mutable] [rhs3 #:mutable]) #:transparent)
(struct gcall ([lhs #:mutable] fn args fntype internal? flags) #:transparent)   ; flags: call_expr_flags (ECF_*)
(struct gcond (code lhs rhs [true-label #:mutable] [false-label #:mutable]) #:transparent)
(struct glabel (label) #:transparent)
(struct ggoto (dest) #:transparent)
(struct greturn (retval [loc #:auto #:mutable]) #:transparent)
(struct gswitch (index default-case cases) #:transparent)   ; cases: list of CASE_LABEL_EXPR trees
(struct gbind ([vars #:mutable] [body #:mutable] block) #:transparent)
(struct gpredict (predictor outcome) #:transparent)
(struct gnop () #:transparent)
(struct gtry ([eval #:mutable] [cleanup #:mutable] kind) #:transparent)   ; kind: 'finally | 'catch
(define (gimple-build-try eval cleanup kind) (gtry eval cleanup kind))

;; ---- sequences --------------------------------------------------------

(struct seq ([items #:mutable]))                 ; pvector of statements, in order
(define (make-seq) (seq (pvector-empty)))
(define (seq-add! s g) (set-seq-items! s (pvector-cons-right (seq-items s) g)))
(define (seq-add-seq! dst src)
  (when src (set-seq-items! dst (pvector-append (seq-items dst) (seq-items src)))))
(define (seq->list s) (pvector->list (seq-items s)))
(define (in-seq s) (in-pvector (seq-items s)))
(define (seq-empty? s) (pvector-empty? (seq-items s)))
(define (seq-last s) (if (seq-empty? s) #f (pvector-last (seq-items s))))
(define (seq-first s) (if (seq-empty? s) #f (pvector-first (seq-items s))))
(define (seq-length s) (pvector-length (seq-items s)))
(define (seq-replace-last! s g)
  (define items (seq-items s))
  (set-seq-items! s (pvector-set items (sub1 (pvector-length items)) g)))

;; ---- builders ---------------------------------------------------------

;; extract_ops_from_tree: split an RHS tree into (code op0 op1 op2)
(define (extract-ops-from-tree e)
  (define code (tree-code e))
  (case (gimple-rhs-class code)
    [(ternary) (values code (tree-operand e 0) (tree-operand e 1) (tree-operand e 2))]
    [(binary) (values code (tree-operand e 0) (tree-operand e 1) #f)]
    [(unary) (values code (tree-operand e 0) #f #f)]
    [(single) (values code e #f #f)]
    [else (error 'extract-ops-from-tree "invalid GIMPLE rhs: ~a" code)]))

(define (gimple-build-assign lhs rhs)
  (define-values (code a b c) (extract-ops-from-tree rhs))
  (gassign lhs code a b c))

;; gimple_build_call_from_tree: fn is the (already gimplified) function operand
(define (gimple-build-call-from-tree call fntype)
  (gcall #f (call-expr-fn call) (call-expr-args call) fntype (call-expr-internal? call) (attr call 'flags 0)))

;; gimple_cond_get_ops_from_tree (gcc/gimple.cc)
(define (gimple-cond-get-ops-from-tree c)
  (define code (tree-code c))
  (cond
    [(memq code '(lt_expr le_expr gt_expr ge_expr eq_expr ne_expr unordered_expr ordered_expr
                  unlt_expr unle_expr ungt_expr unge_expr uneq_expr ltgt_expr))
     (values code (tree-operand c 0) (tree-operand c 1))]
    [(eq? code 'truth_not_expr)
     (values 'eq_expr (tree-operand c 0) (build-zero-cst (tree-type (tree-operand c 0))))]
    [else
     ;; (c != 0)
     (values 'ne_expr c (build-zero-cst (tree-type c)))]))

(define (gimple-build-cond code lhs rhs tl fl) (gcond code lhs rhs tl fl))
(define (gimple-build-label l) (glabel l))
(define (gimple-build-goto l) (ggoto l))
(define (gimple-build-return v [loc #f]) (define g (greturn v)) (set-greturn-loc! g loc) g)
(define ECF-NORETURN 8)
(define (gimple-call-noreturn-p g) (not (zero? (bitwise-and (gcall-flags g) ECF-NORETURN))))
(define (gimple-build-switch index default-case cases) (gswitch index default-case cases))
(define (gimple-build-bind vars body block) (gbind vars (or body (make-seq)) block))
(define (gimple-build-predict p o) (gpredict p o))

;; ---- fallthrough (gcc/gimple.cc gimple_stmt_may_fallthru) -------------

(define (gimple-stmt-may-fallthru g)
  (cond
    [(or (ggoto? g) (greturn? g)) #f]
    [(gswitch? g) #f]
    [(gcond? g) #f]   ; GIMPLE_COND always ends in gotos
    [(gbind? g) (gimple-seq-may-fallthru (gbind-body g))]
    [(gtry? g) (and (gimple-seq-may-fallthru (gtry-eval g)) (gimple-seq-may-fallthru (gtry-cleanup g)))]
    [(gcall? g) (not (gimple-call-noreturn-p g))]
    [else #t]))

(define (gimple-seq-may-fallthru s)
  (define l (seq-last s))
  (or (not l) (gimple-stmt-may-fallthru l)))
