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
;; A `seq` is an append-only queue (gimple_seq).
;; ============================================================

(require racket/list "tree.rkt")

(provide (struct-out gassign) (struct-out gcall) (struct-out gcond) (struct-out glabel)
         (struct-out ggoto) (struct-out greturn) (struct-out gswitch) (struct-out gbind)
         (struct-out gpredict) (struct-out gnop)
         gimple-build-assign gimple-build-call-from-tree gimple-build-cond gimple-build-label
         gimple-build-goto gimple-build-return gimple-build-switch gimple-build-bind gimple-build-predict
         gimple-cond-get-ops-from-tree extract-ops-from-tree
         gimple-seq-may-fallthru gimple-stmt-may-fallthru
         (struct-out seq) make-seq seq-add! seq-add-seq! seq->list seq-empty? seq-last seq-first seq-length)

(struct gassign ([lhs #:mutable] code [rhs1 #:mutable] [rhs2 #:mutable] [rhs3 #:mutable]) #:transparent)
(struct gcall ([lhs #:mutable] fn args fntype internal?) #:transparent)
(struct gcond (code lhs rhs [true-label #:mutable] [false-label #:mutable]) #:transparent)
(struct glabel (label) #:transparent)
(struct ggoto (dest) #:transparent)
(struct greturn (retval) #:transparent)
(struct gswitch (index default-case cases) #:transparent)   ; cases: list of CASE_LABEL_EXPR trees
(struct gbind ([vars #:mutable] [body #:mutable] block) #:transparent)
(struct gpredict (predictor outcome) #:transparent)
(struct gnop () #:transparent)

;; ---- sequences --------------------------------------------------------

(struct seq ([items #:mutable] [count #:mutable]))   ; items kept reversed
(define (make-seq) (seq '() 0))
(define (seq-add! s g) (set-seq-items! s (cons g (seq-items s))) (set-seq-count! s (add1 (seq-count s))))
(define (seq-add-seq! dst src)
  (when src
    (set-seq-items! dst (append (seq-items src) (seq-items dst)))
    (set-seq-count! dst (+ (seq-count dst) (seq-count src)))))
(define (seq->list s) (reverse (seq-items s)))
(define (seq-empty? s) (null? (seq-items s)))
(define (seq-last s) (if (null? (seq-items s)) #f (car (seq-items s))))
(define (seq-first s) (if (null? (seq-items s)) #f (last (seq-items s))))
(define (seq-length s) (seq-count s))

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
  (gcall #f (call-expr-fn call) (call-expr-args call) fntype (call-expr-internal? call)))

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
(define (gimple-build-return v) (greturn v))
(define (gimple-build-switch index default-case cases) (gswitch index default-case cases))
(define (gimple-build-bind vars body block) (gbind vars (or body (make-seq)) block))
(define (gimple-build-predict p o) (gpredict p o))

;; ---- fallthrough (gcc/gimple.cc gimple_stmt_may_fallthru) -------------

(define (gimple-stmt-may-fallthru g)
  (cond
    [(or (ggoto? g) (greturn? g)) #f]
    [(gswitch? g) #f]
    [(gcond? g) #t]   ; GIMPLE_COND: "This can only be a fallthru if the dest is also fallthru"-- GCC returns true
    [(gbind? g) (gimple-seq-may-fallthru (gbind-body g))]
    [(gcall? g) #t]   ; noreturn calls would be #f; not modelled
    [else #t]))

(define (gimple-seq-may-fallthru s)
  (define l (seq-last s))
  (or (not l) (gimple-stmt-may-fallthru l)))
