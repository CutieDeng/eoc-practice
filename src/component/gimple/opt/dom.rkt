#lang racket/base

;; ============================================================
;; opt: pass_dominator (gcc/tree-ssa-dom.cc) with the forward jump
;; threader of tree-ssa-threadedge.cc and the path registry checks of
;; tree-ssa-threadupdate.cc, GCC 15, with the GCC 12-14 differences
;; listed below
;; ============================================================
;;
;; (dom-pass st fndecl cfg) -> (values cfg st)
;;
;; loop_optimizer_init (preheaders, simple latches; "Created preheader
;; block for loop N"), mark_dfs_back_edges, record_edge_info for every
;; block, the ranger, then the dominator walk:
;;   before: record_equivalences_from_incoming_edge (edge equivalences
;;     and condition equivalences of the single non-loop predecessor),
;;     record_equivalences_from_phis (a PHI whose executable arguments
;;     agree is a copy), redundant PHIs, optimize_stmt for every
;;     statement (cprop_into_stmt from SSA_NAME_VALUE or a ranger
;;     singleton, fold_stmt after a change, GIMPLE_COND through the
;;     ranger, eliminate_redundant_computations, the redundant store
;;     check, record_equivalences_from_stmt), record_edge_info,
;;     cprop_into_successor_phis;
;;   after: thread_outgoing_edges, unwind the scoped tables.
;; Then export_global_ranges (dumped by GCC >= 15), the registered
;; jump threads, TODO_cleanup_cfg.
;;
;; The threader follows thread_across_edge / thread_through_normal_block
;; / thread_around_empty_blocks with the statement counting that
;; prints "threading bb N kills K stmts" / "threading bb N up to M
;; stmts"; conditions are simplified by folding through SSA_NAME_VALUE
;; and the available-expression table, then (GCC >= 13) by
;; hybrid_jt_simplifier's path range query over the jt_state blocks
;; (ranger.rkt rg-path-range-of-cond; switches are not resolved that
;; way).  cancel_invalid_paths stops after the loop exit check once
;; PROP_loop_opts_done is set (dom3).  When a condition was folded
;; (cfg_altered), the edges leaving a block with a non-executable
;; successor edge go to remove_jump_threads_including.  The surviving
;; threads go to threadupdate.rkt.
;;
;; register_equivs_stmt propagates SSA_NAME_VALUE into all uses, the
;; VUSE included, and restoring them relinks the statement's uses
;; (mfn-relink!).  __builtin_constant_p stops a thread;
;; __builtin_object_size records no equivalence.
;;
;; Version differences: GCC <= 14 simplify_control_stmt_condition_1
;; looks through (A & B) CMP 0, (A | B) CMP 0 and (A CMP B) CMP 0
;; (recursion limit 4); param_max_jump_thread_paths (64) is GCC 15 only.
;;
;; Not ported: test_for_singularity, the alias walk of lookup_avail_expr
;; (a load with a different VUSE is a miss), EH and noreturn fixups;
;; GCC 12's dom_jt_simplifier range queries (vr_values:
;; vrp_evaluate_conditional, find_case_label_range, the singleton of
;; extract_range_from_stmt, the ranges dom_jt_state records), so a GCC 12
;; thread that needs a range is missed; fold_binary on the comparison
;; is reduced to two integer constants or the same integral name
;; (pointers, ADDR_EXPRs and reals are not folded); computed gotos;
;; EDGE_COMPLEX / abnormal edges (none are built for C here); unique
;; internal calls and volatile asm (not in the IL).
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../../../driver/dominance/dom.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         (only-in "../gimplify.rkt" tree-swap-operands-p is-gimple-min-invariant)
         "../cfg.rkt"
         (only-in "../ssa.rkt" stmt-vuse stmt-vdef virtual-operand?)
         (only-in "../print.rkt" print-cond-rhs)
         "ssa-ops.rkt"
         "mfn.rkt"
         "loops.rkt"
         (only-in "simplify.rkt" fold-stmt follow-ssa-edges? gimple-fold-stmt-to-constant may-propagate-copy
                  commutative-code? swap-tree-comparison invert-tree-comparison)
         (only-in "fold.rkt" fold-const-compare fold-const-binop type-precision* type-unsigned?*)
         (only-in "ipa.rkt" estimate-num-insns)
         "irange.rkt"
         "range-info.rkt"
         "ranger.rkt"
         "threadupdate.rkt"
         (only-in "update-ssa.rkt" update-ssa))

(provide dom-pass dom-pass*)

(define PARAM-MAX-JUMP-THREAD-PATHS 64)
;; tcc_comparison
(define COMPARISON-CODES '(lt_expr le_expr gt_expr ge_expr eq_expr ne_expr unordered_expr ordered_expr
                           unlt_expr unle_expr ungt_expr unge_expr uneq_expr ltgt_expr))
(define PARAM-MAX-JUMP-THREAD-DUPLICATION-STMTS 15)

(define (icst? t) (and (Tree? t) (eq? (tree-code t) 'integer_cst)))
(define (ver t) (ssa-name-version t))

;; edge_info: simple equivalences (lhs . rhs) and condition equivalences (key . value)
(struct ei ([simple #:mutable] [conds #:mutable]))

(define (dom-pass st0 fndecl cfg0) ((dom-pass* #t) st0 fndecl cfg0))
;; MAY-PEEL-LOOP-HEADERS?: pass_dominator's parameter (dom2 #t, dom3 #f)
(define ((dom-pass* may-peel-loop-headers?) st0 fndecl cfg0)
  (define major (gcc-major-version st0))
  (define st (gs-set-fndecl st0 fndecl))
  (define loop-opts-done (decl-flag st0 fndecl 'loop-opts-done #f))   ; PROP_loop_opts_done
  ;; loop_optimizer_init (LOOPS_HAVE_PREHEADERS | LOOPS_HAVE_SIMPLE_LATCHES)
  (define cfg (normalize-loops st (cfg-clear-messages cfg0)))
  (define messages '())
  (define (msg! fmt . args) (set! messages (cons (apply format fmt args) messages)))
  (define dom (compute-dom (pvector-cons-left (pvector-cons-left (cfg-bbs cfg) EXIT) ENTRY) ENTRY
                           (lambda (i) (for/pvector ([e (in-pvector (cfg-succs cfg i))]) (GEdge-dest e)))))
  (define-values (loops _lb) (find-loops cfg dom))
  (define back (dfs-back-edges cfg))
  (define (dfs-back? s d) (hash-ref back (cons s d) #f))
  (define m (gcfg->mfn cfg))
  (define bool (boolean-type st))

  ;; ---- CFG helpers -----------------------------------------------
  (define (succs bb) (for/list ([e (in-pvector (cfg-succs cfg bb))]) (GEdge-dest e)))
  (define (preds bb) (pvector->list (cfg-preds cfg bb)))
  (define (dominated-by? a b)
    (define ia (dom-index dom a)) (define ib (dom-index dom b))
    (and ia ib (dom-dominates? dom ib ia)))
  (define (idom bb)
    (define i (dom-index dom bb)) (define j (and i (dom-idom dom i)))
    (and j (not (= j i)) (dom-node dom j)))
  (define (dom-kids bb)
    (define i (dom-index dom bb))
    (if i (for/list ([k (in-list (sort (pvector->list (dom-children dom i)) <))]) (dom-node dom k)) '()))
  (define non-exec (make-hash))                 ; (src . dest) -> #t
  (define cfg-altered #f)                       ; a condition was folded
  (define (executable? s d) (not (hash-ref non-exec (cons s d) #f)))
  (define (last-id bb) (mfn-last m bb))
  (define (last-stmt bb) (let ([id (last-id bb)]) (and id (mfn-stmt m id))))
  (define (true-false-edges bb)
    (define-values (te fe) (extract-true-false-edges cfg bb))
    (values (GEdge-dest te) (GEdge-dest fe)))

  ;; loop structure for cancel_invalid_paths
  (define (loop-father bb)
    (or (loop-of loops bb) 'root))
  (define (loop-header l) (and (Loop? l) (Loop-header l)))
  (define (loop-latch l) (and (Loop? l) (= 1 (length (Loop-latches l))) (car (Loop-latches l))))
  (define (loop-parent l) (cond [(eq? l 'root) #f]
                                [(Loop-parent l) (findf (lambda (o) (= (Loop-num o) (Loop-parent l))) loops)]
                                [else 'root]))
  ;; flow_loop_nested_p (outer, loop): OUTER strictly encloses LOOP
  (define (nested? outer l)
    (let loop ([p (loop-parent l)]) (cond [(not p) #f] [(eq? p outer) #t] [else (loop (loop-parent p))])))

  ;; ---- SSA_NAME_VALUE and const_and_copies -------------------------
  (define values-tab (make-hasheqv))
  (define (ssa-value t) (and (ssa-name? t) (hash-ref values-tab (ver t) #f)))
  (define (set-ssa-value! t v) (if v (hash-set! values-tab (ver t) v) (hash-remove! values-tab (ver t))))
  (define (dom-valueize t) (or (ssa-value t) t))
  (define copies-stack '())
  (define (copies-push-marker!) (set! copies-stack (cons 'marker copies-stack)))
  (define (copies-pop-to-marker!)
    (let loop ()
      (unless (null? copies-stack)
        (define top (car copies-stack))
        (set! copies-stack (cdr copies-stack))
        (unless (eq? top 'marker)
          (set-ssa-value! (car top) (cdr top))
          (loop)))))
  (define (record-const-or-copy-raw! x y prev)
    (set-ssa-value! x y)
    (set! copies-stack (cons (cons x prev) copies-stack)))
  (define (record-const-or-copy! x y [prev-x (ssa-value x)])
    (define y* (if (ssa-name? y) (or (ssa-value y) y) y))
    (record-const-or-copy-raw! x y* prev-x))

  ;; ---- the available expression table ------------------------------
  (define (type-key ty)
    (and ty (list (type-precision* st ty) (type-unsigned?* st ty) (if (pointer-type-p st ty) 'ptr 'int))))
  (define (tkey t)
    (cond
      [(not t) #f]
      [(ssa-name? t) (list 'ssa (ver t))]
      [(icst? t) (list 'cst (integer-cst-value t) (type-key (tree-type t)))]
      [(tree-attr t 'uid #f) (list 'decl (tree-attr t 'uid))]
      [else (cons (tree-code t) (for/list ([o (in-pvector (tree-ops t))]) (tkey o)))]))
  ;; hashable expression of a statement: (kind type . parts) or #f
  (define (stmt-expr g)
    (cond
      [(gassign? g)
       (define code (gassign-code g))
       (case (gimple-rhs-class st code)
         [(single) (list 'single (type-key (tree-type (gassign-rhs1 g))) (tkey (gassign-rhs1 g)))]
         [(unary) (list 'unary (type-key (tree-type (gassign-lhs g))) (if (memq code '(nop_expr convert_expr)) 'nop_expr code)
                        (tkey (gassign-rhs1 g)))]
         [(binary) (list 'binary (type-key (tree-type (gassign-lhs g))) code (tkey (gassign-rhs1 g)) (tkey (gassign-rhs2 g)))]
         [(ternary) (list 'ternary (type-key (tree-type (gassign-lhs g))) code (tkey (gassign-rhs1 g)) (tkey (gassign-rhs2 g)) (tkey (gassign-rhs3 g)))]
         [else #f])]
      [(gcond? g) (list 'binary (type-key bool) (gcond-code g) (tkey (gcond-lhs g)) (tkey (gcond-rhs g)))]
      [(gcall? g)
       (if (not (zero? (bitwise-and (call-ecf st g) (bitwise-ior ECF-CONST ECF-PURE))))
           (list* 'call (type-key (tree-type (gcall-lhs g))) (tkey (gcall-fn g)) (for/list ([a (in-pvector (gcall-args g))]) (tkey a)))
           (list 'impure (gensym)))]
      [(gswitch? g) (list 'single (type-key (tree-type (gswitch-index g))) (tkey (gswitch-index g)))]
      [(gphi? g) (list* 'phi (type-key (tree-type (gphi-result g))) (for/list ([a (in-pvector (gphi-args g))]) (tkey (cdr a))))]
      [else #f]))
  (define (cond-expr code a b) (list 'binary (type-key bool) code (tkey a) (tkey b)))
  ;; the commutative alternative key
  (define (swapped-key k)
    (and k (eq? (car k) 'binary) (commutative-code? (caddr k))
         (list 'binary (cadr k) (caddr k) (list-ref k 4) (list-ref k 3))))
  (define avail (make-hash))                    ; key -> (vector lhs vop)
  (define avail-stack '())
  (define (avail-push-marker!) (set! avail-stack (cons 'marker avail-stack)))
  (define (avail-pop-to-marker!)
    (let loop ()
      (unless (null? avail-stack)
        (define top (car avail-stack))
        (set! avail-stack (cdr avail-stack))
        (unless (eq? top 'marker)
          (if (cdr top) (hash-set! avail (car top) (cdr top)) (hash-remove! avail (car top)))
          (loop)))))
  (define (avail-find k)
    (cond [(not k) #f]
          [(hash-ref avail k #f) => (lambda (e) (cons k e))]
          [(swapped-key k) => (lambda (k2) (let ([e (hash-ref avail k2 #f)]) (and e (cons k2 e))))]
          [else #f]))
  (define (avail-insert! k e)
    (set! avail-stack (cons (cons k (hash-ref avail k #f)) avail-stack))
    (hash-set! avail k e))
  ;; record_cond: inserted only when absent
  (define (record-cond! k value)
    (unless (avail-find k) (avail-insert! k (vector value #f))))
  ;; lookup_avail_expr (stmt, insert): the valueized cached lhs or #f
  (define (lookup-avail-expr g insert)
    (define k (stmt-expr g))
    (define lhs (cond [(gphi? g) (gphi-result g)] [(gassign? g) (gassign-lhs g)] [(gcall? g) (gcall-lhs g)] [else #f]))
    (cond
      [(not k) #f]
      [(and (eq? (car k) 'single)
            (let ([rhs (cond [(gassign? g) (gassign-rhs1 g)] [(gswitch? g) (gswitch-index g)] [else #f])])
              (and rhs (or (ssa-name? rhs) (is-gimple-min-invariant st rhs)))))
       #f]
      [else
       (define found (avail-find k))
       (define vuse (if (gphi? g) #f (stmt-vuse g)))
       (cond
         [(not found)
          (when insert (avail-insert! k (vector lhs vuse)))
          #f]
         [(not (equal? (and vuse (tkey vuse)) (let ([v (vector-ref (cdr found) 1)]) (and v (tkey v)))))
          (when insert (avail-insert! (car found) (vector lhs vuse)))
          #f]
         [else
          (define l (vector-ref (cdr found) 0))
          (if (ssa-name? l) (or (ssa-value l) l) l)])]))

  ;; ---- edge information ----------------------------------------------
  (define edge-infos (make-hash))               ; (src . dest) -> ei
  (define (new-edge-info! s d) (let ([x (ei '() '())]) (hash-set! edge-infos (cons s d) x) x))
  (define (edge-info s d) (hash-ref edge-infos (cons s d) #f))
  (define (push-simple! x lhs rhs) (set-ei-simple! x (append (ei-simple x) (list (cons lhs rhs)))))
  (define (push-cond! x k v) (set-ei-conds! x (append (ei-conds x) (list (cons k v)))))
  (define (bool-cst v) (build-int-cst st bool v))
  ;; record_conditions: (code a b) holds, the inverse does not
  (define (record-conditions! x code a b)
    (define inv (invert-tree-comparison code))
    (define (new! c v) (push-cond! x (cond-expr c a b) (bool-cst v)))
    (case code
      [(lt_expr gt_expr) (new! (if (eq? code 'lt_expr) 'le_expr 'ge_expr) 1) (new! 'ne_expr 1) (new! 'eq_expr 0)]
      [(eq_expr) (new! 'le_expr 1) (new! 'ge_expr 1)]
      [else (void)])
    (push-cond! x (cond-expr code a b) (bool-cst 1))
    (when inv (push-cond! x (cond-expr inv a b) (bool-cst 0))))
  (define (has-boolean-range? op)
    (define ty (tree-type op))
    (and (integral-type-p st ty)
         (or (and (type-unsigned?* st ty) (= 1 (type-precision* st ty)))
             (and (> (type-precision* st ty) 1)
                  (let ([ri (range-info st fndecl op)]) (and ri (not (ir-undefined? ri)) (= 1 (ir-nonzero-bits ri))))))))
  (define (record-simple-equiv! x lhs rhs)
    (if (icst? rhs) (derive-equivalences! x lhs rhs 4) (push-simple! x lhs rhs)))
  (define (derive-equivalences! x name value limit)
    (when (and (ssa-name? name) (icst? value))
      (push-simple! x name value)
      (unless (= limit 0)
        (define g (mfn-def-stmt m name))
        (when (and g (gassign? g))
          (define code (gassign-code g))
          (define a (gassign-rhs1 g)) (define b (gassign-rhs2 g))
          (define (cst-of ty v) (build-int-cst st ty v))
          (define (rec n v) (derive-equivalences! x n v (sub1 limit)))
          (case code
            [(bit_ior_expr) (when (integer-zerop value) (rec a (cst-of (tree-type a) 0)) (rec b (cst-of (tree-type b) 0)))]
            [(bit_and_expr)
             (unless (integer-zerop value)
               (when (and (ssa-name? a) (has-boolean-range? a)) (rec a (cst-of (tree-type a) 1)))
               (when (and (ssa-name? b) (has-boolean-range? b)) (rec b (cst-of (tree-type b) 1))))]
            [(nop_expr convert_expr)
             (define rt (tree-type a))
             (when (and (integral-type-p st rt) (>= (type-precision* st (tree-type name)) (type-precision* st rt))
                        (fits? (integer-cst-value value) rt))
               (rec a (cst-of rt (integer-cst-value value))))]
            [(plus_expr pointer_plus_expr)
             (cond
               [(and (icst? a) (ssa-name? b)) (rec b (fold-const-binop st 'minus_expr (tree-type a) value a))]
               [(and (icst? b) (ssa-name? a)) (rec a (fold-const-binop st 'minus_expr (tree-type a) value b))])]
            [(minus_expr)
             (cond
               [(and (icst? a) (ssa-name? b)) (rec b (fold-const-binop st 'minus_expr (tree-type a) a value))]
               [(and (icst? b) (ssa-name? a)) (rec a (fold-const-binop st 'plus_expr (tree-type a) value b))]
               [(integer-zerop value) (record-conditions! x 'eq_expr a b)])]
            [(eq_expr ne_expr)
             (cond
               [(or (and (eq? code 'eq_expr) (integer-onep value)) (and (eq? code 'ne_expr) (integer-zerop value)))
                (cond [(icst? a) (rec b a)] [(icst? b) (rec a b)])]
               [else (if (integer-zerop value)
                         (record-conditions! x (invert-tree-comparison code) a b)
                         (record-conditions! x code a b))])]
            [(bit_not_expr negate_expr)
             (define res
               (if (and (eq? code 'bit_not_expr) (ssa-name? a) (has-boolean-range? a))
                   (cst-of (tree-type a) (if (even? (integer-cst-value value)) 1 0))
                   (fold-const-unop* code (tree-type a) value)))
             (when res (rec a res))]
            [(lt_expr le_expr gt_expr ge_expr)
             (if (integer-zerop value)
                 (record-conditions! x (invert-tree-comparison code) a b)
                 (record-conditions! x code a b))]
            [else (void)])))))
  (define (fits? v ty)
    (define t (tinfo st ty))
    (and (>= v (ti-min t)) (<= v (ti-max t))))
  (define (fold-const-unop* code ty v)
    (define t (tinfo st ty))
    (define x (integer-cst-value v))
    (build-int-cst st ty (from-pattern (if (eq? code 'negate_expr) (- x) (bitwise-not x)) t)))

  ;; record_edge_info
  (define (record-edge-info! bb)
    (for ([d (in-list (succs bb))]) (hash-remove! edge-infos (cons bb d)))
    (define g (last-stmt bb))
    (cond
      [(and g (gswitch? g) (ssa-name? (gswitch-index g)))
       (define idx (gswitch-index g))
       (define info (make-hasheqv))
       (for ([cs (in-pvector (pvector-cons-left (gswitch-cases g) (gswitch-default-case g)))])
         (define target (label-to-block cfg (tree-operand cs 2)))
         (if (or (tree-operand cs 1) (not (tree-operand cs 0)) (hash-ref info target #f))
             (hash-set! info target 'error)
             (hash-set! info target cs)))
       (for ([d (in-list (succs bb))])
         (define cs (hash-ref info d #f))
         (when (and cs (not (eq? cs 'error)))
           (define x (new-edge-info! bb d))
           (record-simple-equiv! x idx (build-int-cst st (tree-type idx) (integer-cst-value (tree-operand cs 0))))))]
      [(and g (gcond? g))
       (define-values (te fe) (true-false-edges bb))
       (define op0 (gcond-lhs g)) (define op1 (gcond-rhs g)) (define code (gcond-code g))
       (define (invariant? t) (is-gimple-min-invariant st t))
       (cond
         [(and (memq code '(eq_expr ne_expr)) (ssa-name? op0) (has-boolean-range? op0) (invariant? op1)
               (or (integer-zerop op1) (integer-onep op1)))
          (define tv (build-int-cst st (tree-type op0) 1)) (define fv (build-int-cst st (tree-type op0) 0))
          (define eq-true (if (eq? code 'eq_expr) (if (integer-zerop op1) fv tv) (if (integer-zerop op1) tv fv)))
          (define eq-false (if (eq? code 'eq_expr) (if (integer-zerop op1) tv fv) (if (integer-zerop op1) fv tv)))
          (record-simple-equiv! (new-edge-info! bb te) op0 eq-true)
          (record-simple-equiv! (new-edge-info! bb fe) op0 eq-false)]
         [(or (and (invariant? op0) (ssa-name? op1))
              (and (ssa-name? op0) (or (ssa-name? op1) (invariant? op1))))
          (define inv (invert-tree-comparison code))
          (define-values (nm val) (if (ssa-name? op0) (values op0 op1) (values op1 op0)))
          (define xt (new-edge-info! bb te))
          (record-conditions! xt code op0 op1)
          (when (eq? code 'eq_expr) (record-simple-equiv! xt nm val))
          (define xf (new-edge-info! bb fe))
          (when inv (record-conditions! xf inv op0 op1))
          (when (eq? inv 'eq_expr) (record-simple-equiv! xf nm val))])
       ;; single block loop with a loop-invariant exit test
       (when (single-block-loop? bb)
         (define (outside? t) (or (not (ssa-name? t)) (not (eqv? (and (mfn-def m t) (mfn-bb m (mfn-def m t))) bb))))
         (when (and (outside? op0) (outside? op1))
           (define ps (preds bb))
           (define alt (if (dfs-back? (first ps) bb) 1 0))
           (for ([pid (in-list (mfn-phis m bb))])
             (define ph (mfn-stmt m pid))
             (define src (cdr (pvector-ref (gphi-args ph) alt)))
             (define dst (gphi-result ph))
             (unless (equal? (tkey dst) (tkey (cdr (pvector-ref (gphi-args ph) (- 1 alt)))))
               (define s0 (first (succs bb)))
               (define target (if (not (= s0 (list-ref ps (- 1 alt)))) s0 (second (succs bb))))
               (define x (or (edge-info bb target) (new-edge-info! bb fe)))
               (record-simple-equiv! x dst src)))))]
      [else (void)]))
  (define (single-block-loop? bb)
    (define ps (preds bb))
    (and (= 2 (length ps))
         (let ([backs (filter (lambda (p) (dfs-back? p bb)) ps)])
           (and (= 1 (length backs))
                (let ([pred (car backs)])
                  (and (= 1 (length (preds pred))) (= 1 (length (succs pred)))
                       (= bb (car (preds pred))) (= bb (car (succs pred)))
                       (null? (mfn-phis m pred))
                       (for/and ([id (in-list (mfn-stmts m pred))]) (glabel? (mfn-stmt m id)))))))))

  ;; record_equality
  (define (record-equality! x y)
    (define-values (x1 y1) (if (tree-swap-operands-p st x y) (values y x) (values x y)))
    (define-values (x2 y2)
      (if (and (ssa-name? x1) (ssa-name? y1) (mfn-single-use? m y1) (not (mfn-single-use? m x1))) (values y1 x1) (values x1 y1)))
    (define prev-x (and (ssa-name? x2) (ssa-value x2)))
    (define prev-y (and (ssa-name? y2) (ssa-value y2)))
    (define-values (x3 y3 px)
      (cond
        [(is-gimple-min-invariant st y2) (values x2 y2 prev-x)]
        [(is-gimple-min-invariant st x2) (values y2 x2 prev-y)]
        [(and prev-x (is-gimple-min-invariant st prev-x)) (values y2 prev-x prev-y)]
        [prev-y (values x2 prev-y prev-x)]
        [else (values x2 y2 prev-x)]))
    (when (ssa-name? x3) (record-const-or-copy! x3 y3 px)))

  (define (stmt-cost name)
    (define g (mfn-def-stmt m name))
    (if g (estimate-num-insns st g) 0))

  ;; record_temporary_equivalences
  (define blocks-on-stack (make-hasheqv))
  (define (record-temporary-equivalences! s d)
    (define x (edge-info s d))
    (when x
      (for ([c (in-list (ei-conds x))]) (record-cond! (car c) (cdr c)))
      (for ([p (in-list (ei-simple x))] #:when (ssa-name? (car p)))
        (define lhs (car p)) (define rhs (cdr p))
        (cond
          [(and (ssa-name? lhs) (ssa-name? rhs))
           (define rc (stmt-cost rhs)) (define lc (stmt-cost lhs))
           (cond [(> rc lc) (record-equality! rhs lhs)] [(< rc lc) (record-equality! lhs rhs)])]
          [else (record-equality! lhs rhs)])
        (back-propagate-equivalences! lhs d))))
  (define (back-propagate-equivalences! lhs dest)
    (for ([uid (in-list (mfn-imm-uses m lhs))])
      (define ub (mfn-bb m uid))
      (define u (mfn-stmt m uid))
      (define lhs2 (cond [(gassign? u) (gassign-lhs u)] [(gcall? u) (gcall-lhs u)] [(gphi? u) (gphi-result u)] [else #f]))
      (when (and (not (= dest ub)) lhs2 (ssa-name? lhs2) (dominated-by? dest ub))
        (define res (parameterize ([follow-ssa-edges? #f]) (gimple-fold-stmt-to-constant st m u dom-valueize)))
        (when (and res (or (ssa-name? res) (is-gimple-min-invariant st res)))
          (record-equality! lhs2 res)))))

  ;; ---- the ranger ----------------------------------------------------
  (define R (make-ranger st fndecl m dom))

  ;; ---- statements ------------------------------------------------------
  (define (replace! id g) (mfn-replace! m id g))
  (define (cprop-into-stmt! id)
    (define g (mfn-stmt m id))
    (define changed #f)
    (define last-prop #f)
    (define g*
      (for/fold ([g g]) ([op (in-list (stmt-real-uses g))])
        (cond
          [(and last-prop (ssa-name=? op last-prop)) g]
          [else
           (define v0 (ssa-value op))
           (define val
             (or v0
                 (let ([r (rg-range-of-expr R op id)])
                   (and r (not (ir-undefined? r)) (ir-singleton r)
                        (build-int-cst st (tree-type op) (ir-singleton r))))))
           (cond
             [(and val (not (equal? (tkey val) (tkey op)))
                   (may-propagate-copy st op val)
                   (or (icst? val)
                       (let ([did (mfn-def m op)])
                         (not (and did (mfn-phi? m did)
                                   (let ([l (loop-father (mfn-bb m did))]) (and (Loop? l) (= (Loop-header l) (mfn-bb m did)))))))))
              (set! changed #t)
              (when (ssa-name? val) (set! last-prop val))
              (stmt-map-uses st g (lambda (t) (if (ssa-name=? t op) val t)))]
             [else g])])))
    (when changed (replace! id g*))
    changed)

  (define (cond-make id true?)
    (define g (mfn-stmt m id))
    (replace! id (gcond (gcond-loc g) (gcond-flags g) 'ne_expr (bool-cst (if true? 1 0)) (bool-cst 0)
                        (gcond-true-label g) (gcond-false-label g))))

  ;; optimize_stmt -> taken edge (dest) or #f
  (define (optimize-stmt! bb id)
    (define modified (cprop-into-stmt! id))
    (when modified
      (parameterize ([follow-ssa-edges? #f]) (fold-stmt st m id (lambda (t) t))))
    (define g (mfn-stmt m id))
    (define may-optimize
      (and (not (stmt-has-side-effects? st g))
           (or (gassign? g) (and (gcall? g) (gcall-lhs g)) (gcond? g) (gswitch? g))))
    (let/ec return
      (when may-optimize
        (when (gcond? g)
          (define lhs (gcond-lhs g)) (define rhs (gcond-rhs g))
          (cond
            [(and (ssa-name? lhs) (has-boolean-range? lhs) (icst? rhs) (not (or (integer-zerop rhs) (integer-onep rhs))))
             (replace! id (gcond (gcond-loc g) (gcond-flags g) (gcond-code g) (build-int-cst st (tree-type lhs) 0) rhs
                                 (gcond-true-label g) (gcond-false-label g)))
             (set! modified #t)]
            [(ssa-name? lhs)
             ;; simplify_using_ranges::fold_cond
             (define r (rg-range-of-stmt R id))
             (when (and r (not (ir-undefined? r)) (ir-singleton r) (or (ssa-name? lhs) (ssa-name? rhs)))
               (define v (ir-singleton r))
               (msg! "Folding predicate ~a to ~a" (print-cond-rhs st g) v)
               (cond-make id (not (= v 0)))
               (define-values (te fe) (true-false-edges bb))
               (hash-set! non-exec (cons bb (if (= v 0) te fe)) #t)
               (set! cfg-altered #t)
               (return (if (= v 0) fe te)))]))
        (eliminate-redundant-computations! id)
        (define g2 (mfn-stmt m id))
        ;; simple redundant store elimination
        (when (and (gassign? g2) (gassign-single-p st g2) (not (ssa-name? (gassign-lhs g2))))
          (define lhs (gassign-lhs g2))
          (define rhs (dom-valueize (gassign-rhs1 g2)))
          (define probe (stmt-set-flag (gassign #f (hasheq) rhs (tree-code lhs) lhs #f #f) 'vuse (stmt-vuse g2)))
          (define cached (lookup-avail-expr probe #f))
          (when (and cached (equal? (tkey cached) (tkey rhs)))
            (mfn-unlink-vdef! m id)
            (mfn-remove! m id)
            (return #f))))
      (define g3 (mfn-stmt m id))
      (when (gassign? g3) (record-equivalences-from-stmt! id may-optimize))
      (when (or modified (not (eq? g3 g)))
        (cond
          [(and (gcond? g3) (icst? (gcond-lhs g3)) (icst? (gcond-rhs g3)))
           (define v (fold-const-compare st (gcond-code g3) bool (gcond-lhs g3) (gcond-rhs g3)))
           (when (and v (icst? v))
             (cond-make id (not (integer-zerop v)))
             (define-values (te fe) (true-false-edges bb))
             (set! cfg-altered #t)
             (return (if (integer-zerop v) fe te)))]
          [else (void)]))
      #f))

  (define (simple-iv-increment? g)
    (and (gassign? g) (ssa-name? (gassign-lhs g)) (memq (gassign-code g) '(plus_expr minus_expr pointer_plus_expr))
         (ssa-name? (gassign-rhs1 g))
         (let loop ([pre (gassign-rhs1 g)])
           (define d (mfn-def-stmt m pre))
           (cond
             [(not d) #f]
             [(gphi? d) (for/or ([a (in-pvector (gphi-args d))]) (and (ssa-name? (cdr a)) (ssa-name=? (cdr a) (gassign-lhs g))))]
             [(and (gassign? d) (eq? (gassign-code d) 'ssa_name)) (loop (gassign-rhs1 d))]
             [else #f]))))

  ;; eliminate_redundant_computations
  (define (eliminate-redundant-computations! id)
    (define g (mfn-stmt m id))
    (define def (cond [(gphi? g) (gphi-result g)] [(gassign? g) (gassign-lhs g)] [(gcall? g) (gcall-lhs g)] [else #f]))
    (define insert (and def (ssa-name? def) (not (stmt-vdef g)) (not (simple-iv-increment? g))))
    (define cached (lookup-avail-expr g insert))
    (cond
      [(gphi? g) (when (and def cached) (record-const-or-copy! def cached))]
      [(not cached) (void)]
      [(gassign? g)
       ;; may_propagate_copy_into_stmt
       (when (or (not (ssa-name? cached))
                 (if (gassign-single-p st g)
                     (may-propagate-copy st (gassign-rhs1 g) cached)
                     (useless-type-conversion-p st (tree-type (gassign-lhs g)) (tree-type cached))))
         (replace! id (gassign (gassign-loc g) (gassign-flags g) (gassign-lhs g) (tree-code cached) cached #f #f)))]
      [(gcond? g)
       (when (icst? cached) (cond-make id (not (integer-zerop cached))))]
      [else (void)]))

  ;; record_equivalences_from_stmt
  (define (record-equivalences-from-stmt! id may-optimize)
    (define g (mfn-stmt m id))
    (define lhs (gassign-lhs g))
    (when (and (ssa-name? lhs) (gassign-single-p st g) may-optimize)
      (define rhs (gassign-rhs1 g))
      (when (or (ssa-name? rhs) (is-gimple-min-invariant st rhs))
        (set-ssa-value! lhs (dom-valueize rhs))))
    (when (and (not (stmt-has-volatile-ops? g)) (stmt-vdef g) (gassign-single-p st g)
               (let ([rhs (gassign-rhs1 g)]) (or (ssa-name? rhs) (is-gimple-min-invariant st rhs)))
               (not (ssa-name? lhs)))
      (define probe (stmt-set-flag (gassign #f (hasheq) (gassign-rhs1 g) (tree-code lhs) lhs #f #f) 'vuse (stmt-vdef g)))
      (lookup-avail-expr probe #t)))

  ;; record_equivalences_from_phis
  (define (record-equivalences-from-phis! bb)
    (for ([pid (in-list (mfn-phis m bb))])
      (define ph (mfn-stmt m pid))
      (define lhs (gphi-result ph))
      (define-values (rhs complete?)
        (let loop ([args (pvector->list (gphi-args ph))] [rhs #f])
          (cond
            [(null? args) (values rhs #t)]
            [else
             (define a (car args))
             (define t (cdr a))
             (cond
               [(and (ssa-name? t) (ssa-name=? t lhs)) (loop (cdr args) rhs)]
               [(not (executable? (car a) bb)) (loop (cdr args) rhs)]
               [else
                (define tv (dom-valueize t))
                (cond
                  [(and (ssa-name? tv) (dfs-back? (car a) bb)) (values rhs #f)]
                  [(not rhs) (loop (cdr args) tv)]
                  [(equal? (tkey rhs) (tkey tv)) (loop (cdr args) rhs)]
                  [else (values rhs #f)])])])))
      (when complete?
        (define r (or rhs lhs))
        (when (may-propagate-copy st lhs r) (set-ssa-value! lhs r)))))

  ;; cprop_into_successor_phis
  (define (cprop-into-successor-phis! bb)
    (for ([d (in-list (succs bb))] #:unless (null? (mfn-phis m d)))
      (copies-push-marker!)
      (define x (edge-info bb d))
      (when x (for ([p (in-list (ei-simple x))] #:when (ssa-name? (car p))) (record-const-or-copy! (car p) (cdr p))))
      (for ([pid (in-list (mfn-phis m d))])
        (define ph (mfn-stmt m pid))
        (define new-args
          (for/pvector ([a (in-pvector (gphi-args ph))])
            (define orig (cdr a))
            (cond
              [(and (= (car a) bb) (ssa-name? orig))
               (define nv (ssa-value orig))
               (if (and nv (not (equal? (tkey nv) (tkey orig))) (may-propagate-copy st orig nv)) (cons (car a) nv) a)]
              [else a])))
        (unless (equal? new-args (gphi-args ph)) (replace! pid (phi-set-args ph new-args))))
      (copies-pop-to-marker!)))

  ;; ---- the jump threader ----------------------------------------------
  (define stmt-count 0)
  (define jt-blocks '())                         ; jt_state::m_blocks, 'marker separated
  (define (jt-push! s d)
    (copies-push-marker!) (avail-push-marker!)
    (set! jt-blocks (append jt-blocks (if (null? jt-blocks) (list 'marker s d) (list 'marker d)))))
  (define (jt-pop!)
    (copies-pop-to-marker!) (avail-pop-to-marker!)
    (let loop ()
      (unless (null? jt-blocks)
        (define l (last jt-blocks))
        (set! jt-blocks (drop-right jt-blocks 1))
        (unless (eq? l 'marker) (loop)))))
  (define (jt-append! bb) (set! jt-blocks (append jt-blocks (list bb))))
  (define paths '())

;; empty_block_with_phis_p: gsi_start_nondebug_bb, a label is a statement
  (define (empty-block-with-phis? bb)
    (and (null? (mfn-stmts m bb)) (pair? (mfn-phis m bb))))
  (define (control-stmt? g) (and g (or (gcond? g) (ggoto? g) (gswitch? g))))
  (define (potentially-threadable? bb)
    (cond
      [(empty-block-with-phis? bb) #t]
      [(or (= 1 (length (succs bb))) (= 1 (length (preds bb)))) #f]
      [else (control-stmt? (last-stmt bb))]))

  (define (record-temporary-equivalences-from-phis! s d)
    (let/ec return
      (for ([pid (in-list (mfn-phis m d))])
        (define ph (mfn-stmt m pid))
        (define src (phi-arg-for-pred ph s))
        (define dst (gphi-result ph))
        (when (and (ssa-name? src) (not (ssa-name=? src dst))
                   (let ([did (mfn-def m src)]) (and did (mfn-phi? m did) (= (mfn-bb m did) d))))
          (return #f))
        (unless (ssa-virtual? dst) (set! stmt-count (add1 stmt-count)))
        (record-const-or-copy! dst src))
      #t))

  ;; estimate_threading_killed_stmts
  (define (uses-in-bb t bb)
    (define-values (uses outside)
      (for/fold ([u 0] [o #f]) ([uid (in-list (mfn-use-ids m t))])
        (if (= (mfn-bb m uid) bb) (values (+ u (count-uses (mfn-stmt m uid) t)) o) (values u #t))))
    (cond [(and outside (> uses 1)) -2] [outside -1] [else uses]))
  (define (count-uses g t) (for/sum ([u (in-list (stmt-use-names g))]) (if (ssa-name=? u t) 1 0)))
  (define (estimate-killed bb)
    (define drop-all-phis (= 2 (length (preds bb))))
    (define killed0
      (if drop-all-phis (for/sum ([pid (in-list (mfn-phis m bb))]) (if (ssa-virtual? (gphi-result (mfn-stmt m pid))) 0 1)) 0))
    (define g (last-stmt bb))
    (define killed
      (cond
        [(not (control-stmt? g)) killed0]
        [else
         (define remaining (make-hasheqv))
         (let loop ([work (list g)] [k (add1 killed0)])
           (cond
             [(null? work) k]
             [else
              (define s (car work))
              (define-values (k* work*)
                (for/fold ([k k] [w (cdr work)]) ([t (in-list (stmt-real-uses s))])
                  (define did (mfn-def m t))
                  (define def (and did (mfn-stmt m did)))
                  (cond
                    [(and def (= (mfn-bb m did) bb) (or (not (gphi? def)) (not drop-all-phis)) (not (stmt-has-side-effects? st def)))
                     (define usesp (hash-ref remaining (ver t) #f))
                     (define uses0 (or usesp (uses-in-bb t bb)))
                     (when (and (not usesp) (or (< uses0 -1) (> uses0 1))) (hash-set! remaining (ver t) uses0))
                     (cond
                       [(< uses0 0) (values k w)]
                       [else
                        (define uses (sub1 uses0))
                        (when (hash-ref remaining (ver t) #f) (hash-set! remaining (ver t) uses))
                        (cond
                          [(= uses 0)
                           (hash-remove! remaining (ver t))
                           (values (add1 k) (if (gphi? def) w (append w (list def))))]
                          [else (values k w)])])]
                    [else (values k w)])))
              (loop work* k*)]))]))
    (msg! "threading bb ~a kills ~a stmts" bb killed)
    killed)

  ;; record_temporary_equivalences_from_stmts_at_dest -> last stmt id or #f
  (define (record-temporary-equivalences-from-stmts! s d)
    (let/ec return
      (define max-count PARAM-MAX-JUMP-THREAD-DUPLICATION-STMTS)
      (define last #f)
      (for ([id (in-list (mfn-stmts m d))])
        (define g (mfn-stmt m id))
        (set! last id)
        ;; __builtin_constant_p: constant on each path, not after the merge
        (when (builtin-call? g "__builtin_constant_p") (return #f))
        (unless (glabel? g)
          (set! stmt-count (add1 stmt-count))
          (when (> stmt-count max-count)
            (when (= max-count PARAM-MAX-JUMP-THREAD-DUPLICATION-STMTS)
              (set! max-count (+ max-count (estimate-killed d)))
              (msg! "threading bb ~a up to ~a stmts" d max-count))
            (when (> stmt-count max-count) (return #f)))
          (when (and (or (and (gassign? g) (ssa-name? (gassign-lhs g)))
                         (and (gcall? g) (gcall-lhs g) (ssa-name? (gcall-lhs g))))
                     (not (builtin-call? g "__builtin_object_size")))
            (register-equivs-stmt! id))))
      last))
  (define (builtin-call? g name)
    (and (gcall? g) (let ([d (call-fndecl g)]) (and d (call-fndecl-builtin? d) (equal? (decl-name d) name)))))

  (define (register-equivs-stmt! id)
    (define g (mfn-stmt m id))
    (define lhs (if (gassign? g) (gassign-lhs g) (gcall-lhs g)))
    (define cached0
      (cond
        [(and (gassign? g) (gassign-single-p st g) (ssa-name? (gassign-rhs1 g))) (gassign-rhs1 g)]
        [else (parameterize ([follow-ssa-edges? #f]) (gimple-fold-stmt-to-constant st m g dom-valueize))]))
    (define cached
      (cond
        [(and (pair? (stmt-use-names g))
              (or (not cached0) (and (not (ssa-name? cached0)) (not (is-gimple-min-invariant st cached0)))))
         ;; temporarily copy-propagate the operands, the VUSE included
         ;; (SSA_OP_ALL_USES), and look the statement up; restoring the
         ;; operands with SET_USE relinks every use of the statement
         (define g0* (stmt-map-uses st g (lambda (t) (or (ssa-value t) t))))
         (define v (stmt-vuse g))
         (define g* (if (and v (ssa-name? v) (ssa-value v)) (stmt-set-flag g0* 'vuse (ssa-value v)) g0*))
         (begin0 (lookup-avail-expr g* #f)
                 (mfn-relink! m id))]
        [else cached0]))
    (when (and cached (or (ssa-name? cached) (is-gimple-min-invariant st cached)))
      (record-const-or-copy! lhs cached)))

  ;; simplify_control_stmt_condition -> tree or #f
  (define (simplify-control-condition s d id)
    (define g (mfn-stmt m id))
    (define (val2 t) (let loop ([t t] [i 0]) (if (and (< i 2) (ssa-name? t) (ssa-value t)) (loop (ssa-value t) (add1 i)) t)))
    (cond
      [(gcond? g)
       (define op0 (val2 (gcond-lhs g))) (define op1 (val2 (gcond-rhs g)))
       (define cached (simplify-condition-1 (gcond-code g) op0 op1))
       (cond
         [cached cached]
         [(and (ssa-name? (gcond-lhs g)) (icst? (gcond-rhs g))
               (let ([ty (tree-type (gcond-lhs g))]) (or (integral-type-p st ty) (pointer-type-p st ty))))
          (gcond-lhs g)]
         [else #f])]
      [(gswitch? g)
       (define idx (gswitch-index g))
       (cond
         [(ssa-name? idx)
          (define c (val2 idx))
          (cond
            [(is-gimple-min-invariant st c) c]
            [else
             ;; dom_jt_simplifier: the available expression table (the path ranger is not ported)
             (or (lookup-avail-expr (gswitch (gswitch-loc g) (gswitch-flags g) c (gswitch-default-case g) (gswitch-cases g)) #f)
                 idx)])]
         [else #f])]
      [else #f]))
  ;; simplify_control_stmt_condition_1 (LIMIT = 4 from the caller)
  (define (simplify-condition-1 code0 a0 b0 [limit 4])
    (define-values (code a b) (if (tree-swap-operands-p st a0 b0) (values (swap-tree-comparison code0) b0 a0) (values code0 a0 b0)))
    (cond
      [(= limit 0) #f]
      [(and (< major 15) (simplify-condition-recurse code a b limit)) => values]
      [else (simplify-condition-fold code a b)]))
  ;; GCC <= 14: (A & B) CMP 0, (A | B) CMP 0 and (A CMP B) CMP 0 for
  ;; CMP = EQ / NE look into A and B (or A CMP B) through op0's definition
  (define (simplify-condition-recurse code a b limit)
    (define def (and (memq code '(eq_expr ne_expr)) (ssa-name? a) (icst? b) (integer-zerop b) (mfn-def-stmt m a)))
    (define (t) (bool-cst 1)) (define (f) (bool-cst 0))
    (define (zero? r) (and r (icst? r) (integer-zerop r)))
    (define (nonzero? r) (and r (icst? r) (not (integer-zerop r))))
    (cond
      [(not (and def (gassign? def))) #f]
      [(memq (gassign-code def) '(bit_and_expr bit_ior_expr))
       (define and? (eq? (gassign-code def) 'bit_and_expr))
       (define ne? (eq? code 'ne_expr))
       (let/ec return
         (define (check r)
           (cond
             [(and and? (zero? r)) (return (if ne? (f) (t)))]
             [(and (not and?) (nonzero? r)) (return (if ne? (t) (f)))]
             [else (void)]))
         (define r1 (simplify-condition-1 'ne_expr (gassign-rhs1 def) b (sub1 limit)))
         (check r1)
         (define r2 (simplify-condition-1 'ne_expr (gassign-rhs2 def) b (sub1 limit)))
         (check r2)
         (cond
           [(and r1 r2 and? (= 1 (type-precision* st (tree-type a))) (nonzero? r1) (nonzero? r2)) (if ne? (t) (f))]
           [(and r1 r2 (not and?) (zero? r1) (zero? r2)) (if ne? (f) (t))]
           [else #f]))]
      [(memq (gassign-code def) COMPARISON-CODES)
       (define nc (if (eq? code 'eq_expr) (invert-tree-comparison (gassign-code def)) (gassign-code def)))
       (define r (and nc (simplify-condition-1 nc (gassign-rhs1 def) (gassign-rhs2 def) (sub1 limit))))
       (and r (is-gimple-min-invariant st r) r)]
      [else #f]))
  (define (simplify-condition-fold code a b)
    (define res
      (cond
        [(and (icst? a) (icst? b)) (fold-const-compare st code bool a b)]
        [(and (ssa-name? a) (ssa-name? b) (ssa-name=? a b) (integral-type-p st (tree-type a)))
         (bool-cst (if (memq code '(eq_expr le_expr ge_expr)) 1 0))]
        [else #f]))
    (if (and res (is-gimple-min-invariant st res))
        res
        (or (lookup-avail-expr (gcond #f (hasheq) code a b #f #f) #f)
            (path-simplify code a b))))
  ;; hybrid_jt_simplifier (GCC >= 13): the comparison's range along the
  ;; jt_state path
  (define (path-simplify code a b)
    (define path (filter (lambda (x) (not (eq? x 'marker))) jt-blocks))
    (and (>= major 13) (> (length path) 1)
         (let ([v (rg-path-range-of-cond R path code a b)])
           (and v (bool-cst v)))))

  (define (taken-edge-for d cond-val)
    (define g (last-stmt d))
    (cond
      [(gcond? g)
       (define-values (te fe) (true-false-edges d))
       (if (integer-zerop cond-val) fe te)]
      [(gswitch? g)
       (define v (integer-cst-value cond-val))
       (define hit
         (for/first ([cs (in-pvector (gswitch-cases g))]
                     #:when (let ([lo (integer-cst-value (tree-operand cs 0))]
                                  [hi (if (tree-operand cs 1) (integer-cst-value (tree-operand cs 1)) (integer-cst-value (tree-operand cs 0)))])
                              (and (<= lo v) (<= v hi))))
           cs))
       (label-to-block cfg (tree-operand (or hit (gswitch-default-case g)) 2))]
      [else #f]))

  ;; -> 1 threaded, 0 not, -1 the block cannot be a joiner
  (define (thread-through-normal-block! path s d visited limit)
    (cond
      [(= (unbox limit) 0) 0]
      [else
       (set-box! limit (sub1 (unbox limit)))
       (record-temporary-equivalences! s d)
       (cond
         [(not (record-temporary-equivalences-from-phis! s d)) -1]
         [else
          (define sid (record-temporary-equivalences-from-stmts! s d))
          (cond
            [(not sid) (if (empty-block-with-phis? d) 0 -1)]
            [(control-stmt? (mfn-stmt m sid))
             (define c (simplify-control-condition s d sid))
             (cond
               [(and c (is-gimple-min-invariant st c))
                (define dest (taken-edge-for d c))
                (cond
                  [(or (not dest) (= dest d) (dfs-back? d dest) (hash-ref visited dest #f)) 0]
                  [else
                   (when (null? (unbox path)) (set-box! path (list (list s d 'start))))
                   (set-box! path (append (unbox path) (list (list d dest 'copy-src))))
                   (jt-append! dest)
                   (hash-set! visited dest #t) (hash-set! visited d #t)
                   (thread-around-empty-blocks! path d dest visited limit)
                   1])]
               [else 0])]
            [else 0])])]))

  (define (thread-around-empty-blocks! path s d visited limit)
    (cond
      [(= (unbox limit) 0) #f]
      [else
       (set-box! limit (sub1 (unbox limit)))
       (cond
         [(pair? (mfn-phis m d)) #f]
         [else
          ;; gsi_start_nondebug_bb: a label stops the walk like any
          ;; statement other than a control statement
          (define real (mfn-stmts m d))
          (cond
            [(null? real)
             (cond
               [(= 1 (length (succs d)))
                (define nd (car (succs d)))
                (cond
                  [(dfs-back? d nd) #f]
                  [(hash-ref visited nd #f) #f]
                  [else
                   (set-box! path (append (unbox path) (list (list d nd 'no-copy))))
                   (jt-append! nd)
                   (hash-set! visited nd #t)
                   (thread-around-empty-blocks! path d nd visited limit)])]
               [else #f])]
            [(not (control-stmt? (mfn-stmt m (car real)))) #f]
            [else
             (define c (simplify-control-condition s d (car real)))
             (cond
               [(and c (is-gimple-min-invariant st c))
                (define nd (taken-edge-for d c))
                (cond
                  [(or (not nd) (dfs-back? d nd) (hash-ref visited nd #f)) #f]
                  [else
                   (hash-set! visited nd #t)
                   (set-box! path (append (unbox path) (list (list d nd 'no-copy))))
                   (jt-append! nd)
                   (thread-around-empty-blocks! path d nd visited limit)
                   #t])]
               [else #f])])])]))

  (define (edge-forwards-cmp-to-conditional-jump? s d)
    (define real (filter (lambda (id) (not (glabel? (mfn-stmt m id)))) (mfn-stmts m d)))
    (and (= 1 (length real))
         (let ([g (mfn-stmt m (car real))])
           (and (gcond? g) (ssa-name? (gcond-lhs g)) (memq (gcond-code g) '(ne_expr eq_expr))
                (or (integer-onep (gcond-rhs g)) (integer-zerop (gcond-rhs g)))
                (let* ([did (mfn-def m (gcond-lhs g))] [ph (and did (mfn-stmt m did))])
                  (and ph (gphi? ph) (= (mfn-bb m did) d)
                       (let ([v (phi-arg-for-pred ph s)])
                         (and (ssa-name? v) (mfn-single-use? m v)
                              (let* ([def (mfn-def-stmt m v)]
                                     [def* (if (and def (gassign? def) (memq (gassign-code def) '(nop_expr convert_expr)))
                                               (let ([v2 (gassign-rhs1 def)])
                                                 (and (ssa-name? v2) (mfn-single-use? m v2) (mfn-def-stmt m v2)))
                                               def)])
                                (and def* (gassign? def*)
                                     (memq (gassign-code def*) COMPARISON-CODES)))))))))))

  ;; jt_path_registry::cancel_invalid_paths -> #t when cancelled
  (define (cancel-invalid-path? path)
    (define entry (first path)) (define exit (last path))
    (define l (loop-father (cadr entry)))
    (define latch (loop-latch l))
    (define-values (seen-latch crossed-latch loops-crossed crossed-header _cur)
      (for/fold ([sl #f] [cl #f] [lc 0] [ch #f] [cur l]) ([e (in-list path)])
        (define es (car e)) (define ed (cadr e))
        (define sl1 (or sl (and latch (or (= latch es) (= latch ed)))))
        (define cl1 (or cl (and latch (or (= latch es) (= latch ed)) (not (= es (car entry))))))
        (define df (loop-father ed))
        (define-values (lc1 cur1) (if (eq? df cur) (values lc cur) (values (add1 lc) df)))
        (define ch1 (or ch (and (Loop? df) (= (Loop-header df) ed) (not (nested? (loop-father (cadr exit)) df)))))
        (values sl1 cl1 lc1 ch1 cur1)))
    (cond
      [(and (= loops-crossed 1) (not crossed-latch) (nested? (loop-father (cadr exit)) (loop-father (car exit)))) #f]
      [loop-opts-done #f]
      [(and seen-latch latch (null? (mfn-phis m latch))
            (for/and ([id (in-list (mfn-stmts m latch))]) (glabel? (mfn-stmt m id))))
       #t]
      [(> loops-crossed 0) #t]
      [(and (not (eq? (loop-father (car entry)) (loop-father (cadr exit))))
            (not (nested? (loop-father (car exit)) (loop-father (cadr entry)))))
       #t]
      [crossed-header #t]
      [else #f]))
  (define (register-jump-thread! path)
    (unless (cancel-invalid-path? path) (set! paths (cons path paths))))

  (define (thread-across-edge! s d)
    (jt-push! s d)
    (set! stmt-count 0)
    (define path (box '()))
    (define visited (make-hasheqv))
    (hash-set! visited s #t) (hash-set! visited d #t)
    ;; param_max_jump_thread_paths exists in GCC >= 15
    (define limit (box (if (>= major 15) PARAM-MAX-JUMP-THREAD-PATHS +inf.0)))
    (define threaded (if (dfs-back? s d) 0 (thread-through-normal-block! path s d visited limit)))
    (cond
      [(> threaded 0) (register-jump-thread! (unbox path)) (jt-pop!)]
      [(< threaded 0) (jt-pop!)]
      [else
       ;; E->dest as a joiner
       (for ([td (in-list (succs d))])
         (unless (or (dfs-back? s d) (dfs-back? d td))
           (jt-push! d td)
           (define vis (make-hasheqv))
           (hash-set! vis s #t) (hash-set! vis d #t) (hash-set! vis td #t)
           (define p (box (list (list s d 'start) (list d td 'copy-joiner))))
           (define found
             (or (thread-around-empty-blocks! p d td vis limit)
                 (let ([lst (last (unbox p))]) (> (thread-through-normal-block! p (car lst) (cadr lst) vis limit) 0))))
           (when (or found (edge-forwards-cmp-to-conditional-jump? s d))
             (register-jump-thread! (unbox p)))
           (jt-pop!)))
       (jt-pop!)]))

  (define (thread-outgoing-edges! bb)
    (define ss (succs bb))
    (cond
      [(and (= 1 (length ss)) (potentially-threadable? (car ss))) (thread-across-edge! bb (car ss))]
      [(and (gcond? (last-stmt bb)) (= 2 (length ss)))
       (define-values (te fe) (true-false-edges bb))
       (when (potentially-threadable? te) (thread-across-edge! bb te))
       (when (potentially-threadable? fe) (thread-across-edge! bb fe))]
      [else (void)]))

  ;; ---- the dominator walk ------------------------------------------------
  (define (single-pred-ignoring-loop-edges bb)
    (let/ec return
      (for/fold ([ret #f]) ([p (in-list (preds bb))])
        (cond
          [(dominated-by? p bb) ret]
          [(not (executable? p bb)) ret]
          [ret (return #f)]
          [else p]))))
  (define (reachable? bb)
    (or (= bb ENTRY) (for/or ([p (in-list (preds bb))]) (executable? p bb))))

  (define (before! bb)
    (avail-push-marker!) (copies-push-marker!)
    (hash-set! blocks-on-stack bb #t)
    (define parent (idom bb))
    (define p (and (not (= bb ENTRY)) (single-pred-ignoring-loop-edges bb)))
    (when (and p (eqv? p parent)) (record-temporary-equivalences! p bb))
    (record-equivalences-from-phis! bb)
    (avail-push-marker!)
    (for ([pid (in-list (mfn-phis m bb))]) (eliminate-redundant-computations! pid))
    (avail-pop-to-marker!)
    (define taken
      (for/fold ([taken #f]) ([id (in-list (if (= bb ENTRY) '() (mfn-stmts m bb)))])
        (optimize-stmt! bb id)))
    (unless (= bb ENTRY)
      (record-edge-info! bb)
      (cprop-into-successor-phis! bb))
    (when taken
      (for ([d (in-list (succs bb))] #:unless (= d taken)) (hash-set! non-exec (cons bb d) #t))))
  (define (after! bb)
    (thread-outgoing-edges! bb)
    (hash-remove! blocks-on-stack bb)
    (avail-pop-to-marker!) (copies-pop-to-marker!))
  (define (walk bb)
    (cond
      [(not (reachable? bb))
       (for ([d (in-list (succs bb))]) (hash-set! non-exec (cons bb d) #t))]
      [else
       (before! bb)
       (for ([k (in-list (dom-kids bb))]) (walk k))
       (after! bb)]))

  (for ([bb (in-pvector (cfg-bbs cfg))]) (record-edge-info! bb))
  (walk ENTRY)

  ;; export_global_ranges
  (define-values (st1 lines) (rg-export-global-ranges R st))
  (when (>= major 15)
    (msg! "Exporting new  global ranges:")
    (msg! "============================")
    (for ([l (in-list lines)]) (msg! "~a" l))
    (msg! "========= Done ============="))
  (define cfg1 (for/fold ([c (mfn->gcfg m)]) ([l (in-list (reverse messages))]) (cfg-add-message c l)))
  ;; thread_through_all_blocks, then TODO_cleanup_cfg | TODO_update_ssa
  ;; (loop_optimizer_finalize dropped the simple-latch / preheader
  ;; protection); the duplicates' names are rewritten by the update_ssa
  ;; that cleanup_tree_cfg runs after cleanup_control_flow_pre
  (cond
    [(null? paths) (cleanup-tree-cfg st1 cfg1 print-cond-rhs)]
    [else
     ;; remove_jump_threads_including: when a condition was folded, every
     ;; edge leaving a block with a non-executable successor edge
     (define removed
       (and cfg-altered
            (for*/hash ([bb (in-pvector (cfg-bbs cfg))]
                        #:when (for/or ([d (in-list (succs bb))]) (not (executable? bb d)))
                        [d (in-list (succs bb))])
              (values (cons bb d) #t))))
     (define-values (cfg2 st2 table _threaded?) (thread-through-all-blocks st1 fndecl cfg1 (reverse paths) loops #:may-peel-loop-headers? may-peel-loop-headers?
                                                                           #:removed-edges removed))
     (cleanup-tree-cfg st2 cfg2 print-cond-rhs
                       #:after-pre (lambda (st c pre) (if (null? table) (values c st) (update-ssa st fndecl c table #:released-before pre))))]))
