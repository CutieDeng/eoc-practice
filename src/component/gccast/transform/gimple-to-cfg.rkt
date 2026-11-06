#lang racket/base

;; ============================================================
;; gimple-to-cfg: pass_build_cfg (gcc/tree-cfg.cc) and the
;; cleanup_tree_cfg it runs (gcc/tree-cfgcleanup.cc), -O0 C subset
;; ============================================================
;;
;; Function-by-function port of GCC trunk (17.0.0).  The algorithm
;; is inherently mutable (statements move between blocks, edges are
;; redirected in place), so blocks and edges are mutable structs;
;; every sequence is a pvector.
;;
;; execute_build_cfg:
;;   build_gimple_cfg
;;     make_blocks            (stmt_starts_bb_p / stmt_ends_bb_p)
;;     cleanup_dead_labels    (1st)
;;     group_case_labels
;;     make_edges             (goto statements are removed here)
;;     assign_discriminators  (changes location equality!)
;;     cleanup_dead_labels    (2nd)
;;   cleanup_tree_cfg
;;     cleanup_control_flow_pre  (DFS; folds constant conds; removes
;;                                unreachable blocks, "Removing basic block N")
;;     cleanup_tree_cfg_bb loop  (forwarder removal / block merging,
;;                                gated by the -O0 location checks)
;;     end_recording_case_labels, compact_blocks
;;
;; Orderings that the dump exposes and that are reproduced exactly:
;;   - block chain order (new blocks link after the previous one,
;;     merge moves B after A, compact_blocks renumbers along the chain)
;;   - succ/pred vectors push on creation and use unordered_remove
;;     (last element moves into the hole) on removal
;;   - LABEL_DECL_UID assigned on first gimple_set_bb, in statement order
;;
;; Not ported (no fixture needs them yet): abnormal/EH edges, computed
;; goto, setjmp, generate_range_test in convert_single_case_switch,
;; dominator-based parts (no PHIs exist at this point).
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../generic/tree.rkt"
         "../generic/gimple.rkt")

(provide build-cfg
         (struct-out gcfg) (struct-out bb) (struct-out edge)
         EDGE-FALLTHRU EDGE-TRUE-VALUE EDGE-FALSE-VALUE
         for-each-bb cfg-bbs bb-last-stmt single-succ-p single-succ-edge find-fallthru-edge
         extract-true-false-edges)

;; ------------------------------------------------------------
;; Data
;; ------------------------------------------------------------

;; edge flags (subset of EDGE_* in cfg-flags.def; values are our own)
(define EDGE-FALLTHRU     1)
(define EDGE-ABNORMAL     2)
(define EDGE-EH           4)
(define EDGE-TRUE-VALUE   8)
(define EDGE-FALSE-VALUE 16)
(define EDGE-COMPLEX (bitwise-ior EDGE-ABNORMAL EDGE-EH))

(struct edge ([src #:mutable] [dest #:mutable] [flags #:mutable] [goto-locus #:mutable]))
(struct bb ([index #:mutable] [stmts #:mutable] [succs #:mutable] [preds #:mutable]
            [prev #:mutable] [next #:mutable]))                ; stmts/succs/preds: pvector
(struct gcfg (entry exit
              [blocks #:mutable]            ; pvector index -> bb or #f   (last_basic_block = length)
              [last-label-uid #:mutable]    ; cfg->last_label_uid
              label->bb                     ; mutable hasheq LABEL_DECL -> bb
              ctrl-altering                 ; mutable hasheq gcall -> #t  (GF_CALL_CTRL_ALTERING)
              [messages #:mutable]          ; pvector of dump lines ("Removing basic block 4" ...)
              [altered #:mutable]           ; cfgcleanup_altered_bbs (intbits)
              [touched-switch #:mutable]))  ; touched_switch_bbs (intbits)

(define (dump! cfg fmt . args)
  (set-gcfg-messages! cfg (pvector-cons-right (gcfg-messages cfg) (apply format fmt args))))

;; ---- block chain -------------------------------------------------

(define (link-block! b after)
  (set-bb-next! b (bb-next after))
  (set-bb-prev! b after)
  (set-bb-next! after b)
  (set-bb-prev! (bb-next b) b))

(define (unlink-block! b)
  (set-bb-prev! (bb-next b) (bb-prev b))
  (set-bb-next! (bb-prev b) (bb-next b))
  (set-bb-prev! b #f)
  (set-bb-next! b #f))

;; FOR_EACH_BB_FN: the chain between ENTRY and EXIT
(define (cfg-bbs cfg)
  (let loop ([b (bb-next (gcfg-entry cfg))] [acc (pvector-empty)])
    (if (eq? b (gcfg-exit cfg)) acc (loop (bb-next b) (pvector-cons-right acc b)))))
(define (for-each-bb cfg f) (for ([b (in-pvector (cfg-bbs cfg))]) (f b)))

;; create_bb: index = last_basic_block++, linked after AFTER
(define (create-bb! cfg stmts after)
  (define b (bb (pvector-length (gcfg-blocks cfg)) stmts (pvector-empty) (pvector-empty) #f #f))
  (set-gcfg-blocks! cfg (pvector-cons-right (gcfg-blocks cfg) b))
  (link-block! b after)
  b)

;; expunge_block
(define (expunge-block! cfg b)
  (unlink-block! b)
  (set-gcfg-blocks! cfg (pvector-set (gcfg-blocks cfg) (bb-index b) #f)))

(define (block-at cfg i) (pvector-ref (gcfg-blocks cfg) i))

;; ---- statements ---------------------------------------------------

;; gimple_set_bb: a label gets its LABEL_DECL_UID on first assignment and
;; (re)registers itself in label_to_block_map.
(define (note-stmt-bb! cfg g b)
  (when (glabel? g)
    (define l (glabel-label g))
    (unless (attr l 'label-uid #f)
      (set-attr! l 'label-uid (gcfg-last-label-uid cfg))
      (set-gcfg-last-label-uid! cfg (add1 (gcfg-last-label-uid cfg))))
    (hash-set! (gcfg-label->bb cfg) l b)))

(define (bb-append-stmt! cfg b g)
  (note-stmt-bb! cfg g b)
  (set-bb-stmts! b (pvector-cons-right (bb-stmts b) g)))

(define (bb-insert-stmt-at! cfg b i g)
  (note-stmt-bb! cfg g b)
  (define s (bb-stmts b))
  (set-bb-stmts! b (pvector-append (pvector-cons-right (pvector-take s i) g) (pvector-drop s i))))

(define (bb-remove-stmt-at! b i)
  (define s (bb-stmts b))
  (set-bb-stmts! b (pvector-append (pvector-take s i) (pvector-drop s (add1 i)))))

(define (bb-remove-stmt! b g)
  (bb-remove-stmt-at! b (pvector-index-of (bb-stmts b) g)))

(define (bb-last-stmt b) (if (pvector-empty? (bb-stmts b)) #f (pvector-last (bb-stmts b))))
;; first_stmt (no debug statements here)
(define (bb-first-stmt b) (if (pvector-empty? (bb-stmts b)) #f (pvector-first (bb-stmts b))))
;; gsi_after_labels
(define (bb-first-non-label-stmt b)
  (for/first ([g (in-pvector (bb-stmts b))] #:unless (glabel? g)) g))
;; number of leading labels
(define (bb-leading-label-count b)
  (let loop ([i 0]) (if (and (< i (pvector-length (bb-stmts b))) (glabel? (pvector-ref (bb-stmts b) i))) (loop (add1 i)) i)))

(define (label-to-block cfg l) (hash-ref (gcfg-label->bb cfg) l #f))

;; ---- edges -------------------------------------------------------

(define (pvector-unordered-remove v i)
  (define n (pvector-length v))
  (if (= i (sub1 n))
      (pvector-pop-right v)
      (pvector-pop-right (pvector-set v i (pvector-last v)))))

(define (find-edge src dest)
  (for/first ([e (in-pvector (bb-succs src))] #:when (eq? (edge-dest e) dest)) e))

(define (connect-src! e) (set-bb-succs! (edge-src e) (pvector-cons-right (bb-succs (edge-src e)) e)))
(define (connect-dest! e) (set-bb-preds! (edge-dest e) (pvector-cons-right (bb-preds (edge-dest e)) e)))
(define (disconnect-src! e)
  (define s (edge-src e))
  (set-bb-succs! s (pvector-unordered-remove (bb-succs s) (pvector-index-of (bb-succs s) e))))
(define (disconnect-dest! e)
  (define d (edge-dest e))
  (set-bb-preds! d (pvector-unordered-remove (bb-preds d) (pvector-index-of (bb-preds d) e))))

;; make_edge: returns #f (and ORs the flags) when the edge already exists
(define (make-edge! src dest flags)
  (define e (find-edge src dest))
  (cond
    [e (set-edge-flags! e (bitwise-ior (edge-flags e) flags)) #f]
    [else
     (define n (edge src dest flags #f))
     (connect-src! n) (connect-dest! n)
     n]))

(define (remove-edge! e) (disconnect-src! e) (disconnect-dest! e))

(define (redirect-edge-succ! e new)
  (disconnect-dest! e)
  (set-edge-dest! e new)
  (connect-dest! e))

;; redirect_edge_succ_nodup
(define (redirect-edge-succ-nodup! e new)
  (define s (find-edge (edge-src e) new))
  (cond
    [(and s (not (eq? s e)))
     (set-edge-flags! s (bitwise-ior (edge-flags s) (edge-flags e)))
     (remove-edge! e)
     s]
    [else (redirect-edge-succ! e new) e]))

(define (single-succ-p b) (= 1 (pvector-length (bb-succs b))))
(define (single-pred-p b) (= 1 (pvector-length (bb-preds b))))
(define (single-succ-edge b) (pvector-first (bb-succs b)))
(define (single-pred-edge b) (pvector-first (bb-preds b)))
(define (single-succ b) (edge-dest (single-succ-edge b)))
(define (single-pred b) (edge-src (single-pred-edge b)))
(define (edge-flag? e f) (not (zero? (bitwise-and (edge-flags e) f))))

(define (find-fallthru-edge b)
  (for/first ([e (in-pvector (bb-succs b))] #:when (edge-flag? e EDGE-FALLTHRU)) e))

;; extract_true_false_edges_from_block
(define (extract-true-false-edges b)
  (define e0 (pvector-ref (bb-succs b) 0))
  (define e1 (pvector-ref (bb-succs b) 1))
  (if (edge-flag? e0 EDGE-TRUE-VALUE) (values e0 e1) (values e1 e0)))

;; ------------------------------------------------------------
;; Statement predicates (tree-cfg.cc)
;; ------------------------------------------------------------

(define ECF-RETURNS-TWICE 128)

(define (label-nonlocal? l) (attr l 'nonlocal #f))
(define (label-forced? l) (attr l 'forced #f))

;; gimple_call_initialize_ctrl_altering (C subset: noreturn only)
(define (call-initialize-ctrl-altering! cfg g)
  (when (gimple-call-noreturn-p g) (hash-set! (gcfg-ctrl-altering cfg) g #t)))
(define (ctrl-altering? cfg g) (hash-ref (gcfg-ctrl-altering cfg) g #f))

;; is_ctrl_stmt
(define (ctrl-stmt? g) (or (gcond? g) (gswitch? g) (ggoto? g) (greturn? g)))
;; stmt_ends_bb_p
(define (stmt-ends-bb? cfg g) (or (ctrl-stmt? g) (and (gcall? g) (ctrl-altering? cfg g))))

;; stmt_starts_bb_p
(define (stmt-starts-bb? g prev)
  (cond
    [(glabel? g)
     (define l (glabel-label g))
     (cond
       [(or (label-nonlocal? l) (label-forced? l)) #t]
       [(glabel? prev)
        (define p (glabel-label prev))
        (or (label-nonlocal? p) (not (decl-artificial p)))]
       [else #t])]
    [(gcall? g) (not (zero? (bitwise-and (gcall-flags g) ECF-RETURNS-TWICE)))]
    [else #f]))

;; ------------------------------------------------------------
;; make_blocks
;; ------------------------------------------------------------

(define (make-blocks! cfg body)
  (define b (gcfg-entry cfg))
  (define start-new #t)
  (define prev #f)
  (for ([g (in-seq body)])
    (when (gcall? g) (call-initialize-ctrl-altering! cfg g))
    (when (or start-new (stmt-starts-bb? g prev))
      (set! b (create-bb! cfg (pvector-empty) b))
      (set! start-new #f)
      (set! prev #f))
    (bb-append-stmt! cfg b g)
    (when (stmt-ends-bb? cfg g) (set! start-new #t))
    (set! prev g)))

;; ------------------------------------------------------------
;; cleanup_dead_labels
;; ------------------------------------------------------------

(define (cleanup-dead-labels! cfg)
  (define main-label (make-hasheq))   ; bb -> label
  (define used (make-hasheq))         ; bb -> #t
  (for-each-bb cfg
    (lambda (b)
      (let loop ([i 0])
        (when (and (< i (pvector-length (bb-stmts b))) (glabel? (pvector-ref (bb-stmts b) i)))
          (define l (glabel-label (pvector-ref (bb-stmts b) i)))
          (cond
            [(not (hash-ref main-label b #f)) (hash-set! main-label b l) (loop (add1 i))]
            [(and (not (decl-artificial l)) (decl-artificial (hash-ref main-label b)))
             (hash-set! main-label b l)]
            [else (loop (add1 i))])))))
  (define (main-block-label l)
    (define b (label-to-block cfg l))
    (unless (hash-ref main-label b #f) (hash-set! main-label b l))
    (hash-set! used b #t)
    (hash-ref main-label b))
  (for-each-bb cfg
    (lambda (b)
      (define g (bb-last-stmt b))
      (cond
        [(gcond? g)
         (when (gcond-true-label g) (set-gcond-true-label! g (main-block-label (gcond-true-label g))))
         (when (gcond-false-label g) (set-gcond-false-label! g (main-block-label (gcond-false-label g))))]
        [(gswitch? g)
         (for ([c (in-pvector (pvector-cons-left (gswitch-cases g) (gswitch-default-case g)))])
           (set-tree-operand! c 2 (main-block-label (case-label c))))]
        [(ggoto? g) (set-ggoto-dest! g (main-block-label (ggoto-dest g)))]
        [else (void)])))
  (for-each-bb cfg
    (lambda (b)
      (define keep (and (hash-ref used b #f) (hash-ref main-label b #f)))
      (when (hash-ref main-label b #f)
        (let loop ([i 0])
          (when (and (< i (pvector-length (bb-stmts b))) (glabel? (pvector-ref (bb-stmts b) i)))
            (define l (glabel-label (pvector-ref (bb-stmts b) i)))
            (cond
              [(or (eq? l keep) (not (decl-artificial l)) (label-nonlocal? l) (label-forced? l)) (loop (add1 i))]
              [else (bb-remove-stmt-at! b i) (loop i)])))))))

;; ------------------------------------------------------------
;; group_case_labels
;; ------------------------------------------------------------

;; group_case_labels_stmt: our gswitch keeps the default separately, so the
;; index i of GCC's label vector is (sub1 i) in `cases`.
(define (group-case-labels-stmt! cfg g)
  (define default-bb (label-to-block cfg (case-label (gswitch-default-case g))))
  (define old (gswitch-cases g))
  (define old-size (pvector-length old))
  (define new
    (let loop ([i 0] [acc (pvector-empty)])
      (cond
        [(>= i old-size) acc]
        [else
         (define base (pvector-ref old i))
         (define base-bb (label-to-block cfg (case-label base)))
         (cond
           [(or (not base-bb) (eq? base-bb default-bb)) (loop (add1 i) acc)]
           [else
            (define base-high (or (case-high base) (case-low base)))
            (define next
              (let merge ([j (add1 i)] [high base-high])
                (cond
                  [(>= j old-size) j]
                  [else
                   (define m (pvector-ref old j))
                   (cond
                     [(and (eq? (label-to-block cfg (case-label m)) base-bb)
                           (= (integer-cst-value (case-low m)) (add1 (integer-cst-value high))))
                      (define h (or (case-high m) (case-low m)))
                      (set-tree-operand! base 1 h)
                      (merge (add1 j) h)]
                     [else j])])))
            (loop next (pvector-cons-right acc base))])])))
  (when (< (pvector-length new) old-size) (set-gswitch-cases! g new))
  (< (pvector-length new) old-size))

(define (group-case-labels! cfg)
  (for-each-bb cfg (lambda (b) (define g (bb-last-stmt b)) (when (gswitch? g) (group-case-labels-stmt! cfg g)))))

;; ------------------------------------------------------------
;; make_edges
;; ------------------------------------------------------------

(define (make-edges! cfg)
  (make-edge! (gcfg-entry cfg) (block-at cfg 2) EDGE-FALLTHRU)
  (for-each-bb cfg (lambda (b) (make-edges-bb! cfg b))))

(define (make-edges-bb! cfg b)
  (define last (bb-last-stmt b))
  (define fallthru
    (cond
      [(not last) #f]
      [(ggoto? last)                       ; make_goto_expr_edges (simple goto)
       (define e (make-edge! b (label-to-block cfg (ggoto-dest last)) EDGE-FALLTHRU))
       (set-edge-goto-locus! e (gimple-location last))
       (bb-remove-stmt! b last)
       #f]
      [(greturn? last)
       (define e (make-edge! b (gcfg-exit cfg) 0))
       (set-edge-goto-locus! e (gimple-location last))
       #f]
      [(gcond? last) (make-cond-expr-edges! cfg b last) #f]
      [(gswitch? last)
       (for ([c (in-pvector (pvector-cons-left (gswitch-cases last) (gswitch-default-case last)))])
         (make-edge! b (label-to-block cfg (case-label c)) 0))
       #f]
      [(gcall? last) (not (gimple-call-noreturn-p last))]
      [else #t]))
  (when fallthru (make-edge! b (bb-next b) EDGE-FALLTHRU)))

(define (stmt-location g) (and g (gimple-location g)))

;; make_cond_expr_edges
(define (make-cond-expr-edges! cfg b g)
  (define then-bb (label-to-block cfg (gcond-true-label g)))
  (define else-bb (label-to-block cfg (gcond-false-label g)))
  (define e1 (make-edge! b then-bb EDGE-TRUE-VALUE))
  (set-edge-goto-locus! e1 (stmt-location (bb-first-stmt then-bb)))
  (define e2 (make-edge! b else-bb EDGE-FALSE-VALUE))
  (when e2 (set-edge-goto-locus! e2 (stmt-location (bb-first-stmt else-bb))))
  (set-gcond-true-label! g #f)
  (set-gcond-false-label! g #f))

;; ------------------------------------------------------------
;; assign_discriminators (trunk version, 385d9937f0e)
;; ------------------------------------------------------------

(define (assign-discriminators! cfg)
  (define map (make-hash))   ; line -> (mcons bb-id discrim)
  (define bb-id 0)
  ;; assign_discriminator: LOCATION_LINE of a location without a locus is 0
  (define (assign loc)
    (define line (let ([s (gloc-spot loc)]) (if s (cadr s) 0)))
    (define e (hash-ref map line #f))
    (cond
      [(not e) (hash-set! map line (mcons bb-id 0)) loc]
      [else
       (unless (= (mcar e) bb-id)
         (set-mcar! e bb-id)
         (set-mcdr! e (add1 (mcdr e))))
       (if (zero? (mcdr e)) loc (location-with-discriminator loc (mcdr e)))]))
  (for-each-bb cfg
    (lambda (b)
      (define prev-loc #f)
      (define prev-replacement #f)
      (for ([g (in-pvector (bb-stmts b))])
        (define loc (gimple-location g))
        (when loc
          (cond
            [(equal? loc prev-loc) (gimple-set-location! g prev-replacement)]
            [else
             (set! prev-loc loc)
             (set! prev-replacement (assign loc))
             (gimple-set-location! g prev-replacement)])
          (when (gcall? g)
            (set! prev-loc #f)
            (set! bb-id (add1 bb-id)))))
      (unless (single-succ-p b) (set! bb-id (add1 bb-id)))
      (for ([e (in-pvector (bb-succs b))])
        (when (edge-goto-locus e) (set-edge-goto-locus! e (assign (edge-goto-locus e))))
        (set! bb-id (add1 bb-id)))
      (set! bb-id (add1 bb-id)))))

;; ------------------------------------------------------------
;; cleanup_tree_cfg  (tree-cfgcleanup.cc, -O0, no loops, no PHIs)
;; ------------------------------------------------------------

(define (altered! cfg b) (set-gcfg-altered! cfg (intbits-set (gcfg-altered cfg) (bb-index b))))

;; remove_bb + delete_basic_block
(define (delete-basic-block! cfg b)
  (dump! cfg "Removing basic block ~a" (bb-index b))
  (set-bb-stmts! b (pvector-empty))
  (let loop () (unless (pvector-empty? (bb-preds b)) (remove-edge! (pvector-first (bb-preds b))) (loop)))
  (let loop () (unless (pvector-empty? (bb-succs b)) (remove-edge! (pvector-first (bb-succs b))) (loop)))
  (expunge-block! cfg b))

;; ---- cleanup_control_flow_bb ----------------------------------------

;; gimple_simplify on a GIMPLE_COND, constant-operand subset: returns the
;; folded INTEGER_CST or #f
(define (cond-constant-value g)
  (define l (gcond-lhs g)) (define r (gcond-rhs g))
  (and (eq? (tree-code l) 'integer_cst) (eq? (tree-code r) 'integer_cst)
       (let ([a (integer-cst-value l)] [b (integer-cst-value r)])
         (define v
           (case (gcond-code g)
             [(eq_expr) (= a b)] [(ne_expr) (not (= a b))]
             [(lt_expr) (< a b)] [(le_expr) (<= a b)] [(gt_expr) (> a b)] [(ge_expr) (>= a b)]
             [else 'unknown]))
         (and (boolean? v) (build-int-cst (boolean-type) (if v 1 0))))))

;; find_case_label_for_value: linear scan over the sorted cases, default otherwise
(define (find-case-label-for-value g v)
  (define n (integer-cst-value v))
  (or (for/first ([c (in-pvector (gswitch-cases g))]
                  #:when (let ([lo (integer-cst-value (case-low c))]
                               [hi (if (case-high c) (integer-cst-value (case-high c)) (integer-cst-value (case-low c)))])
                           (<= lo n hi)))
        c)
      (gswitch-default-case g)))

;; find_taken_edge
(define (find-taken-edge cfg b val)
  (define g (bb-last-stmt b))
  (cond
    [(gcond? g)
     (define v (or val (cond-constant-value g)))
     (cond
       [(not (and v (eq? (tree-code v) 'integer_cst))) #f]
       [else
        (define-values (t f) (extract-true-false-edges b))
        (if (integer-zerop v) f t)])]
    [(gswitch? g)
     (define taken
       (cond
         [(pvector-empty? (gswitch-cases g)) (gswitch-default-case g)]
         [else
          (define v (or val (gswitch-index g)))
          (and (eq? (tree-code v) 'integer_cst) (find-case-label-for-value g v))]))
     (and taken (find-edge b (label-to-block cfg (case-label taken))))]
    [else (and (single-succ-p b) (single-succ-edge b))]))

;; convert_single_case_switch
(define (convert-single-case-switch! cfg b g)
  (cond
    [(not (= 1 (pvector-length (gswitch-cases g)))) #f]
    [else
     (define index (gswitch-index g))
     (define c (pvector-first (gswitch-cases g)))
     (when (case-high c)
       (error 'convert-single-case-switch "range case needs generate_range_test (not ported)"))
     (define default-bb (label-to-block cfg (case-label (gswitch-default-case g))))
     (define case-bb (label-to-block cfg (case-label c)))
     (define cnd (gimple-build-cond 'eq_expr index (fold-convert (tree-type index) (case-low c)) #f #f))
     (gimple-set-location! cnd (gimple-location g))
     (set-bb-stmts! b (pvector-set (bb-stmts b) (sub1 (pvector-length (bb-stmts b))) cnd))
     (define ce (find-edge b case-bb))
     (define de (find-edge b default-bb))
     (set-edge-flags! ce (bitwise-ior (edge-flags ce) EDGE-TRUE-VALUE))
     (set-edge-flags! de (bitwise-ior (edge-flags de) EDGE-FALSE-VALUE))
     #t]))

;; canonicalize_bool_cond
(define (canonicalize-bool-cond! cfg g b)
  (define t (tree-type (gcond-lhs g)))
  (define code (gcond-code g))
  (cond
    [(not (memq code '(eq_expr ne_expr))) #f]
    [(not (or (boolean-type-p t) (and (integral-type-p t) (eqv? (type-precision t) 1)))) #f]
    [(and (eq? code 'eq_expr) (not (integer-zerop (gcond-rhs g)))) #f]
    [(and (eq? code 'ne_expr) (not (integer-onep (gcond-rhs g)))) #f]
    [else
     (define n (gimple-build-cond 'ne_expr (gcond-lhs g) (build-zero-cst t) #f #f))
     (gimple-set-location! n (gimple-location g))
     (set-bb-stmts! b (pvector-set (bb-stmts b) (sub1 (pvector-length (bb-stmts b))) n))
     (for ([e (in-pvector (bb-succs b))])
       (set-edge-flags! e (bitwise-xor (edge-flags e) (bitwise-ior EDGE-TRUE-VALUE EDGE-FALSE-VALUE))))
     ;; print_gimple_expr (TDF_RHS_ONLY) of a cond: "lhs op rhs"
     (dump! cfg "  Swapped '~a'" (cond->string n))
     #t]))

(define cond->string-hook (make-parameter (lambda (g) "<cond>")))
(define (cond->string g) ((cond->string-hook) g))

;; cleanup_control_expr_graph
(define (cleanup-control-expr-graph! cfg b)
  (define retval #f)
  (define taken
    (cond
      [(not (single-succ-p b))
       (when (gswitch? (bb-last-stmt b)) (convert-single-case-switch! cfg b (bb-last-stmt b)))
       (when (gcond? (bb-last-stmt b)) (canonicalize-bool-cond! cfg (bb-last-stmt b) b))
       (define g (bb-last-stmt b))
       (define val
         (cond
           [(gcond? g) (cond-constant-value g)]
           [(gswitch? g) (gswitch-index g)]
           [else #f]))
       (define te (find-taken-edge cfg b val))
       (when te
         (let loop ([i 0])
           (when (< i (pvector-length (bb-succs b)))
             (define e (pvector-ref (bb-succs b) i))
             (cond
               [(eq? e te) (loop (add1 i))]
               [else (remove-edge! e) (set! retval #t) (loop i)]))))
       te]
      [else (single-succ-edge b)]))
  (cond
    [(not taken) #f]
    [else
     (altered! cfg b)
     (bb-remove-stmt! b (bb-last-stmt b))
     (set-edge-flags! taken (bitwise-ior EDGE-FALLTHRU
                                         (bitwise-and (edge-flags taken)
                                                      (bitwise-not (bitwise-ior EDGE-TRUE-VALUE EDGE-FALSE-VALUE)))))
     retval]))

;; cleanup_control_flow_bb
(define (cleanup-control-flow-bb! cfg b)
  (define g (bb-last-stmt b))
  (cond
    [(not g) #f]
    [(or (gcond? g) (gswitch? g)) (cleanup-control-expr-graph! cfg b)]
    [(and (gcall? g) (gimple-call-noreturn-p g))
     (define ft (find-fallthru-edge b))
     (when ft (remove-edge! ft))
     (hash-set! (gcfg-ctrl-altering cfg) g #t)
     (and ft #t)]
    [else #f]))

;; ---- cleanup_control_flow_pre -------------------------------------

(define (cleanup-control-flow-pre! cfg)
  (define retval #f)
  (define visited (make-hasheq))
  ;; stack of (mcons edges index)
  (define stack (list (mcons (bb-succs (gcfg-entry cfg)) 0)))
  (let loop ()
    (unless (null? stack)
      (define top (car stack))
      (define e (pvector-ref (mcar top) (mcdr top)))
      (define dest (edge-dest e))
      (cond
        [(and (not (eq? dest (gcfg-exit cfg))) (not (hash-ref visited dest #f)))
         (hash-set! visited dest #t)
         (set! retval (or (cleanup-control-flow-bb! cfg dest) retval))
         (unless (pvector-empty? (bb-succs dest))
           (set! stack (cons (mcons (bb-succs dest) 0) stack)))]
        [(< (add1 (mcdr top)) (pvector-length (mcar top))) (set-mcdr! top (add1 (mcdr top)))]
        [else (set! stack (cdr stack))])
      (loop)))
  ;; remove all unreachable blocks, by index
  (for ([i (in-range 2 (pvector-length (gcfg-blocks cfg)))])
    (define b (block-at cfg i))
    (when (and b (not (hash-ref visited b #f)))
      (delete-basic-block! cfg b)
      (set! retval #t)))
  retval)

;; ---- merging --------------------------------------------------------

;; gimple_can_merge_blocks_p (== want_merge_blocks_p here: counts are uninitialized)
(define (can-merge-blocks? cfg a b)
  (and (single-succ-p a)
       (not (edge-flag? (single-succ-edge a) EDGE-COMPLEX))
       (eq? (single-succ a) b)
       (single-pred-p b)
       (not (eq? a (gcfg-entry cfg)))
       (not (eq? b (gcfg-exit cfg)))
       (let ([last (bb-last-stmt a)]) (not (and last (stmt-ends-bb? cfg last))))
       ;; labels at the beginning of B: at -O0 any user label blocks the merge
       (for/and ([g (in-pvector (bb-stmts b))] #:break (not (glabel? g)))
         (decl-artificial (glabel-label g)))
       ;; -O0: don't merge if we'd lose goto_locus
       (let ([gl (edge-goto-locus (single-succ-edge a))])
         (or (not gl)
             (let ([prev (bb-last-stmt a)] [next (bb-first-non-label-stmt b)])
               (or (and prev (equal? (gimple-location prev) gl))
                   (and next (equal? (gimple-location next) gl))))))))

;; gimple_merge_blocks + cfghooks merge_blocks
(define (merge-blocks! cfg a b)
  (dump! cfg "Merging blocks ~a and ~a" (bb-index a) (bb-index b))
  (unlink-block! b)
  (link-block! b a)
  (define kept
    (for/fold ([acc (pvector-empty)]) ([g (in-pvector (bb-stmts b))])
      (cond
        [(glabel? g)
         (when (label-forced? (glabel-label g))
           (bb-insert-stmt-at! cfg a 0 g))
         acc]
        [else (note-stmt-bb! cfg g a) (pvector-cons-right acc g)])))
  (set-bb-stmts! a (pvector-append (bb-stmts a) kept))
  (set-bb-stmts! b (pvector-empty))
  (altered! cfg a)
  ;; cfghooks.cc merge_blocks: A takes over B's successor edges
  (let loop () (unless (pvector-empty? (bb-succs a)) (remove-edge! (pvector-first (bb-succs a))) (loop)))
  (for ([e (in-pvector (bb-succs b))]) (set-edge-src! e a))
  (set-bb-succs! a (bb-succs b))
  (set-bb-preds! b (pvector-empty))
  (set-bb-succs! b (pvector-empty))
  (expunge-block! cfg b))

;; ---- edge redirection ------------------------------------------------

;; gimple_block_label
(define (gimple-block-label! cfg b)
  (define n (bb-leading-label-count b))
  (define found
    (for/first ([i (in-range n)]
                #:unless (label-nonlocal? (glabel-label (pvector-ref (bb-stmts b) i))))
      i))
  (cond
    [found
     (define g (pvector-ref (bb-stmts b) found))
     (unless (zero? found)
       (bb-remove-stmt-at! b found)
       (bb-insert-stmt-at! cfg b 0 g))
     (glabel-label g)]
    [else
     (define l (create-artificial-label))
     (bb-insert-stmt-at! cfg b 0 (gimple-build-label l))
     l]))

;; gimple_try_redirect_by_replacing_jump
(define (try-redirect-by-replacing-jump! cfg e target)
  (define src (edge-src e))
  (cond
    [(not (= 2 (pvector-length (bb-succs src)))) #f]
    [(not (eq? target (edge-dest (let ([e0 (pvector-ref (bb-succs src) 0)]) (if (eq? e0 e) (pvector-ref (bb-succs src) 1) e0))))) #f]
    [else
     (define g (bb-last-stmt src))
     (cond
       [(or (gcond? g) (gswitch? g))
        (bb-remove-stmt! src g)
        (define r (redirect-edge-succ-nodup! e target))
        (set-edge-flags! r EDGE-FALLTHRU)
        r]
       [else #f])]))

;; gimple_redirect_edge_and_branch (normal edges only)
(define (redirect-edge-and-branch! cfg e dest)
  (cond
    [(eq? (edge-dest e) dest) #f]
    [(and (not (eq? (edge-src e) (gcfg-entry cfg))) (try-redirect-by-replacing-jump! cfg e dest)) => values]
    [else
     (define src (edge-src e))
     (define g (bb-last-stmt src))
     (cond
       [(gswitch? g)
        (define label (gimple-block-label! cfg dest))
        (for ([c (in-pvector (pvector-cons-left (gswitch-cases g) (gswitch-default-case g)))])
          (when (eq? (label-to-block cfg (case-label c)) (edge-dest e))
            (set-tree-operand! c 2 label)))
        (set-gcfg-touched-switch! cfg (intbits-set (gcfg-touched-switch cfg) (bb-index src)))]
       [(ggoto? g) (error 'redirect-edge-and-branch "simple gotos were removed by make_edges")]
       [else (void)])
     (redirect-edge-succ-nodup! e dest)]))

;; ---- forwarder blocks -----------------------------------------------

;; maybe_remove_forwarder_block (no PHIs, no loops, no abnormal edges)
(define (maybe-remove-forwarder-block! cfg b)
  (cond
    [(not (single-succ-p b)) #f]
    [(eq? (single-succ b) (gcfg-exit cfg)) #f]
    [(eq? (single-succ b) b) #f]
    [(edge-flag? (single-succ-edge b) EDGE-ABNORMAL) #f]
    [else
     (define locus (edge-goto-locus (single-succ-edge b)))
     (define dest (single-succ b))
     (cond
       [(for/or ([e (in-pvector (bb-preds b))])
          (or (eq? (edge-src e) (gcfg-entry cfg))
              (edge-flag? e EDGE-EH)
              ;; -O0: a differing goto_locus on any incoming edge keeps the block
              (and (or (loc-has-location? (edge-goto-locus e)) (loc-has-location? locus))
                   (not (equal? (edge-goto-locus e) locus)))))
        #f]
       ;; trunk: leave single-pred/single-succ chains to block merging
       [(and (single-pred-p b) (single-succ-p (single-pred b))) #f]
       [(let ([f (bb-first-stmt dest)]) (and (glabel? f) (label-nonlocal? (glabel-label f)))) #f]
       [(not (for/and ([g (in-pvector (bb-stmts b))])
               (and (glabel? g)
                    (not (label-nonlocal? (glabel-label g)))
                    (not (and (or (gimple-has-location? g) (loc-has-location? locus))
                              (not (equal? (gimple-location g) locus)))))))
        #f]
       [else
        ;; redirect all predecessors to DEST
        (let loop ()
          (unless (pvector-empty? (bb-preds b))
            (define e (pvector-first (bb-preds b)))
            (altered! cfg (edge-src e))
            (redirect-edge-and-branch! cfg e dest)
            (loop)))
        ;; move user / forced / nonlocal labels to the start of DEST
        (define moved
          (for/pvector ([g (in-pvector (bb-stmts b))]
                        #:when (let ([l (glabel-label g)])
                                 (or (label-nonlocal? l) (label-forced? l) (not (decl-artificial l)))))
            g))
        (for ([g (in-pvector moved)]) (note-stmt-bb! cfg g dest))
        (set-bb-stmts! dest (pvector-append moved (bb-stmts dest)))
        (set-bb-stmts! b (pvector-filter-not (lambda (g) (pvector-index-of moved g)) (bb-stmts b)))
        (altered! cfg dest)
        (delete-basic-block! cfg b)
        #t])]))

;; cleanup_tree_cfg_bb
(define (cleanup-tree-cfg-bb! cfg b)
  (cond
    [(maybe-remove-forwarder-block! cfg b) #t]
    [(and (single-pred-p b) (can-merge-blocks? cfg (single-pred b) b))
     (altered! cfg (single-pred b))
     #f]
    [(and (single-succ-p b) (can-merge-blocks? cfg b (single-succ b)))
     (merge-blocks! cfg b (single-succ b))
     #t]
    [else #f]))

;; compact_blocks
(define (compact-blocks! cfg)
  (define bbs (cfg-bbs cfg))
  (for ([b (in-pvector bbs)] [i (in-naturals 2)]) (set-bb-index! b i))
  (set-gcfg-blocks! cfg (pvector-append (pvector (gcfg-entry cfg) (gcfg-exit cfg)) bbs)))

;; cleanup_tree_cfg_noloop
(define (cleanup-tree-cfg! cfg)
  (set-gcfg-altered! cfg intbits-empty)
  (define changed (cleanup-control-flow-pre! cfg))
  (define n (pvector-length (gcfg-blocks cfg)))
  (for ([i (in-range 2 n)])
    (define b (block-at cfg i))
    (when b (set! changed (or (cleanup-tree-cfg-bb! cfg b) changed))))
  (let loop ()
    (unless (intbits-empty? (gcfg-altered cfg))
      (define i (intbits-first (gcfg-altered cfg)))
      (set-gcfg-altered! cfg (intbits-clear (gcfg-altered cfg) i))
      (when (>= i 2)
        (define b (block-at cfg i))
        (when b
          (set! changed (or (cleanup-control-flow-bb! cfg b) changed))
          (set! changed (or (cleanup-tree-cfg-bb! cfg b) changed))))
      (loop)))
  ;; end_recording_case_labels
  (for ([i (in-list (intbits->list (gcfg-touched-switch cfg)))])
    (define b (and (< i (pvector-length (gcfg-blocks cfg))) (block-at cfg i)))
    (when (and b (gswitch? (bb-last-stmt b))) (group-case-labels-stmt! cfg (bb-last-stmt b))))
  (compact-blocks! cfg)
  changed)

;; ------------------------------------------------------------
;; Entry point: execute_build_cfg
;; ------------------------------------------------------------

(define (build-cfg fndecl body #:cond->string [c->s #f])
  (define entry (bb 0 (pvector-empty) (pvector-empty) (pvector-empty) #f #f))
  (define exit (bb 1 (pvector-empty) (pvector-empty) (pvector-empty) #f #f))
  (set-bb-next! entry exit) (set-bb-prev! exit entry)
  (define cfg (gcfg entry exit (pvector entry exit) 0 (make-hasheq) (make-hasheq) (pvector-empty)
                    intbits-empty intbits-empty))
  (parameterize ([current-function-decl fndecl]
                 [cond->string-hook (or c->s (cond->string-hook))])
    (make-blocks! cfg body)
    (when (eq? (bb-next entry) exit) (create-bb! cfg (pvector-empty) entry))
    (cleanup-dead-labels! cfg)
    (group-case-labels! cfg)
    (make-edges! cfg)
    (assign-discriminators! cfg)
    (cleanup-dead-labels! cfg)
    (cleanup-tree-cfg! cfg))
  cfg)
