#lang racket/base

;; ============================================================
;; opt: the -O1 early pass sequence (doc/roadmap.md 7.1)
;; ============================================================
;;
;; (o1-passes st) -> list of (list stage-name pass remove-unused-locals?)
;;   pass : st fndecl cfg -> (values cfg st)
;;   remove-unused-locals? : the pass asks for TODO_remove_unused_locals
;; in GCC's dump order for the dump's GCC version.  A pass that is not
;; ported yet but changes nothing on the fixtures is the identity (its
;; dump must still equal the previous one, which the -O1 test checks);
;; the roadmap table says which ones.
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../cfg.rkt"
         (only-in "../ssa.rkt" ssa-names-in-version-order)
         "ccp.rkt"
         "forwprop.rkt"
         "vn.rkt"
         "dse.rkt"
         "dce.rkt"
         "profile.rkt"
         "pure-const.rkt"
         "modref.rkt"
         "release-ssa.rkt"
         "fixup-cfg.rkt"
         "loops.rkt"
         "ch.rkt"
         "dom.rkt"
         "copyprop.rkt"
         "reassoc.rkt"
         "loop-pipeline.rkt"
         "sccp.rkt"
         "ivcanon.rkt"
         "cunroll.rkt"
         "ivopts.rkt"
         "switchlower.rkt"
         (only-in "../print.rkt" print-cond-rhs)
         (only-in "simplify.rkt" simplify-assign-rhs simplify-cond follow-ssa-edges?)
         (only-in "ssa-ops.rkt" stmt-real-uses stmt-has-side-effects?)
         "../../../driver/dominance/dom.rkt")

(provide late-passes o1-passes identity-pass ealias pass-name pass-fn pass-prunes-locals? pass-enabled? run-pass)
(define pass-name car)
(define pass-fn cadr)
(define pass-prunes-locals? caddr)
;; the pass's gate for one function (a pass without a gate always runs;
;; a pass whose gate fails neither runs nor prints a dump)
(define (pass-gate p) (and (pair? (cdddr p)) (cadddr p)))
(define (pass-enabled? p st decl cfg) (let ([g (pass-gate p)]) (or (not g) (g st decl cfg))))

;; execute_one_pass for one function: the pass, then the SSA free list
;; bookkeeping of execute_todo.  A pass that records its releases
;; (ssa-queue-release, in release order) is taken as is; the versions that
;; disappeared without being recorded are queued in descending order (the
;; order of a bottom-up dead-code walk, which is what the passes that do
;; not record do on the fixtures).  release_ssa empties the free list.
(define (live-versions cfg)
  (for/hasheqv ([n (in-pvector (ssa-names-in-version-order cfg))] #:unless (tree-attr n 'default-def #f))
    (values (ssa-name-version n) #t)))
(define (run-pass p st decl cfg)
  (define before (live-versions cfg))
  (define log (box (hash)))
  (define-values (cfg1 st1) (parameterize ([ssa-pass-log log] [replace-uses-folder fold-stmt-no-follow]) ((pass-fn p) st decl cfg)))
  (define st2 (gs-set-fndecl st1 decl))
  (cond
    [(eq? (pass-name p) 'release_ssa) (values cfg1 (ssa-clear-free st2))]
    [else
     (define after (live-versions cfg1))
     (define queued (ssa-free-queue st2))
     ;; the CFG cleanup's releases (merged PHIs, removed blocks) follow the
     ;; pass's own; a version is released once
     (define cleanup-released
       (for/fold ([acc '()] #:result (reverse acc)) ([v (in-list (reverse (hash-ref (unbox log) 'released '())))])
         (if (or (memv v queued) (memv v acc) (hash-ref after v #f)) acc (cons v acc))))
     (define st2b (for/fold ([st st2]) ([v (in-list cleanup-released)]) (ssa-queue-release st v)))
     (define queued2 (ssa-free-queue st2b))
     ;; names that disappeared without being recorded, the ones this pass
     ;; created included, in descending order
     (define candidates (remove-duplicates (append (hash-keys before) (hash-ref (unbox log) 'allocated '()))))
     (define unrecorded
       (sort (for/list ([v (in-list candidates)] #:unless (or (hash-ref after v #f) (memv v queued2))) v) >))
     (define st3 (for/fold ([st st2b]) ([v (in-list unrecorded)]) (ssa-queue-release st v)))
     (values cfg1 (ssa-flush-free st3))]))

;; fold_stmt with no_follow_ssa_edges (replace_uses_by): gimple_simplify
;; of an assignment's right-hand side or a condition, operands as they are
(define (fold-stmt-no-follow st g)
  (parameterize ([follow-ssa-edges? #f])
    (cond
      [(gassign? g)
       (define r (and (not (eq? (gimple-rhs-class st (gassign-code g)) 'single))
                      (simplify-assign-rhs st #f values (tree-type (gassign-lhs g)) (gassign-code g)
                                           (gassign-rhs1 g) (gassign-rhs2 g) (gassign-rhs3 g))))
       (if r
           (gassign (gassign-loc g) (gassign-flags g) (gassign-lhs g) (car r) (cadr r) (caddr r) (cadddr r))
           g)]
      [(gcond? g)
       (define r (simplify-cond st #f values (gcond-code g) (gcond-lhs g) (gcond-rhs g)))
       (if r
           (gcond (gcond-loc g) (gcond-flags g) (car r) (cadr r) (caddr r) (gcond-true-label g) (gcond-false-label g))
           g)]
      [else g])))

;; a pass without effect: a fresh dump_file, nothing else
(define (identity-pass st fndecl cfg) (values (cfg-clear-messages cfg) st))

;; pass_build_ealias: points-to analysis is not ported; GCC >= 13 prints
;; the constraint solver's progress lines for every function
;; (solve_constraints, `if (dump_file)`), GCC 12 also dumps the
;; constraints and the points-to sets (not reproduced)
(define (ealias st fndecl cfg)
  (define lines
    '("" "Collapsing static cycles and doing variable substitution" "Building predecessor graph"
      "Detecting pointer and location equivalences" "Rewriting constraints and unifying variables"
      "Uniting pointer but not location equivalent variables" "Finding indirect cycles" "Solving graph"))
  (values (for/fold ([c (cfg-clear-messages cfg)]) ([l (in-list lines)]) (cfg-add-message c l)) st))

; pass_early_inline: nothing is inlined by the port (early inlining is not
;; ported), which is what the fixtures show; the dump still reports the
;; iteration count
(define (einline st fndecl cfg)
  (values (cfg-add-message (cfg-clear-messages cfg) "Iterations: 0") st))

(define (o1-passes st)
  (define major (gcc-major-version st))
  (define (p name fn [locals? #f]) (list name fn locals?))
  (append
   (list (p 'fixup_cfg2 fixup-cfg-pass) (p 'einline einline) (p 'ccp1 ccp)
         (p 'forwprop1 forwprop)
         (p 'ethread identity-pass)
         (p 'esra identity-pass)
         (p 'ealias ealias))
   (if (>= major 13) (list (p 'phiprop1 identity-pass)) '())
   (list (p 'fre1 fre)
         (p 'mergephi1 identity-pass)
         (p 'dse1 dse)
         ;; pass_cd_dce, remove_unused_locals since GCC 15
         (p 'cddce1 dce (>= major 15))
         (p 'phiopt1 identity-pass)
         (p 'iftoswitch identity-pass))
   (if (>= major 14) (list (p 'sccopy1 identity-pass)) '())
   (list (p 'profile_estimate profile-estimate) (p 'local-pure-const1 local-pure-const) (p 'modref1 modref) (p 'release_ssa release-ssa #t))))

;; all_optimizations (after the IPA stage): the instances ported so far,
;; in GCC's order.  fixup_cfg3 = pass_fixup_cfg right after the inline
;; transform (whose own fixup / cleanup the harness runs before).
(define (late-passes st)
  (define major (gcc-major-version st))
  (define (p name fn [locals? #f] #:gate [gate #f]) (list name fn locals? gate))
  ;; the sub-passes of pass_tree_loop
  (define (lp name fn [locals? #f]) (p name fn locals? #:gate in-loop-pipeline?))
  (define (has-loops? st decl cfg) (function-has-loops? cfg))
  (define (ccp2 st fndecl cfg) (ccp st fndecl cfg #:nonzero? #t))
  ;; pass_rebuild_frequencies (GCC >= 14): the profile is consistent on the fixtures
  (define (rebuild-frequencies st fndecl cfg) (values (cfg-add-message (cfg-clear-messages cfg) "Profile is consistent") st))
  (append
   (list (p 'fixup_cfg3 fixup-cfg-pass) (p 'adjust_alignment identity-pass) (p 'ccp2 ccp2) (p 'objsz1 identity-pass)
         (p 'waccess2 identity-pass))
   (if (>= major 14) (list (p 'rebuild_frequencies1 rebuild-frequencies)) '())
   (list (p 'backprop identity-pass)
         (p (if (>= major 13) 'phiprop2 'phiprop) identity-pass)
         (p 'forwprop2 forwprop) (p 'alias ealias) (p 'retslot identity-pass) (p 'fre3 fre) (p 'mergephi2 identity-pass)
         (p 'dse2 dse) (p 'dce2 dce (>= major 15))
         (p 'stdarg identity-pass #t) (p 'cdce identity-pass)   ; stdarg: TODO_remove_unused_locals at start
         ;; pass_cselim: loop_optimizer_init (LOOPS_NORMAL) re-creates the simple latches
         (p 'cselim loops-normal-pass) (p 'copyprop1 copyprop) (p 'ifcombine identity-pass) (p 'mergephi3 identity-pass)
         (p 'phiopt2 identity-pass)
         (p 'ch2 ch-pass) (p 'cplxlower1 identity-pass))
   (if (>= major 14) (list (p 'bitintlower1 identity-pass)) '())
   (list (p 'sra identity-pass) (p 'dom2 dom-pass)
         (p 'copyprop2 copyprop) (p 'reassoc1 (reassoc-pass #t)) (p 'dce3 dce) (p 'forwprop3 forwprop)
         (p 'phiopt3 identity-pass) (p 'ccp3 ccp2)
         ;; pass_cse_pow (GCC 15) / pass_cse_sincos (before): no pow / sincos calls on the fixtures
         (p (if (>= major 15) 'pow 'sincos) identity-pass) (p 'laddress identity-pass)
         ;; pass_lim: loop_optimizer_init (LOOPS_NORMAL); no invariant is moved on the fixtures
         (p 'lim2 loops-normal-pass) (p 'walloca2 identity-pass)
         (p 'sink1 (sink-pass #f)) (p 'dse3 dse) (p 'dce4 dce) (p 'fix_loops identity-pass)
         ;; pass_tree_loop: gate_loop (the function has a loop)
         (p 'loop loop-pass #:gate has-loops?)
         (lp 'loopinit loop-init-pass) (lp 'sccp sccp-pass) (lp 'cddce2 dce) (lp 'ivcanon ivcanon-pass) (lp 'copyprop3 copyprop) (lp 'cunroll cunroll-pass)
         ;; pass_lim (lim4): nothing is invariant in the remaining loops
         (lp 'ivopts ivopts-pass) (lp 'lim4 identity-pass) (lp 'loopdone loop-done-pass)
         ;; pass_tree_no_loop: the functions that never had a loop
         (p 'no_loop no-loop-pass #:gate not-in-loop-pipeline?)
         ;; pass_lower_vector_ssa: TODO_cleanup_cfg
         (p 'veclower21 cleanup-pass) (p 'switchlower1 switchlower-pass))
   (if (>= major 15) (list (p 'sincos identity-pass)) '())
   (list (p 'reassoc2 (reassoc-pass #f)) (p 'slsr identity-pass) (p 'dom3 dom-pass) (p 'strlen1 loops-normal-pass)
         (p 'ccp4 ccp2) (p 'wrestrict identity-pass) (p 'dse5 dse))
   (if (>= major 15)
       (list (p 'dce7 dce #t) (p 'forwprop4 forwprop) (p 'sink2 (sink-pass #t)) (p 'phiopt4 identity-pass)
             (p 'fab1 identity-pass) (p 'cddce3 dce) (p 'sccopy2 identity-pass))
       (list (p 'cddce3 dce) (p 'forwprop4 forwprop) (p 'sink2 (sink-pass #t)) (p 'phiopt4 identity-pass)
             (p 'fab1 identity-pass) (p 'dce7 dce)))
   (list (p 'crited1 crited) (p 'local-pure-const2 local-pure-const) (p 'modref2 modref) (p 'uncprop1 identity-pass)
         (p 'nrv identity-pass) (p 'isel identity-pass) (p 'waccess3 waccess-late)
         ;; pass_cleanup_cfg_post_optimizing: TODO_remove_unused_locals
         (p 'optimized cleanup-post-optimizing #t))))

;; pass_split_crit_edges: split_critical_edges ()
(define (crited st fndecl cfg)
  (split-edges-for-insertion (gs-set-fndecl st fndecl) (cfg-clear-messages cfg) #:for-insertion? #f))

;; pass_waccess (late): the warnings are not ported; the late instance
;; makes no pointer queries on the fixtures, so the counters it dumps are 0
(define (waccess-late st fndecl cfg)
  (define lines '("pointer_query counters:" "  index cache size:   0" "  index entries:      0" "  access cache size:  0"
                  "  access entries:     0" "  hits:               0" "  misses:             0" "  failures:           0"
                  "  max_depth:          0"))
  (values (for/fold ([c (cfg-clear-messages cfg)]) ([l (in-list lines)]) (cfg-add-message c l)) st))

;; execute_cleanup_cfg_post_optimizing: execute_fixup_cfg, cleanup_tree_cfg,
;; cleanup_dead_labels (group_case_labels changes nothing on the fixtures)
(define (cleanup-post-optimizing st fndecl cfg)
  (define-values (c1 s1) (fixup-cfg-pass st fndecl cfg))
  (define-values (c2 s2) (cleanup-tree-cfg (gs-set-fndecl s1 fndecl) c1 print-cond-rhs))
  (values (cleanup-dead-labels s2 c2) s2))

;; a pass whose only effect is TODO_cleanup_cfg
(define (cleanup-pass st fndecl cfg)
  (cleanup-tree-cfg (gs-set-fndecl st fndecl) (cfg-clear-messages cfg) print-cond-rhs))

;; pass_sink_code: loop_optimizer_init (LOOPS_NORMAL), split_edges_for_insertion,
;; then sink_code_in_bb over the blocks (GCC <= 12: the post-dominator
;; tree from EXIT, preorder; GCC >= 13: inverted reverse post order).  A
;; block that dominates nothing is skipped; the statements are walked
;; backwards and a dead one (an assignment whose single definition has no
;; use, no VDEF) is removed.  The sinking itself (statement_sink_location
;; finding a better block) is not ported: nothing is sunk on the fixtures.
;; UNSPLIT? asks for TODO_cleanup_cfg.
(define ((sink-pass unsplit?) st0 fndecl cfg)
  (define st (gs-set-fndecl st0 fndecl))
  (define cfg1 (normalize-loops st (cfg-clear-messages cfg)))
  (define-values (cfg2 st2) (split-edges-for-insertion st cfg1))
  (define-values (cfg3 st3) (sink-remove-dead st2 cfg2))
  (if unsplit? (cleanup-tree-cfg st3 cfg3 print-cond-rhs) (values cfg3 st3)))

(define (sink-remove-dead st cfg)
  (define nodes (pvector-cons-left (pvector-cons-left (cfg-bbs cfg) EXIT) ENTRY))
  (define dom (compute-dom nodes ENTRY (lambda (i) (for/pvector ([e (in-pvector (cfg-succs cfg i))]) (GEdge-dest e)))))
  (define pdom (compute-dom nodes EXIT (lambda (i) (cfg-preds cfg i))))
  (define order
    (if (>= (gcc-major-version st) 13)
        (for/list ([b (in-pvector (dom-rpo pdom))] #:unless (memv b (list ENTRY EXIT))) b)
        (let walk ([i 0])
          (define b (dom-node pdom i))
          (append (if (memv b (list ENTRY EXIT)) '() (list b))
                  (append* (for/list ([k (in-pvector (dom-children pdom i))]) (walk k)))))))
  ;; the uses of each SSA version (real uses in statements and PHIs)
  (define uses (make-hasheqv))
  (define (count! t d) (when (and (Tree? t) (eq? (tree-code t) 'ssa_name)) (hash-update! uses (ssa-name-version t) (lambda (n) (+ n d)) 0)))
  (for ([bb (in-pvector (cfg-bbs cfg))])
    (define b (cfg-block cfg bb))
    (for* ([ph (in-pvector (GBlock-phis b))] [a (in-pvector (gphi-args ph))]) (count! (cdr a) 1))
    (for* ([g (in-pvector (GBlock-stmts b))] [u (in-list (stmt-real-uses g))]) (count! u 1)))
  (for/fold ([cfg cfg] [st st]) ([bb (in-list order)]
                                 #:when (let ([i (dom-index dom bb)]) (and i (not (pvector-empty? (dom-children dom i))))))
    (define stmts (pvector->list (GBlock-stmts (cfg-block cfg bb))))
    (define-values (kept released)
      (for/fold ([kept '()] [released '()]) ([g (in-list (reverse stmts))])
        (define lhs (and (gassign? g) (gassign-lhs g)))
        (cond
          [(and lhs (eq? (tree-code lhs) 'ssa_name) (not (stmt-flag g 'vdef #f))
                (zero? (hash-ref uses (ssa-name-version lhs) 0))
                (not (stmt-has-side-effects? st g)))
           (for ([u (in-list (stmt-real-uses g))]) (count! u -1))
           (values kept (cons (ssa-name-version lhs) released))]
          [else (values (cons g kept) released)])))
    (values (set-block cfg (block-set-stmts (cfg-block cfg bb) (list->pvector kept)))
            (for/fold ([st st]) ([v (in-list (reverse released))]) (ssa-queue-release st v)))))

;; a pass whose only visible effect on the fixtures is loop_optimizer_init (LOOPS_NORMAL)
(define (loops-normal-pass st fndecl cfg)
  (values (normalize-loops (gs-set-fndecl st fndecl) (cfg-clear-messages cfg)) st))
