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

(require "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
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
         (only-in "../print.rkt" print-cond-rhs))

(provide late-passes o1-passes identity-pass ealias pass-name pass-fn pass-prunes-locals? run-pass)
(define pass-name car)
(define pass-fn cadr)
(define pass-prunes-locals? caddr)

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
  (define-values (cfg1 st1) ((pass-fn p) st decl cfg))
  (define st2 (gs-set-fndecl st1 decl))
  (cond
    [(eq? (pass-name p) 'release_ssa) (values cfg1 (ssa-clear-free st2))]
    [else
     (define after (live-versions cfg1))
     (define queued (ssa-free-queue st2))
     (define unrecorded
       (sort (for/list ([v (in-hash-keys before)] #:unless (or (hash-ref after v #f) (memv v queued))) v) >))
     (define st3 (for/fold ([st st2]) ([v (in-list unrecorded)]) (ssa-queue-release st v)))
     (values cfg1 (ssa-flush-free st3))]))

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
  (define (p name fn [locals? #f]) (list name fn locals?))
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
         (p 'sink1 (sink-pass #f)) (p 'dse3 dse) (p 'dce4 dce) (p 'fix_loops identity-pass))))

;; pass_sink_code: loop_optimizer_init (LOOPS_NORMAL), split_edges_for_insertion,
;; the statement sinking (not ported: nothing is sunk on the fixtures);
;; UNSPLIT? asks for TODO_cleanup_cfg
(define ((sink-pass unsplit?) st0 fndecl cfg)
  (define st (gs-set-fndecl st0 fndecl))
  (define cfg1 (normalize-loops st (cfg-clear-messages cfg)))
  (define-values (cfg2 st2) (split-edges-for-insertion st cfg1))
  (if unsplit? (cleanup-tree-cfg st2 cfg2 print-cond-rhs) (values cfg2 st2)))

;; a pass whose only visible effect on the fixtures is loop_optimizer_init (LOOPS_NORMAL)
(define (loops-normal-pass st fndecl cfg)
  (values (normalize-loops (gs-set-fndecl st fndecl) (cfg-clear-messages cfg)) st))
