#lang racket/base

;; ============================================================
;; Tests: normalize-try-exits (Kappa-recovery prep)
;; ============================================================
;;
;; These tests exercise the pass in isolation on hand-built Cfgs —
;; no JVM frontend involvement.  Fixtures are shaped to trigger the
;; zero-exit / single-exit / multi-exit / no-table code paths.

(require rackunit
         racket/match
         "normalize-try-exits.rkt"
         "../../cfg/utils/graph-ops.rkt"
         "../../../kernel/ir/cfg/types.rkt"
         "../../../kernel/data/data.rkt")

(module+ test
  ;; ---------- Helpers ----------
  ;; N fresh BlockIds 0..n-1.  Edges are implied by terminators.
  (define (alloc-bids n)
    (for/pvector ([i (in-range n)]) (BlockId i)))

  (define (mk-cfg-block bid term #:insns [insns (pvector-empty)]
                                 #:phis [phis (pvector-empty)])
    (CfgBlock bid phis insns term (hash)))

  (define (mk-cfg blocks entry var-cnt info)
    (define blk-map
      (for/fold ([m idmap-empty])
                ([b (in-list blocks)])
        (idmap-set m (CfgBlock-id b) b)))
    (Cfg blk-map entry #f (length blocks) var-cnt 0 info))

  (define (mk-info #:block-order [order #f] #:table [table #f])
    (let* ([m (hash)]
           [m (if order (hash-set m 'java/block-order order) m)]
           [m (if table (hash-set m 'java/exception-table table) m)])
      m))

  ;; ---------- Test 1: no exception-table → identity ----------
  (test-case "normalize-try-exits is identity when no exception-table"
    (define bids (alloc-bids 1))
    (define b0 (pvector-ref bids 0))
    (define blocks (list (mk-cfg-block b0 (Term:ret (pvector-empty)))))
    (define cfg
      (mk-cfg blocks b0 0
              (mk-info #:block-order bids)))
    (define cfg* (normalize-try-exits cfg))
    (check-eq? cfg* cfg "no table ⇒ same object returned"))

  ;; ---------- Test 2: zero-exit window (terminal try) ----------
  (test-case "zero-exit try window leaves the CFG unchanged"
    ;; One try block ending in Term:ret; handler unreachable but
    ;; present.  No edges leave the window, so the pass is a no-op.
    (define bids (alloc-bids 2))
    (define try-bid     (pvector-ref bids 0))
    (define handler-bid (pvector-ref bids 1))
    (define blocks
      (list (mk-cfg-block try-bid
                          (Term:ret (pvector-empty))
                          #:insns (pvector-empty))
            (mk-cfg-block handler-bid
                          (Term:ret (pvector-empty))
                          #:insns (pvector-empty))))
    (define table (pvector (list try-bid handler-bid handler-bid
                                 "java/lang/Exception")))
    (define cfg
      (mk-cfg blocks try-bid 10
              (mk-info #:block-order bids #:table table)))
    (define cfg* (normalize-try-exits cfg))
    (check-equal? (cfg-block-count cfg*)
                  (cfg-block-count cfg)
                  "zero-exit ⇒ no new blocks")
    (check-equal? (Cfg-var-cnt cfg*) 10 "no fresh VarIds allocated"))

  ;; ---------- Test 3: single-exit window → no-op ----------
  (test-case "single-exit try window leaves the CFG unchanged"
    ;; try-block has Term:jump to a single post-try block; handler
    ;; separate.  Since targets has cardinality 1, pass is a no-op.
    (define bids (alloc-bids 3))
    (define try-bid     (pvector-ref bids 0))
    (define handler-bid (pvector-ref bids 1))
    (define post-bid    (pvector-ref bids 2))
    (define blocks
      (list (mk-cfg-block try-bid (Term:jump post-bid))
            (mk-cfg-block handler-bid (Term:ret (pvector-empty)))
            (mk-cfg-block post-bid (Term:ret (pvector-empty)))))
    (define table
      (pvector (list try-bid handler-bid handler-bid
                     "java/lang/Exception")))
    (define cfg
      (mk-cfg blocks try-bid 5
              (mk-info #:block-order bids #:table table)))
    (define cfg* (normalize-try-exits cfg))
    ;; No new blocks, no new edges, no new VarIds.
    (check-equal? (cfg-block-count cfg*)
                  (cfg-block-count cfg))
    (check-equal? (cfg-edge-count cfg*)
                  (cfg-edge-count cfg))
    (check-equal? (Cfg-var-cnt cfg*) 5))

  ;; ---------- Test 4: multi-exit via two Term:jump blocks ----------
  (test-case "multi-exit (two Term:jump exits) inserts joiner + switch"
    ;; W = {A, B}.  A → T1 via Term:jump.  B → T2 via Term:jump.
    ;; Post-normalization: A's Term:jump goes to joiner, B's Term:jump
    ;; goes to joiner; joiner's Term:switch dispatches to {T1, T2}.
    (define bids (alloc-bids 5))
    (define A   (pvector-ref bids 0))
    (define B   (pvector-ref bids 1))
    (define H   (pvector-ref bids 2))
    (define T1  (pvector-ref bids 3))
    (define T2  (pvector-ref bids 4))
    (define blocks
      (list (mk-cfg-block A (Term:jump T1))
            (mk-cfg-block B (Term:jump T2))
            (mk-cfg-block H (Term:ret (pvector-empty)))
            (mk-cfg-block T1 (Term:ret (pvector-empty)))
            (mk-cfg-block T2 (Term:ret (pvector-empty)))))
    (define table
      (pvector (list A H H "java/lang/Exception")))
    ;; Window is [A, H) = {A, B} in block-order.
    (define cfg
      (mk-cfg blocks A 50
              (mk-info #:block-order bids #:table table)))
    (define cfg* (normalize-try-exits cfg))
    ;; One new joiner block added.
    (check-equal? (cfg-block-count cfg*) 6
                  "joiner block should be added (no forwarding needed)")
    ;; A's terminator is Term:jump to joiner; B's too.
    (define A-blk* (idmap-ref (Cfg-blocks cfg*) A #f))
    (define B-blk* (idmap-ref (Cfg-blocks cfg*) B #f))
    (check-pred Term:jump? (CfgBlock-terminator A-blk*))
    (check-pred Term:jump? (CfgBlock-terminator B-blk*))
    (define joiner (Term:jump-target (CfgBlock-terminator A-blk*)))
    (check-equal? (Term:jump-target (CfgBlock-terminator B-blk*)) joiner
                  "A and B now share the joiner as their Term:jump target")
    ;; A's insns gained a selector VfInsn (appended).
    (define A-insns (CfgBlock-insns A-blk*))
    (check-equal? (pvector-length A-insns) 1)
    (check-equal? (VfInsn-op (pvector-ref A-insns 0)) 'kappa-exit-sel)
    (define B-insns (CfgBlock-insns B-blk*))
    (check-equal? (pvector-length B-insns) 1)
    (check-equal? (VfInsn-op (pvector-ref B-insns 0)) 'kappa-exit-sel)
    ;; The two selector indexes must be distinct (0 and 1).
    (define a-sel (pvector-ref (VfInsn-inputs (pvector-ref A-insns 0)) 0))
    (define b-sel (pvector-ref (VfInsn-inputs (pvector-ref B-insns 0)) 0))
    (check-true (and (integer? a-sel) (integer? b-sel)))
    (check-not-equal? a-sel b-sel "distinct exit targets ⇒ distinct selector indexes")
    ;; Joiner block: Term:switch with 2 cases, phi merging sel-vars.
    (define joiner-blk (idmap-ref (Cfg-blocks cfg*) joiner #f))
    (check-not-false joiner-blk)
    (define joiner-term (CfgBlock-terminator joiner-blk))
    (check-pred Term:switch? joiner-term)
    (check-equal? (pvector-length (Term:switch-cases joiner-term)) 2)
    (define joiner-phis (CfgBlock-phis joiner-blk))
    (check-true (>= (pvector-length joiner-phis) 1)
                "joiner carries at least the selector merge phi")
    (define sel-phi (pvector-ref joiner-phis 0))
    (check-equal? (pvector-length (PhiInsn-sources sel-phi)) 2
                  "selector phi has one source per edge (2 edges here)")
    ;; Term:switch value equals the selector phi's output.
    (check-equal? (Term:switch-value joiner-term)
                  (PhiInsn-output sel-phi))
    ;; Block-order grew by exactly 1 (only joiner, no forwarding).
    (define order* (hash-ref (Cfg-info cfg*) 'java/block-order #f))
    (check-equal? (pvector-length order*) 6)
    (check-equal? (pvector-ref order* 5) joiner
                  "joiner is appended last in block-order")
    ;; var-cnt advanced by 2 selectors + 1 selector-phi = 3.
    (check-equal? (Cfg-var-cnt cfg*) 53)))
