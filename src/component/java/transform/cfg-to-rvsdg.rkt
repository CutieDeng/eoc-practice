#lang racket/base

;; ============================================================
;; Component: SSA CFG → RVSDG (M4, initial)
;; ============================================================
;;
;; Translate an SSA CFG (as produced by `jvm-cfg->ssa`) into an
;; RVSDG Lambda wrapping a single Region.
;;
;; Scope of this first pass:
;;
;;   - Handles CFGs whose only terminators are Term:jump, Term:ret,
;;     Term:throw, and Term:unreachable.  i.e. straight-line code
;;     with at most unconditional jumps between blocks.  This covers
;;     trivial static initialisers and any method that never
;;     branches.
;;
;;   - Rejects Term:cond / Term:switch.  Structural recovery for
;;     conditionals (Gamma) and loops (Theta) is explicitly deferred
;;     -- those need dominance-tree decomposition and are a later
;;     milestone.
;;
;; Input encoding:
;;
;;   - Method parameters are exposed via a synthetic `Simple 'param`
;;     node whose outputs correspond to VarId(0..local-count-1)
;;     after SSA renaming -- we recover those names from the initial
;;     rename stacks by inspecting each block's phi-less entry state.
;;     Concretely, after SSA the first `local-count` VarIds allocated
;;     during rename are the parameter names; we treat VarId(vc0 ..
;;     vc0+local-count-1) as the param outputs.  (This requires the
;;     SSA pass to always allocate param names first, which it does.)
;;
;;   - Each VfInsn becomes a `Simple` node whose op is the VfInsn's
;;     opcode symbol.  Literal inputs (ints, strings for LDC, etc.)
;;     that aren't VarIds are encoded as distinct `Simple
;;     '(const ,literal)` nodes.  The VfInsn's `info` hash (owner /
;;     name / desc for field/invoke) is preserved via the region's
;;     info under key 'node-insn-info.
;;
;;   - Return values are consumed by a synthetic `Simple 'return`
;;     node with inputs for each Term:ret value.  Throws map to the
;;     kernel `Throw` node.
;;
;; Output: Lambda wrapping a Region.
;; ============================================================

(require racket/match
         "../../../kernel/ir/cfg/types.rkt"
         "../../../kernel/ir/rvsdg/rvsdg.rkt"
         "../../../component/cfg/utils/graph-ops.rkt"
         "../../../component/rvsdg/utils/builder.rkt"
         (except-in "../../../kernel/data/data.rkt" integer-compare))

(provide cfg->rvsdg)

;; ============================================================
;; Entry
;; ============================================================

(define (cfg->rvsdg cfg)
  (define local-count (cfg-get-info cfg 'java/max-local 0))
  (define block-order (linearise-blocks cfg))

  (define region0 (region-empty))

  ;; 1. Allocate the parameter provider node.
  (define-values (region1 param-nid _param-ins param-outs)
    (region-add-node region0 (Simple 'param) 0 local-count))

  ;; 2. Seed the VarId → OutputId map with the parameter VarIds.
  ;;    After SSA, the rename pass allocates exactly `local-count`
  ;;    fresh VarIds before anything else -- VarId(vc0 .. vc0 +
  ;;    local-count - 1) where vc0 is the pre-rename var-cnt.  The
  ;;    CFG's current Cfg-var-cnt is *after* all renaming, so we
  ;;    can't read vc0 directly.  Instead, we derive the parameter
  ;;    names by inspecting the entry block's first reads / uses;
  ;;    a cleaner fix is for jvm-cfg->ssa to publish param names in
  ;;    the info map.  For now we require the SSA pass to record
  ;;    them there.
  (define param-names
    (or (cfg-get-info cfg 'java/ssa-param-names #f)
        (error 'cfg->rvsdg
               "ssa must publish 'java/ssa-param-names; run jvm-cfg->ssa first")))

  (define var->output-init
    (for/fold ([m (ordered-map-empty var-id-compare)])
              ([name (in-pvector param-names)]
               [oid (in-pvector param-outs)])
      (ordered-map-set m name oid)))

  ;; 3. Walk blocks in linear order, emitting Simple nodes.
  (define-values (region-final var->output-final return-payload)
    (for/fold ([r region1]
               [m var->output-init]
               [ret #f])
              ([bid (in-pvector block-order)])
      (translate-block cfg bid r m ret)))

  ;; 4. Attach return/throw node.
  (define region-done
    (cond
      [(not return-payload) region-final]
      [(eq? (car return-payload) 'ret)
       (install-return region-final var->output-final (cdr return-payload))]
      [(eq? (car return-payload) 'throw)
       (install-throw region-final var->output-final (cdr return-payload))]
      [else region-final]))

  (Lambda region-done))

;; ============================================================
;; Linearise blocks
;; ============================================================

;; Returns a pvector[BlockId] reachable via Term:jump chains from
;; the entry.  Errors on any Term:cond/Term:switch encountered.
(define (linearise-blocks cfg)
  (define entry (Cfg-entry cfg))
  (let loop ([bid entry] [acc (pvector-empty)] [seen (ordered-map-empty block-id-compare)])
    (cond
      [(ordered-map-ref seen bid #f)
       (error 'linearise-blocks "cycle detected at ~a; control-flow recovery not implemented" bid)]
      [else
       (define acc* (pvector-cons-right acc bid))
       (define seen* (ordered-map-set seen bid #t))
       (define blk (cfg-get-block cfg bid))
       (unless blk
         (error 'linearise-blocks "missing block ~a" bid))
       (unless (= (pvector-length (CfgBlock-phis blk)) 0)
         (error 'linearise-blocks
                "phi in block ~a; Gamma/Theta recovery not implemented" bid))
       (match (CfgBlock-terminator blk)
         [(Term:jump nxt) (loop nxt acc* seen*)]
         [(Term:ret _) acc*]
         [(Term:throw _) acc*]
         [(Term:unreachable) acc*]
         [(Term:cond _ _ _)
          (error 'linearise-blocks
                 "Term:cond in block ~a; Gamma recovery not implemented" bid)]
         [(Term:switch _ _ _)
          (error 'linearise-blocks
                 "Term:switch in block ~a; switch recovery not implemented" bid)]
         [#f acc*]
         [_ acc*])])))

;; ============================================================
;; Per-block translation
;; ============================================================

(define (translate-block cfg bid region var->out return-payload)
  (define blk (cfg-get-block cfg bid))
  (define-values (region* var->out*)
    (for/fold ([r region] [m var->out])
              ([insn (in-pvector (CfgBlock-insns blk))])
      (translate-vfinsn insn r m)))

  (define term (CfgBlock-terminator blk))
  (define payload*
    (match term
      [(Term:ret values) (cons 'ret values)]
      [(Term:throw ex)   (cons 'throw ex)]
      [_                  return-payload]))
  (values region* var->out* payload*))

;; ============================================================
;; Instruction → Simple node
;; ============================================================

(define (translate-vfinsn insn region var->out)
  (define op (VfInsn-op insn))
  (define inputs (VfInsn-inputs insn))
  (define outputs (VfInsn-outputs insn))
  (define n-in (pvector-length inputs))
  (define n-out (pvector-length outputs))

  ;; Convert each input: if it's a VarId, look it up; if it's a
  ;; literal, synthesise a `Simple (const ,lit)` node and hold its
  ;; single output.
  (define-values (region1 input-outputs)
    (for/fold ([r region] [acc (pvector-empty)])
              ([x (in-pvector inputs)])
      (cond
        [(VarId? x)
         (define oid (ordered-map-ref var->out x
                        (lambda () (error 'translate-vfinsn "undefined ~a for op ~a" x op))))
         (define oid* (if (procedure? oid) (oid) oid))
         (values r (pvector-cons-right acc oid*))]
        [else
         ;; literal: spawn a const node
         (define-values (r* _nid _ins outs)
           (region-add-node r (Simple (list 'const x)) 0 1))
         (values r* (pvector-cons-right acc (pvector-ref outs 0)))])))

  ;; Allocate the main op node.
  (define-values (region2 nid in-ids out-ids)
    (region-add-node region1 (Simple op) n-in n-out))

  ;; Wire each input position from its source OutputId.
  (define region3
    (for/fold ([r region2])
              ([src (in-pvector input-outputs)]
               [dst (in-pvector in-ids)])
      (define-values (r* _wid) (region-add-wire r src dst))
      r*))

  ;; Extend VarId→OutputId.
  (define var->out*
    (for/fold ([m var->out])
              ([vout (in-pvector outputs)]
               [oid (in-pvector out-ids)]
               #:when (VarId? vout))
      (ordered-map-set m vout oid)))

  (values region3 var->out*))

;; ============================================================
;; Terminator materialisation
;; ============================================================

(define (install-return region var->out ret-values)
  (define n (pvector-length ret-values))
  (define-values (region1 nid in-ids _outs)
    (region-add-node region (Simple 'return) n 0))
  (for/fold ([r region1])
            ([v (in-pvector ret-values)]
             [iid (in-pvector in-ids)])
    (define oid (ordered-map-ref var->out v
                   (lambda () (error 'install-return "undef ~a" v))))
    (define-values (r* _w) (region-add-wire r (if (procedure? oid) (oid) oid) iid))
    r*))

(define (install-throw region var->out exn)
  (define-values (region1 nid in-ids _outs)
    (region-add-node region (Throw #f) 1 0))
  (define oid (ordered-map-ref var->out exn
                  (lambda () (error 'install-throw "undef ~a" exn))))
  (define oid* (if (procedure? oid) (oid) oid))
  (define-values (r* _w) (region-add-wire region1 oid* (pvector-ref in-ids 0)))
  r*)
