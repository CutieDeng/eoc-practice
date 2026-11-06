#lang racket/base

;; ============================================================
;; CFG -> RVSDG structurizer: VfInsn -> Simple nodes
;; ============================================================
;; Split out of structurize.rkt (roadmap Phase 3.1); see that file's
;; header for the algorithm.  The CFG is read through port.rkt.
;; ============================================================

(require racket/match
         racket/list
         "../../../kernel/ir/cfg/types.rkt"
         "../../../kernel/ir/rvsdg/rvsdg.rkt"
         "../utils/builder.rkt"
         "../port.rkt"
         "../../../kernel/data/data.rkt"
         "ctx.rkt")

(provide translate-insns translate-vfinsn translate-vfinsn/generic)


;; ============================================================
;; Instruction-list translation (shared by parent and sub-regions)
;; ============================================================

(define (translate-insns insns region var->out)
  (for/fold ([r region] [m var->out])
            ([insn (in-pvector insns)])
    (translate-vfinsn insn r m)))


(define (translate-vfinsn insn region var->out)
  (define op (VfInsn-op insn))
  (define inputs (VfInsn-inputs insn))
  (define outputs (VfInsn-outputs insn))
  (define n-in (pvector-length inputs))
  (define n-out (pvector-length outputs))

  ;; Special case: `'java/exception-ref` is a synthetic single-output
  ;; producer inserted by `jvm-method->cfg` at each handler block's
  ;; entry (its outputs are the stack slots holding the caught
  ;; exception reference).  Under Kappa lowering the handler runs
  ;; inside a sub-region whose `(+ n-ctx 1)`-th region-arg output IS
  ;; the exception reference, installed by translate-kappa-group via
  ;; `current-exn-out`.  Rather than allocate a fresh
  ;; `Simple 'java/exception-ref` node (dead) we rebind the insn's
  ;; output VarId to the handler-region's exn-ref OutputId.
  ;; Encountering this op outside a handler region is a structural
  ;; error — the insn should only be reachable via translate-kappa-
  ;; group's handler-region walk.
  (cond
    [(eq? op 'java/exception-ref)
     (define exn-oid (current-exn-out))
     (unless exn-oid
       (error 'translate-vfinsn
              "'java/exception-ref insn encountered outside a Kappa handler region"))
     (unless (and (= n-in 0) (= n-out 1))
       (error 'translate-vfinsn
              "'java/exception-ref insn has unexpected arity (in=~a out=~a)"
              n-in n-out))
     (define out-var (pvector-ref outputs 0))
     (unless (VarId? out-var)
       (error 'translate-vfinsn
              "'java/exception-ref output is not a VarId: ~s" out-var))
     (values region (idmap-set var->out out-var exn-oid))]
    [else
     (translate-vfinsn/generic region var->out op inputs outputs n-in n-out)]))


(define (translate-vfinsn/generic region var->out op inputs outputs n-in n-out)
  ;; Resolve each input: VarId lookup, or materialise a const node.
  (define-values (region1 input-outputs)
    (for/fold ([r region] [acc (pvector-empty)])
              ([x (in-pvector inputs)])
      (cond
        [(VarId? x)
         (define oid (idmap-ref var->out x #f))
         (unless oid
           (error 'translate-vfinsn "undefined ~a for op ~a" x op))
         (values r (pvector-cons-right acc oid))]
        [else
         (define-values (r* _nid _ins outs)
           (region-add-node r (Simple (list 'const x)) 0 1))
         (values r* (pvector-cons-right acc (pvector-ref outs 0)))])))

  ;; Main op node.
  (define-values (region2 _nid in-ids out-ids)
    (region-add-node region1 (Simple op) n-in n-out))

  ;; Wire inputs.
  (define region3
    (for/fold ([r region2])
              ([src (in-pvector input-outputs)]
               [dst (in-pvector in-ids)])
      (define-values (r* _w) (region-add-wire r src dst))
      r*))

  ;; Extend var → output.
  (define var->out*
    (for/fold ([m var->out])
              ([vout (in-pvector outputs)]
               [oid (in-pvector out-ids)]
               #:when (VarId? vout))
      (idmap-set m vout oid)))

  (values region3 var->out*))
