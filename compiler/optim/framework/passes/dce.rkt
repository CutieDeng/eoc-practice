#lang racket/base

;; ============================================================
;; Abstract Dead Code Elimination Pass
;; ============================================================
;;
;; Removes instructions whose results are never used.
;; Uses the InsnSemantics abstraction for portability.
;; ============================================================

(require racket/match racket/list racket/set)
(require "../semantics.rkt")
(require "../pass.rkt")
(require "../../../ir/cfg/main.rkt")

;; ============================================================
;; Liveness Analysis
;; ============================================================

;; Collect all used variables in the CFG
(define (collect-used-vars cfg sem)
  (define used (mutable-set))

  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      ;; Collect uses from instructions
      (for ([insn (CfgBlock-insns block)])
        (when (VfInsn? insn)
          (for ([input (VfInsn-inputs insn)])
            (when (VarId? input)
              (set-add! used input)))))

      ;; Collect uses from PHI nodes
      (for ([phi (CfgBlock-phis block)])
        (when (PhiInsn? phi)
          (for ([src (PhiInsn-sources phi)])
            (when (VarId? (cdr src))
              (set-add! used (cdr src))))))

      ;; Collect uses from terminator
      (define term (CfgBlock-terminator block))
      (for ([v (terminator-uses term)])
        (when (VarId? v)
          (set-add! used v)))))

  used)

;; Iteratively compute live variables
;; (Variables that are used directly or through chains)
(define (compute-live-vars cfg sem)
  (define used (collect-used-vars cfg sem))

  ;; Build def->uses graph for propagation
  (define var->def (make-hash))
  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (for ([insn (CfgBlock-insns block)])
        (when (VfInsn? insn)
          (for ([out (VfInsn-outputs insn)])
            (hash-set! var->def out insn))))))

  ;; Propagate: if var is live, its inputs are live
  (define live (set-copy used))
  (define worklist (set->list used))

  (let loop ()
    (unless (null? worklist)
      (define var (car worklist))
      (set! worklist (cdr worklist))

      ;; Find defining instruction
      (define def-insn (hash-ref var->def var #f))
      (when def-insn
        (for ([input (VfInsn-inputs def-insn)])
          (when (and (VarId? input)
                     (not (set-member? live input)))
            (set-add! live input)
            (set! worklist (cons input worklist)))))

      (loop)))

  live)

;; ============================================================
;; Dead Code Elimination
;; ============================================================

;; Check if instruction can be eliminated
(define (can-eliminate? insn sem live-vars)
  (cond
    ;; Instructions with side effects cannot be eliminated
    [(sem-has-side-effects? sem insn) #f]

    ;; Check if all outputs are dead
    [else
     (define outputs (VfInsn-outputs insn))
     (for/and ([out outputs])
       (not (set-member? live-vars out)))]))

;; Transform block by removing dead code
(define (eliminate-dead-in-block block sem live-vars ctx)
  (define new-insns
    (for/list ([insn (CfgBlock-insns block)]
               #:unless (and (VfInsn? insn)
                            (can-eliminate? insn sem live-vars)))
      insn))

  (define eliminated (- (length (CfgBlock-insns block))
                        (length new-insns)))
  (when (> eliminated 0)
    (ctx-incr-stat! ctx 'eliminated eliminated))

  (if (= eliminated 0)
      block
      (struct-copy CfgBlock block [insns new-insns])))

;; Main transformation
(define (dce-transform cfg sem ctx)
  (define live-vars (compute-live-vars cfg sem))
  (ctx-record-stat! ctx 'live-vars (set-count live-vars))

  (define changed? #f)
  (define cfg^
    (for/fold ([cfg cfg])
              ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (if block
          (let ([new-block (eliminate-dead-in-block block sem live-vars ctx)])
            (when (not (eq? block new-block))
              (set! changed? #t))
            (cfg-set-block cfg new-block))
          cfg)))

  (if changed?
      (pass-changed cfg^ ctx)
      (pass-unchanged cfg ctx)))

;; ============================================================
;; Pass Definition
;; ============================================================

(define dce-pass
  (OptPass
   'dce
   "Remove instructions whose results are never used"
   dce-transform
   '()                ; preserves nothing
   '()                ; no requirements
   '(liveness use-def))) ; invalidates

(provide dce-pass)

;; ============================================================
;; Convenience Function
;; ============================================================

(define (cfg-dce/sem cfg sem)
  (define ctx (make-context sem))
  (define result (run-pass dce-pass cfg ctx))
  (PassResult-cfg result))

;; Note: For backward compatibility with JVM semantics,
;; use: (cfg-dce/sem cfg jvm-semantics)
;; after requiring "../jvm-semantics.rkt"

(provide cfg-dce/sem)
