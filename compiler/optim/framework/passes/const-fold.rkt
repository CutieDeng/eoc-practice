#lang racket/base

;; ============================================================
;; Abstract Constant Folding Pass
;; ============================================================
;;
;; Evaluates constant expressions at compile time.
;; Uses the InsnSemantics abstraction for portability.
;;
;; This pass demonstrates how to write an optimization that
;; works with any IR, not just JVM bytecode.
;; ============================================================

(require racket/match racket/list racket/hash)
(require "../semantics.rkt")
(require "../pass.rkt")
(require "../../../ir/cfg/main.rkt")

;; ============================================================
;; Constant Folding Algorithm
;; ============================================================

;; Build a map from VarId to constant value
;; by analyzing instructions in block order
(define (build-const-map cfg sem)
  (define const-map (make-hash))

  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (for ([insn (CfgBlock-insns block)])
        (when (VfInsn? insn)
          (define outputs (VfInsn-outputs insn))
          (when (and (pair? outputs)
                     (= (length outputs) 1))
            (define out-var (car outputs))
            ;; Try to evaluate this instruction
            (define result
              (sem-const-eval sem insn
                              (lambda (v)
                                (hash-ref const-map v #f))))
            (when result
              (hash-set! const-map out-var result)))))))

  const-map)

;; Transform block by folding constants
(define (fold-block-constants block sem const-map ctx)
  (define new-insns
    (for/list ([insn (CfgBlock-insns block)])
      (if (VfInsn? insn)
          (fold-insn-constants insn sem const-map ctx)
          insn)))

  (if (equal? new-insns (CfgBlock-insns block))
      block
      (struct-copy CfgBlock block [insns new-insns])))

;; Try to fold a single instruction
(define (fold-insn-constants insn sem const-map ctx)
  (define outputs (VfInsn-outputs insn))

  ;; Only fold single-output instructions
  (if (and (pair? outputs) (= (length outputs) 1))
      (let* ([out-var (car outputs)]
             [const-val (hash-ref const-map out-var #f)])
        (if const-val
            ;; Replace with constant
            (begin
              (ctx-incr-stat! ctx 'folded)
              (VfInsn 'const (list const-val) outputs
                      (VfInsn-info insn) (VfInsn-id insn)))
            ;; No constant value, keep original
            insn))
      insn))

;; Main transformation
(define (const-fold-transform cfg sem ctx)
  (define const-map (build-const-map cfg sem))
  (ctx-record-stat! ctx 'constants-found (hash-count const-map))

  (define changed? #f)
  (define cfg^
    (for/fold ([cfg cfg])
              ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (if block
          (let ([new-block (fold-block-constants block sem const-map ctx)])
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

(define const-fold-pass
  (OptPass
   'const-fold
   "Evaluate constant expressions at compile time"
   const-fold-transform
   '(const-values)  ; preserves const analysis
   '()              ; no requirements
   '(liveness)))    ; invalidates liveness

(provide const-fold-pass)

;; ============================================================
;; Convenience Function
;; ============================================================

;; Run constant folding with given semantics
(define (cfg-const-fold/sem cfg sem)
  (define ctx (make-context sem))
  (define result (run-pass const-fold-pass cfg ctx))
  (PassResult-cfg result))

;; Note: For backward compatibility with JVM semantics,
;; use: (cfg-const-fold/sem cfg jvm-semantics)
;; after requiring "../jvm-semantics.rkt"

(provide cfg-const-fold/sem)
