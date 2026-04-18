#lang racket/base

;; ============================================================
;; Component: Java Basic-Block Splitting (M1)
;; ============================================================
;;
;; Turn a JvmMethod's linear instruction stream into a set of
;; basic blocks keyed by JVM label.  No stack simulation, no
;; opcode→VfInsn translation yet -- this pass only isolates
;; control flow.
;;
;; Input  : JvmMethod
;; Output : JvmMethodBBs  (see struct definitions below)
;;
;; Conventions:
;;   - The ASM-based Java tool emits a CUTIEDENG-LABEL pseudo-insn
;;     at every CFG boundary, so labels are our block leaders.
;;   - TRY-CATCH-BLOCK pseudo-insns are lifted into JvmExceptionEntry
;;     records and dropped from the block stream.
;;   - If real insns precede the first label, a synthetic 'entry
;;     block is introduced.
;;   - Successors are derived from the terminating opcode; non-
;;     terminating blocks fall through to the next block in source
;;     order.
;;
;; ============================================================

(require racket/match
         "../../../kernel/ir/jvm/types.rkt"
         "../../../kernel/data/data.rkt")

(provide
  (struct-out JvmBB)
  (struct-out JvmMethodBBs)
  jvm-method->bbs)

;; ============================================================
;; Data
;; ============================================================

;; Basic block at the JVM layer.
;;   label      : String          - entry label (ASM label name)
;;   insns      : pvector[JvmInsn]- real insns only (no pseudo)
;;   successors : pvector[String] - labels of successor blocks
(struct JvmBB (label insns successors) #:prefab)

;; BB-partition result for a single method.
;;   entry      : String                              - entry label
;;   blocks     : ordered-map[String -> JvmBB]
;;   order      : pvector[String]                     - source order
;;   exceptions : pvector[JvmExceptionEntry]
(struct JvmMethodBBs (entry blocks order exceptions) #:prefab)

;; Synthetic entry label used when real insns precede the first
;; CUTIEDENG-LABEL.  Namespaced with a leading $ so it cannot collide
;; with ASM-emitted "L<n>" names.
(define SYNTHETIC-ENTRY "$entry")

;; ============================================================
;; Main entry point
;; ============================================================

(define (jvm-method->bbs method)
  (define-values (segments exceptions)
    (split-at-labels (JvmMethod-instructions method)))
  (define order
    (for/pvector ([seg (in-pvector segments)])
      (car seg)))
  (define n (pvector-length order))
  (define blocks
    (for/fold ([m (ordered-map-empty string-compare)])
              ([seg (in-pvector segments)] [i (in-naturals)])
      (define label (car seg))
      (define insns (cdr seg))
      (define next-label
        (and (< (add1 i) n) (pvector-ref order (add1 i))))
      (define succs (compute-successors insns next-label))
      (ordered-map-set m label (JvmBB label insns succs))))
  (define entry (if (= n 0) SYNTHETIC-ENTRY (pvector-ref order 0)))
  (JvmMethodBBs entry blocks order exceptions))

;; ============================================================
;; Split the instruction stream at labels
;; ============================================================

;; Returns (values segments exceptions) where
;;   segments   : pvector[(Pairof Symbol pvector[JvmInsn])]
;;   exceptions : pvector[JvmExceptionEntry]
(define (split-at-labels all-insns)
  (define-values (segs-rev excs-rev cur-label cur-insns-rev)
    (for/fold ([segs-rev '()]
               [excs-rev '()]
               [cur-label #f]
               [cur-insns-rev '()])
              ([i (in-list all-insns)])
      (case (JvmInsn-opcode i)
        [(CUTIEDENG-LABEL)
         (define label (car (JvmInsn-operands i)))
         (define closed
           (if cur-label
               (cons (cons cur-label cur-insns-rev) segs-rev)
               segs-rev))
         (values closed excs-rev label '())]
        [(TRY-CATCH-BLOCK)
         (match (JvmInsn-operands i)
           [(list start end handler type)
            (values segs-rev
                    (cons (JvmExceptionEntry start end handler type) excs-rev)
                    cur-label
                    cur-insns-rev)])]
        [else
         (values segs-rev
                 excs-rev
                 (or cur-label SYNTHETIC-ENTRY)
                 (cons i cur-insns-rev))])))
  (define segs-rev-final
    (if cur-label
        (cons (cons cur-label cur-insns-rev) segs-rev)
        segs-rev))
  (values
    (for/pvector ([seg (in-list (reverse segs-rev-final))])
      (cons (car seg)
            (for/pvector ([x (in-list (reverse (cdr seg)))]) x)))
    (for/pvector ([e (in-list (reverse excs-rev))]) e)))

;; ============================================================
;; Successors from terminator
;; ============================================================

(define (compute-successors insns next-label)
  (cond
    [(= (pvector-length insns) 0)
     (fallthrough-succs next-label)]
    [else
     (define last-i (pvector-ref insns (sub1 (pvector-length insns))))
     (successors-for last-i next-label)]))

(define (fallthrough-succs next-label)
  (if next-label
      (pvector-cons-right (pvector-empty) next-label)
      (pvector-empty)))

(define (successors-for insn next-label)
  (define op (JvmInsn-opcode insn))
  (define args (JvmInsn-operands insn))
  (cond
    ;; unconditional jumps
    [(memq op '(GOTO GOTO_W))
     (pvector-cons-right (pvector-empty) (car args))]

    ;; conditional jumps: (branch-target, fallthrough)
    [(memq op '(IFEQ IFNE IFLT IFGE IFGT IFLE
                IF_ICMPEQ IF_ICMPNE IF_ICMPLT IF_ICMPGE IF_ICMPGT IF_ICMPLE
                IF_ACMPEQ IF_ACMPNE IFNULL IFNONNULL))
     (define target (car args))
     (define base (pvector-cons-right (pvector-empty) target))
     (if next-label
         (pvector-cons-right base next-label)
         base)]

    ;; returns / throws terminate: no successor
    [(memq op '(RETURN IRETURN LRETURN FRETURN DRETURN ARETURN ATHROW))
     (pvector-empty)]

    ;; TABLESWITCH / LOOKUPSWITCH --
    ;; ASM reader encoding for these is not yet exercised by any fixture;
    ;; leave a conservative no-successor result and revisit in M1.5 when
    ;; a switch-bearing test class exists.
    [(memq op '(TABLESWITCH LOOKUPSWITCH))
     (pvector-empty)]

    ;; anything else: ordinary insn, fall through
    [else (fallthrough-succs next-label)]))
