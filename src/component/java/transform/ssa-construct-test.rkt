#lang racket/base

;; ============================================================
;; Tests: pre-SSA CFG → SSA (M3)
;; ============================================================

(require rackunit
         racket/runtime-path
         "jvm-to-cfg.rkt"
         "ssa-construct.rkt"
         "../../../frontend/java/reader.rkt"
         "../../../kernel/ir/jvm/types.rkt"
         "../../../kernel/ir/cfg/types.rkt"
         "../../../component/cfg/utils/graph-ops.rkt"
         (except-in "../../../kernel/data/data.rkt" integer-compare))

(define-runtime-path fixture-class-transform
  "../../../../test/integration/ClassTransform.dat")

(module+ test
  (define (mk-insn op . args) (JvmInsn op args))
  (define (mk-method insns #:name [name "m"] #:desc [desc "()V"])
    (JvmMethod name desc 0 0 0 insns '() '() '() '() '() '()))

  (define (all-outputs cfg)
    ;; Flatten every VfInsn and PhiInsn output VarId across the CFG.
    (for/fold ([acc (pvector-empty)])
              ([bid (in-list (cfg-all-block-ids cfg))])
      (define blk (cfg-get-block cfg bid))
      (define acc*
        (for/fold ([a acc]) ([phi (in-pvector (CfgBlock-phis blk))])
          (pvector-cons-right a (PhiInsn-output phi))))
      (for/fold ([a acc*]) ([i (in-pvector (CfgBlock-insns blk))])
        (for/fold ([a a]) ([o (in-pvector (VfInsn-outputs i))]
                           #:when (VarId? o))
          (pvector-cons-right a o)))))

  (define (no-duplicate-defs? cfg)
    (define outs (all-outputs cfg))
    (define seen (ordered-map-empty var-id-compare))
    (define-values (_seen ok?)
      (for/fold ([s seen] [ok? #t]) ([v (in-pvector outs)])
        (cond
          [(not ok?) (values s #f)]
          [(ordered-map-ref s v #f) (values s #f)]
          [else (values (ordered-map-set s v #t) #t)])))
    ok?)

  ;; ----- trivial method: nothing to rename -----
  (test-case "RETURN-only: SSA is identity in shape"
    (define m (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L0")
                               (mk-insn 'RETURN))))
    (define cfg0 (jvm-method->cfg m))
    (define cfg1 (jvm-cfg->ssa cfg0))
    (check-equal? (length (cfg-all-block-ids cfg1)) 1)
    (define blk (cfg-get-block cfg1 (Cfg-entry cfg1)))
    (check-equal? (pvector-length (CfgBlock-phis blk)) 0)
    (check-equal? (pvector-length (CfgBlock-insns blk)) 0))

  ;; ----- single ISTORE: no phi needed (single def, in entry) -----
  (test-case "ICONST_1 + ISTORE 0 + RETURN"
    (define m (mk-method
                (list (mk-insn 'CUTIEDENG-LABEL "L0")
                      (mk-insn 'ICONST_1)
                      (mk-insn 'ISTORE 0)
                      (mk-insn 'RETURN))))
    (define cfg1 (jvm-cfg->ssa (jvm-method->cfg m)))
    (check-true (no-duplicate-defs? cfg1))
    (define blk (cfg-get-block cfg1 (Cfg-entry cfg1)))
    (check-equal? (pvector-length (CfgBlock-phis blk)) 0))

  ;; ----- ISTORE on both branches of an IF -> phi at join -----
  (test-case "IF-join needs a phi for slot 0"
    ;; if (arg0) { local0 = 1; } else { local0 = 2; } return local0;
    (define m (mk-method
                (list (mk-insn 'CUTIEDENG-LABEL "L0")
                      (mk-insn 'ILOAD 0)
                      (mk-insn 'IFEQ "L_ELSE")
                      (mk-insn 'CUTIEDENG-LABEL "L_THEN")
                      (mk-insn 'ICONST_1)
                      (mk-insn 'ISTORE 0)
                      (mk-insn 'GOTO "L_JOIN")
                      (mk-insn 'CUTIEDENG-LABEL "L_ELSE")
                      (mk-insn 'ICONST_2)
                      (mk-insn 'ISTORE 0)
                      (mk-insn 'CUTIEDENG-LABEL "L_JOIN")
                      (mk-insn 'ILOAD 0)
                      (mk-insn 'IRETURN))
                #:desc "(I)I"))
    (define cfg0 (jvm-method->cfg m))
    (define cfg1 (jvm-cfg->ssa cfg0))
    (check-true (no-duplicate-defs? cfg1))
    ;; Locate the join block.
    (define join-bid
      (for/or ([bid (in-list (cfg-all-block-ids cfg1))])
        (define blk (cfg-get-block cfg1 bid))
        (and (= (pvector-length (CfgBlock-phis blk)) 1) bid)))
    (check-not-false join-bid)
    (define join-blk (cfg-get-block cfg1 join-bid))
    (define phi (pvector-ref (CfgBlock-phis join-blk) 0))
    ;; phi has 2 sources (one per predecessor).
    (check-equal? (pvector-length (PhiInsn-sources phi)) 2)
    ;; all phi sources are fully renamed (not VarId(0)).
    (for ([src (in-pvector (PhiInsn-sources phi))])
      (define nm (cdr src))
      (check-true (VarId? nm))
      (check-true (> (VarId-id nm) 0)
                  (format "phi source must be renamed, got ~a" nm))))

  ;; ----- loop: defs in both entry and loop body -> phi at header -----
  (test-case "Simple loop: phi at loop header for slot 0"
    ;; init slot 0 = 0; while (cond) { slot 0 = slot 0 + 1; } return slot 0;
    (define m (mk-method
                (list (mk-insn 'CUTIEDENG-LABEL "L_ENTRY")
                      (mk-insn 'ICONST_0)
                      (mk-insn 'ISTORE 0)
                      (mk-insn 'CUTIEDENG-LABEL "L_HEAD")
                      (mk-insn 'ILOAD 1)
                      (mk-insn 'IFEQ "L_EXIT")
                      (mk-insn 'CUTIEDENG-LABEL "L_BODY")
                      (mk-insn 'ILOAD 0)
                      (mk-insn 'ICONST_1)
                      (mk-insn 'IADD)
                      (mk-insn 'ISTORE 0)
                      (mk-insn 'GOTO "L_HEAD")
                      (mk-insn 'CUTIEDENG-LABEL "L_EXIT")
                      (mk-insn 'ILOAD 0)
                      (mk-insn 'IRETURN))
                #:desc "(I)I"))
    (define cfg1 (jvm-cfg->ssa (jvm-method->cfg m)))
    (check-true (no-duplicate-defs? cfg1))
    ;; exactly one block carries a phi (the loop header).
    (define phi-blocks
      (for/list ([bid (in-list (cfg-all-block-ids cfg1))]
                 #:when (> (pvector-length (CfgBlock-phis
                                             (cfg-get-block cfg1 bid)))
                            0))
        bid))
    (check-equal? (length phi-blocks) 1))

  ;; ----- fixture init: no multi-def locals -> no phis -----
  (test-case "fixture: init method has no phis"
    (define klass (read-jvm-class-file fixture-class-transform))
    (define init-m
      (for/or ([mth (JvmClass-methods klass)])
        (and (equal? (JvmMethod-name mth) "init") mth)))
    (check-not-false init-m)
    (define cfg1 (jvm-cfg->ssa (jvm-method->cfg init-m)))
    (check-true (no-duplicate-defs? cfg1))
    (for ([bid (in-list (cfg-all-block-ids cfg1))])
      (check-equal? (pvector-length (CfgBlock-phis (cfg-get-block cfg1 bid)))
                    0))))
