#lang racket/base

;; ============================================================
;; Tests: JVM basic-block splitting (M1)
;; ============================================================

(require rackunit
         racket/runtime-path
         "bbs.rkt"
         "../../../frontend/java/reader.rkt"
         "../../../kernel/ir/jvm/types.rkt"
         "../../../kernel/data/data.rkt")

(define-runtime-path fixture-class-transform
  "../../../../test/integration/ClassTransform.dat")

(module+ test
  (define (mk-insn op . args)
    (JvmInsn op args))

  (define (mk-method insns)
    (JvmMethod "m" "()V" 0 0 0 insns '() '() '() '() '() '()))

  (define (bb-labels mbb)
    (for/list ([l (in-pvector (JvmMethodBBs-order mbb))]) l))

  (define (succs-of mbb lbl)
    (for/list ([s (in-pvector
                    (JvmBB-successors
                      (ordered-map-ref (JvmMethodBBs-blocks mbb) lbl #f)))])
      s))

  (define (insn-ops mbb lbl)
    (for/list ([i (in-pvector
                    (JvmBB-insns
                      (ordered-map-ref (JvmMethodBBs-blocks mbb) lbl #f)))])
      (JvmInsn-opcode i)))

  ;; --------------------------------------------------------
  ;; Trivial: label + RETURN
  ;; --------------------------------------------------------
  (test-case "single-block method"
    (define m (mk-method
                (list (mk-insn 'CUTIEDENG-LABEL "L0")
                      (mk-insn 'RETURN))))
    (define mbb (jvm-method->bbs m))
    (check-equal? (JvmMethodBBs-entry mbb) "L0")
    (check-equal? (bb-labels mbb) '("L0"))
    (check-equal? (succs-of mbb "L0") '())
    (check-equal? (insn-ops mbb "L0") '(RETURN)))

  ;; --------------------------------------------------------
  ;; Fall-through between two labels (like <init>)
  ;; --------------------------------------------------------
  (test-case "two-block fall-through"
    (define m (mk-method
                (list (mk-insn 'CUTIEDENG-LABEL "L0")
                      (mk-insn 'GETSTATIC)
                      (mk-insn 'CUTIEDENG-LABEL "L1")
                      (mk-insn 'RETURN))))
    (define mbb (jvm-method->bbs m))
    (check-equal? (bb-labels mbb) '("L0" "L1"))
    (check-equal? (succs-of mbb "L0") '("L1"))
    (check-equal? (succs-of mbb "L1") '()))

  ;; --------------------------------------------------------
  ;; GOTO: unconditional jump
  ;; --------------------------------------------------------
  (test-case "GOTO successor"
    (define m (mk-method
                (list (mk-insn 'CUTIEDENG-LABEL "L0")
                      (mk-insn 'GOTO "L2")
                      (mk-insn 'CUTIEDENG-LABEL "L1")
                      (mk-insn 'RETURN)
                      (mk-insn 'CUTIEDENG-LABEL "L2")
                      (mk-insn 'RETURN))))
    (define mbb (jvm-method->bbs m))
    (check-equal? (bb-labels mbb) '("L0" "L1" "L2"))
    (check-equal? (succs-of mbb "L0") '("L2"))
    (check-equal? (succs-of mbb "L1") '())
    (check-equal? (succs-of mbb "L2") '()))

  ;; --------------------------------------------------------
  ;; Conditional branch: two successors (target + fallthrough)
  ;; --------------------------------------------------------
  (test-case "IFEQ two successors"
    (define m (mk-method
                (list (mk-insn 'CUTIEDENG-LABEL "L0")
                      (mk-insn 'ILOAD 1)
                      (mk-insn 'IFEQ "L2")
                      (mk-insn 'CUTIEDENG-LABEL "L1")
                      (mk-insn 'RETURN)
                      (mk-insn 'CUTIEDENG-LABEL "L2")
                      (mk-insn 'RETURN))))
    (define mbb (jvm-method->bbs m))
    (check-equal? (succs-of mbb "L0") '("L2" "L1"))
    (check-equal? (succs-of mbb "L1") '())
    (check-equal? (succs-of mbb "L2") '()))

  ;; --------------------------------------------------------
  ;; Exception entries lifted out; label-only blocks preserved
  ;; --------------------------------------------------------
  (test-case "TRY-CATCH-BLOCK extracted"
    (define m (mk-method
                (list (mk-insn 'TRY-CATCH-BLOCK "LS" "LE" "LH" "java/lang/Exception")
                      (mk-insn 'CUTIEDENG-LABEL "LS")
                      (mk-insn 'ALOAD 1)
                      (mk-insn 'CUTIEDENG-LABEL "LE")
                      (mk-insn 'GOTO "END")
                      (mk-insn 'CUTIEDENG-LABEL "LH")
                      (mk-insn 'ASTORE 2)
                      (mk-insn 'CUTIEDENG-LABEL "END")
                      (mk-insn 'RETURN))))
    (define mbb (jvm-method->bbs m))
    (check-equal? (bb-labels mbb) '("LS" "LE" "LH" "END"))
    (check-equal? (succs-of mbb "LS") '("LE"))
    (check-equal? (succs-of mbb "LE") '("END"))
    (check-equal? (succs-of mbb "LH") '("END"))
    (check-equal? (succs-of mbb "END") '())
    (define excs (JvmMethodBBs-exceptions mbb))
    (check-equal? (pvector-length excs) 1)
    (define exc (pvector-ref excs 0))
    (check-equal? (JvmExceptionEntry-start exc) "LS")
    (check-equal? (JvmExceptionEntry-end exc) "LE")
    (check-equal? (JvmExceptionEntry-handler exc) "LH")
    (check-equal? (JvmExceptionEntry-catch-type exc) "java/lang/Exception"))

  ;; --------------------------------------------------------
  ;; Synthetic entry when real insns precede the first label
  ;; --------------------------------------------------------
  (test-case "synthetic entry label"
    (define m (mk-method
                (list (mk-insn 'ALOAD 0)
                      (mk-insn 'CUTIEDENG-LABEL "L0")
                      (mk-insn 'RETURN))))
    (define mbb (jvm-method->bbs m))
    (check-equal? (JvmMethodBBs-entry mbb) "$entry")
    (check-equal? (bb-labels mbb) '("$entry" "L0"))
    (check-equal? (succs-of mbb "$entry") '("L0")))

  ;; --------------------------------------------------------
  ;; Real fixture: ClassTransform's `test` method
  ;; --------------------------------------------------------
  (test-case "fixture: test method bbs"
    (define klass (read-jvm-class-file fixture-class-transform))
    (define test-method
      (for/or ([mth (JvmClass-methods klass)])
        (and (equal? (JvmMethod-name mth) "test") mth)))
    (check-not-false test-method)
    (define mbb (jvm-method->bbs test-method))
    ;; 5 labels in source: L1452126962 L931919113 L1607521710 L381259350 L764977973
    (check-equal? (pvector-length (JvmMethodBBs-order mbb)) 5)
    (check-equal? (pvector-length (JvmMethodBBs-exceptions mbb)) 1)
    (check-equal? (JvmMethodBBs-entry mbb) "L1452126962")
    (check-equal? (succs-of mbb "L1452126962") '("L931919113"))
    (check-equal? (succs-of mbb "L931919113") '("L764977973"))
    (check-equal? (succs-of mbb "L764977973") '())))
