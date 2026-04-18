#lang racket/base

;; ============================================================
;; Tests: JVM → pre-SSA CFG (M2)
;; ============================================================

(require rackunit
         racket/runtime-path
         (only-in cutie-ftree/graph graph-vertex-count graph-edge-count)
         "jvm-to-cfg.rkt"
         "../../../frontend/java/reader.rkt"
         "../../../kernel/ir/jvm/types.rkt"
         "../../../kernel/ir/cfg/types.rkt"
         (except-in "../../../kernel/data/data.rkt" integer-compare))

(define-runtime-path fixture-class-transform
  "../../../../test/integration/ClassTransform.dat")

(module+ test
  (define (mk-insn op . args) (JvmInsn op args))
  (define (mk-method insns #:name [name "m"] #:desc [desc "()V"])
    (JvmMethod name desc 0 0 0 insns '() '() '() '() '() '()))

  ;; ----- parse-method-descriptor -----
  (test-case "descriptor: ()V"
    (define-values (n r?) (parse-method-descriptor "()V"))
    (check-equal? n 0)
    (check-equal? r? #f))
  (test-case "descriptor: (II)V"
    (define-values (n r?) (parse-method-descriptor "(II)V"))
    (check-equal? n 2)
    (check-equal? r? #f))
  (test-case "descriptor: (Ljava/lang/String;)Ljava/lang/String;"
    (define-values (n r?) (parse-method-descriptor "(Ljava/lang/String;)Ljava/lang/String;"))
    (check-equal? n 1)
    (check-equal? r? #t))
  (test-case "descriptor: ([IJLjava/lang/String;)[[B"
    (define-values (n r?) (parse-method-descriptor "([IJLjava/lang/String;)[[B"))
    (check-equal? n 3)
    (check-equal? r? #t))

  ;; ----- trivial method -----
  (test-case "RETURN-only method"
    (define m (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L0")
                               (mk-insn 'RETURN))))
    (define cfg (jvm-method->cfg m))
    (define entry (Cfg-entry cfg))
    (define blk (ordered-map-ref (Cfg-blocks cfg) entry #f))
    (check-not-false blk)
    (check-equal? (pvector-length (CfgBlock-insns blk)) 0)
    (check-pred Term:ret? (CfgBlock-terminator blk))
    (check-equal? (pvector-length (Term:ret-values (CfgBlock-terminator blk))) 0))

  ;; ----- IRETURN of a constant -----
  (test-case "ICONST_1 + IRETURN"
    (define m (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L0")
                               (mk-insn 'ICONST_1)
                               (mk-insn 'IRETURN))
                         #:desc "()I"))
    (define cfg (jvm-method->cfg m))
    (define blk (ordered-map-ref (Cfg-blocks cfg) (Cfg-entry cfg) #f))
    (define insns (CfgBlock-insns blk))
    ;; One VfInsn (ICONST_1) and a Term:ret
    (check-equal? (pvector-length insns) 1)
    (check-equal? (VfInsn-op (pvector-ref insns 0)) 'ICONST_1)
    (define term (CfgBlock-terminator blk))
    (check-pred Term:ret? term)
    (check-equal? (pvector-length (Term:ret-values term)) 1)
    ;; the returned VarId equals ICONST_1's output
    (check-equal? (pvector-ref (Term:ret-values term) 0)
                  (pvector-ref (VfInsn-outputs (pvector-ref insns 0)) 0)))

  ;; ----- IADD of locals -----
  (test-case "ILOAD 0; ILOAD 1; IADD; IRETURN"
    (define m (mk-method
                (list (mk-insn 'CUTIEDENG-LABEL "L0")
                      (mk-insn 'ILOAD 0)
                      (mk-insn 'ILOAD 1)
                      (mk-insn 'IADD)
                      (mk-insn 'IRETURN))
                #:desc "(II)I"))
    (define cfg (jvm-method->cfg m))
    (define blk (ordered-map-ref (Cfg-blocks cfg) (Cfg-entry cfg) #f))
    (define insns (CfgBlock-insns blk))
    (check-equal? (pvector-length insns) 3)
    (check-equal? (VfInsn-op (pvector-ref insns 0)) 'ILOAD)
    (check-equal? (VfInsn-op (pvector-ref insns 1)) 'ILOAD)
    (check-equal? (VfInsn-op (pvector-ref insns 2)) 'IADD)
    ;; ILOAD 0 reads VarId(0)
    (check-equal? (pvector-ref (VfInsn-inputs (pvector-ref insns 0)) 0)
                  (VarId 0))
    ;; ILOAD 1 reads VarId(1)
    (check-equal? (pvector-ref (VfInsn-inputs (pvector-ref insns 1)) 0)
                  (VarId 1))
    ;; IADD inputs = [iload0-out, iload1-out]
    (define iload0-out (pvector-ref (VfInsn-outputs (pvector-ref insns 0)) 0))
    (define iload1-out (pvector-ref (VfInsn-outputs (pvector-ref insns 1)) 0))
    (check-equal? (pvector-ref (VfInsn-inputs (pvector-ref insns 2)) 0) iload0-out)
    (check-equal? (pvector-ref (VfInsn-inputs (pvector-ref insns 2)) 1) iload1-out))

  ;; ----- IFEQ: two successors + pre-terminator VfInsn -----
  (test-case "ILOAD+IFEQ lowered to Term:cond"
    (define m (mk-method
                (list (mk-insn 'CUTIEDENG-LABEL "L0")
                      (mk-insn 'ILOAD 0)
                      (mk-insn 'IFEQ "L2")
                      (mk-insn 'CUTIEDENG-LABEL "L1")
                      (mk-insn 'RETURN)
                      (mk-insn 'CUTIEDENG-LABEL "L2")
                      (mk-insn 'RETURN))
                #:desc "(I)V"))
    (define cfg (jvm-method->cfg m))
    (check-equal? (graph-vertex-count (Cfg-graph cfg)) 3)
    (check-equal? (graph-edge-count  (Cfg-graph cfg)) 2)
    (define entry-blk (ordered-map-ref (Cfg-blocks cfg) (Cfg-entry cfg) #f))
    (define t (CfgBlock-terminator entry-blk))
    (check-pred Term:cond? t)
    ;; Entry block instructions: ILOAD + IFEQ (pre-lowered)
    (check-equal? (pvector-length (CfgBlock-insns entry-blk)) 2)
    (check-equal? (VfInsn-op (pvector-ref (CfgBlock-insns entry-blk) 0)) 'ILOAD)
    (check-equal? (VfInsn-op (pvector-ref (CfgBlock-insns entry-blk) 1)) 'IFEQ))

  ;; ----- INVOKESTATIC pops based on descriptor -----
  (test-case "INVOKESTATIC (I)V pops one"
    (define m (mk-method
                (list (mk-insn 'CUTIEDENG-LABEL "L0")
                      (mk-insn 'ICONST_3)
                      (mk-insn 'INVOKESTATIC "Owner" "f" "(I)V")
                      (mk-insn 'RETURN))))
    (define cfg (jvm-method->cfg m))
    (define blk (ordered-map-ref (Cfg-blocks cfg) (Cfg-entry cfg) #f))
    (define call-i (pvector-ref (CfgBlock-insns blk) 1))
    (check-equal? (VfInsn-op call-i) 'INVOKESTATIC)
    (check-equal? (pvector-length (VfInsn-inputs call-i)) 1)
    (check-equal? (pvector-length (VfInsn-outputs call-i)) 0))

  ;; ----- INVOKEVIRTUAL includes receiver -----
  (test-case "INVOKEVIRTUAL (I)I pops receiver + 1 arg, pushes 1"
    (define m (mk-method
                (list (mk-insn 'CUTIEDENG-LABEL "L0")
                      (mk-insn 'ALOAD 0)
                      (mk-insn 'ICONST_3)
                      (mk-insn 'INVOKEVIRTUAL "Owner" "f" "(I)I" #f)
                      (mk-insn 'POP)
                      (mk-insn 'RETURN))))
    (define cfg (jvm-method->cfg m))
    (define blk (ordered-map-ref (Cfg-blocks cfg) (Cfg-entry cfg) #f))
    (define call-i (pvector-ref (CfgBlock-insns blk) 2))
    (check-equal? (VfInsn-op call-i) 'INVOKEVIRTUAL)
    (check-equal? (pvector-length (VfInsn-inputs call-i)) 2)
    (check-equal? (pvector-length (VfInsn-outputs call-i)) 1))

  ;; ----- real fixture: init -----
  (test-case "fixture: init method"
    (define klass (read-jvm-class-file fixture-class-transform))
    (define init-m
      (for/or ([mth (JvmClass-methods klass)])
        (and (equal? (JvmMethod-name mth) "init") mth)))
    (check-not-false init-m)
    (define cfg (jvm-method->cfg init-m))
    ;; 2 blocks (L1023892928, L558638686)
    (check-equal? (graph-vertex-count (Cfg-graph cfg)) 2)
    ;; one fallthrough edge between them
    (check-equal? (graph-edge-count (Cfg-graph cfg)) 1)
    ;; entry block: GETSTATIC + INVOKESTATIC
    (define entry-blk (ordered-map-ref (Cfg-blocks cfg) (Cfg-entry cfg) #f))
    (define insns (CfgBlock-insns entry-blk))
    (check-equal? (pvector-length insns) 2)
    (check-equal? (VfInsn-op (pvector-ref insns 0)) 'GETSTATIC)
    (check-equal? (VfInsn-op (pvector-ref insns 1)) 'INVOKESTATIC))

  ;; ----- real fixture: test (has try-catch + GOTO) -----
  (test-case "fixture: test method"
    (define klass (read-jvm-class-file fixture-class-transform))
    (define test-m
      (for/or ([mth (JvmClass-methods klass)])
        (and (equal? (JvmMethod-name mth) "test") mth)))
    (check-not-false test-m)
    (define cfg (jvm-method->cfg test-m))
    ;; 5 blocks
    (check-equal? (graph-vertex-count (Cfg-graph cfg)) 5)
    ;; edges: L1452126962→L931919113, L931919113→L764977973 (GOTO),
    ;;        L1607521710→L381259350 (fall), L381259350→L764977973 (fall)
    ;; L764977973 is terminal.  Total 4 edges.
    (check-equal? (graph-edge-count (Cfg-graph cfg)) 4)))
