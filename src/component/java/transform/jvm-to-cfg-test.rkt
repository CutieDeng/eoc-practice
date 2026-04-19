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
    (check-equal? (graph-edge-count (Cfg-graph cfg)) 4))

  ;; ----- synthetic try/catch: exception table preserved on Cfg.info -----
  (test-case "try/catch preserves exception-table in Cfg.info"
    ;; try { return arg0; } catch (RuntimeException e) { return -1; }
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L_TRY")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IRETURN)
                       (mk-insn 'CUTIEDENG-LABEL "L_HANDLER")
                       (mk-insn 'ASTORE 1)
                       (mk-insn 'ICONST_M1)
                       (mk-insn 'IRETURN)
                       (mk-insn 'TRY-CATCH-BLOCK
                                "L_TRY" "L_HANDLER" "L_HANDLER"
                                "java/lang/RuntimeException"))
                 #:desc "(I)I"))
    (define cfg (jvm-method->cfg m))
    (define info (Cfg-info cfg))
    (define table (ordered-map-ref info 'java/exception-table #f))
    (check-pred (lambda (x) (and x (> (pvector-length x) 0)))
                table
                "Cfg.info should carry 'java/exception-table")
    (check-equal? (pvector-length table) 1)
    (define rec (pvector-ref table 0))
    ;; (list start-bid end-bid handler-bid catch-type); end-label ==
    ;; handler-label here, so end-bid = handler-bid.
    (check-pred BlockId? (car rec) "start-bid is a BlockId")
    (check-pred BlockId? (caddr rec) "handler-bid is a BlockId")
    (check-equal? (cadddr rec) "java/lang/RuntimeException"))

  (test-case "try/catch: covered block carries covering-handlers annotation"
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L_TRY")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IRETURN)
                       (mk-insn 'CUTIEDENG-LABEL "L_HANDLER")
                       (mk-insn 'ASTORE 1)
                       (mk-insn 'ICONST_M1)
                       (mk-insn 'IRETURN)
                       (mk-insn 'TRY-CATCH-BLOCK
                                "L_TRY" "L_HANDLER" "L_HANDLER"
                                "java/lang/RuntimeException"))
                 #:desc "(I)I"))
    (define cfg (jvm-method->cfg m))
    (define entry (Cfg-entry cfg))
    (define entry-blk (ordered-map-ref (Cfg-blocks cfg) entry #f))
    (define covering
      (ordered-map-ref (CfgBlock-info entry-blk) 'java/covering-handlers #f))
    (check-pred (lambda (x) (and x (= 1 (pvector-length x))))
                covering
                "try block should carry one covering handler")
    (define pair (pvector-ref covering 0))
    (check-equal? (car pair) "java/lang/RuntimeException"
                  "covering pair's catch-type")
    (check-pred BlockId? (cdr pair) "covering pair's handler-bid")
    ;; Handler block itself is excluded from its own try range.
    (define handler-bid (cdr pair))
    (define handler-blk (ordered-map-ref (Cfg-blocks cfg) handler-bid #f))
    (check-equal? (ordered-map-ref (CfgBlock-info handler-blk)
                                   'java/covering-handlers #f)
                  #f
                  "handler block is not covered by its own try range"))

  (test-case "try/catch: handler block begins with a synthetic 'java/exception-ref producer"
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L_TRY")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IRETURN)
                       (mk-insn 'CUTIEDENG-LABEL "L_HANDLER")
                       (mk-insn 'ASTORE 1)
                       (mk-insn 'ICONST_M1)
                       (mk-insn 'IRETURN)
                       (mk-insn 'TRY-CATCH-BLOCK
                                "L_TRY" "L_HANDLER" "L_HANDLER"
                                "java/lang/RuntimeException"))
                 #:desc "(I)I"))
    (define cfg (jvm-method->cfg m))
    ;; Locate the handler block via the exception-table entry.
    (define table (ordered-map-ref (Cfg-info cfg) 'java/exception-table #f))
    (define handler-bid (caddr (pvector-ref table 0)))
    (define handler-blk (ordered-map-ref (Cfg-blocks cfg) handler-bid #f))
    (check-not-false handler-blk)
    (define insns (CfgBlock-insns handler-blk))
    (check-true (> (pvector-length insns) 0)
                "handler block should have at least the synthetic producer + ASTORE body")
    (define first-insn (pvector-ref insns 0))
    (check-equal? (VfInsn-op first-insn) 'java/exception-ref
                  "handler block's first insn should be the synthetic exception-ref producer")
    (check-equal? (pvector-length (VfInsn-inputs first-insn)) 0
                  "exception-ref producer takes no inputs")
    (check-equal? (pvector-length (VfInsn-outputs first-insn)) 1
                  "exception-ref producer produces exactly one output (stack height 1)")
    ;; The producer's output is consumed as the first ASTORE's input.
    (define astore (pvector-ref insns 1))
    (check-equal? (VfInsn-op astore) 'ASTORE)
    (check-equal? (pvector-ref (VfInsn-inputs astore) 0)
                  (pvector-ref (VfInsn-outputs first-insn) 0)
                  "ASTORE's first input is the exception-ref producer's output"))

  (test-case "non-handler blocks do not carry a synthetic exception-ref producer"
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L0")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IRETURN))
                 #:desc "(I)I"))
    (define cfg (jvm-method->cfg m))
    (for ([kv (in-ordered-map (Cfg-blocks cfg))])
      (define insns (CfgBlock-insns (cdr kv)))
      (for ([insn (in-pvector insns)])
        (check-false (eq? (VfInsn-op insn) 'java/exception-ref)
                     "no block should synthesize an exception-ref in a try-free method"))))

  (test-case "Cfg.info publishes 'java/block-order matching graph vertex count"
    ;; Three blocks (entry cond / then / else), each must appear exactly
    ;; once in block-order and every entry must be a BlockId that exists
    ;; in Cfg.blocks.  Downstream passes rely on this as the canonical
    ;; linearisation of the method's CFG.
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
    (define order (ordered-map-ref (Cfg-info cfg) 'java/block-order #f))
    (check-not-false order "Cfg.info should carry 'java/block-order")
    (check-equal? (pvector-length order) 3)
    ;; Entry block must be the first element.
    (check-equal? (pvector-ref order 0) (Cfg-entry cfg))
    ;; Every listed BlockId resolves in Cfg.blocks.
    (for ([bid (in-pvector order)])
      (check-pred BlockId? bid)
      (check-not-false (ordered-map-ref (Cfg-blocks cfg) bid #f)
                       "block-order entry must exist in Cfg.blocks"))
    ;; No duplicates.
    (define seen (for/fold ([s (ordered-map-empty block-id-compare)])
                           ([bid (in-pvector order)])
                   (ordered-map-set s bid #t)))
    (check-equal? (ordered-map-count seen) (pvector-length order)
                  "block-order should list each BlockId exactly once"))

  (test-case "method without try/catch has no exception-table key"
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L0")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IRETURN))
                 #:desc "(I)I"))
    (define cfg (jvm-method->cfg m))
    (check-equal? (ordered-map-ref (Cfg-info cfg) 'java/exception-table #f)
                  #f)
    ;; And no block carries covering-handlers.
    (for ([kv (in-ordered-map (Cfg-blocks cfg))])
      (check-equal? (ordered-map-ref (CfgBlock-info (cdr kv))
                                     'java/covering-handlers #f)
                    #f))))
