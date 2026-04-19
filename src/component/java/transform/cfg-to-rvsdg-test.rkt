#lang racket/base

;; ============================================================
;; Tests: SSA CFG → RVSDG (M4, initial scope)
;; ============================================================

(require rackunit
         racket/runtime-path
         racket/list
         "jvm-to-cfg.rkt"
         "ssa-construct.rkt"
         "cfg-to-rvsdg.rkt"
         "../../../frontend/java/reader.rkt"
         "../../../kernel/ir/jvm/types.rkt"
         "../../../kernel/ir/rvsdg/rvsdg.rkt"
         (except-in "../../../kernel/data/data.rkt" integer-compare))

(define-runtime-path fixture-class-transform
  "../../../../test/integration/ClassTransform.dat")

(module+ test
  (define (mk-insn op . args) (JvmInsn op args))
  (define (mk-method insns #:name [name "m"] #:desc [desc "()V"])
    (JvmMethod name desc 0 0 0 insns '() '() '() '() '() '()))

  (define (compile-method m)
    (cfg->rvsdg (jvm-cfg->ssa (jvm-method->cfg m))))

  (define (region-of lam) (Lambda-region lam))

  (define (region-node-count r)
    (ordered-map-count (Region-node->value r)))

  (define (node-ops r)
    (for/list ([kv (in-ordered-map (Region-node->value r))])
      (define v (cdr kv))
      (cond
        [(Simple? v) (Simple-op v)]
        [(Throw? v)  'throw]
        [else        'unknown])))

  ;; ----- RETURN-only method -----
  (test-case "RETURN-only"
    (define lam
      (compile-method (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L0")
                                       (mk-insn 'RETURN)))))
    (check-pred Lambda? lam)
    (define r (region-of lam))
    ;; Exactly: 'param, 'return.
    (check-equal? (node-ops r) '(param return)))

  ;; ----- ICONST_1 + IRETURN -----
  (test-case "ICONST_1 + IRETURN produces const → return"
    (define lam
      (compile-method
        (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L0")
                         (mk-insn 'ICONST_1)
                         (mk-insn 'IRETURN))
                   #:desc "()I")))
    (define r (region-of lam))
    ;; The ICONST_1 VfInsn has input `1` (a literal), which our lowering
    ;; materialises as a separate `(const 1)` node feeding into the
    ;; ICONST_1 op node.
    (check-equal? (node-ops r) '(param (const 1) ICONST_1 return)))

  ;; ----- ILOAD 0 + IRETURN -----
  (test-case "Parameter flows through ILOAD to return"
    (define lam
      (compile-method
        (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L0")
                         (mk-insn 'ILOAD 0)
                         (mk-insn 'IRETURN))
                   #:desc "(I)I")))
    (define r (region-of lam))
    (check-equal? (node-ops r) '(param ILOAD return))
    ;; Exactly two wires: param→ILOAD, ILOAD→return.
    (check-equal? (ordered-map-count (Region-wire->input r)) 2))

  ;; ----- Literal inputs become const nodes -----
  (test-case "BIPUSH integer literal spawns const node"
    (define lam
      (compile-method
        (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L0")
                         (mk-insn 'BIPUSH 42)
                         (mk-insn 'IRETURN))
                   #:desc "()I")))
    (define r (region-of lam))
    ;; param, const (for 42), BIPUSH, return
    (define ops (node-ops r))
    (check-equal? (length ops) 4)
    (check-true (equal? (car ops) 'param))
    (check-true (equal? (last ops) 'return))
    (check-true (for/or ([o (in-list ops)])
                  (and (list? o) (equal? (car o) 'const)))))

  ;; ----- IADD of locals -----
  (test-case "IADD of two locals wires correctly"
    (define lam
      (compile-method
        (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L0")
                         (mk-insn 'ILOAD 0)
                         (mk-insn 'ILOAD 1)
                         (mk-insn 'IADD)
                         (mk-insn 'IRETURN))
                   #:desc "(II)I")))
    (define r (region-of lam))
    (check-equal? (node-ops r) '(param ILOAD ILOAD IADD return)))

  ;; ----- Gamma recovery: IF-join -----
  (test-case "IF-join lowers to a Gamma node"
    ;; if (arg0) { local0 = 1; } else { local0 = 2; } return local0;
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L0")
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
    (define lam (compile-method m))
    (check-pred Lambda? lam)
    (define r (region-of lam))
    ;; The parent region must contain a Gamma node.
    (define gamma-nid
      (for/or ([kv (in-ordered-map (Region-node->value r))])
        (and (Gamma? (cdr kv)) (car kv))))
    (check-not-false gamma-nid)
    ;; Context pruning: both branches only write locally-defined vars,
    ;; so the Gamma should have just the predicate input (no context).
    (define gamma-in-info (ordered-map-ref (Region-node->input r) gamma-nid))
    (check-equal? (cdr gamma-in-info) 1
                  "Gamma input count should be 1 (predicate only) after escape pruning")
    ;; And must finish with a return.
    (check-not-false (memq 'return (node-ops r))))

  ;; ----- Gamma recovery: multi-block arms -----
  (test-case "Gamma lowers with multi-block then-arm"
    ;; if (arg0) { t = 1; slot0 = 2; } else { slot0 = 3; } return slot0;
    ;; Forces the then-arm to span two blocks via an intermediate label.
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L0")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IFEQ "L_ELSE")
                       (mk-insn 'CUTIEDENG-LABEL "L_THEN1")
                       (mk-insn 'ICONST_1)
                       (mk-insn 'ISTORE 1)
                       (mk-insn 'GOTO "L_THEN2")
                       (mk-insn 'CUTIEDENG-LABEL "L_THEN2")
                       (mk-insn 'ICONST_2)
                       (mk-insn 'ISTORE 0)
                       (mk-insn 'GOTO "L_JOIN")
                       (mk-insn 'CUTIEDENG-LABEL "L_ELSE")
                       (mk-insn 'ICONST_3)
                       (mk-insn 'ISTORE 0)
                       (mk-insn 'CUTIEDENG-LABEL "L_JOIN")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IRETURN))
                 #:desc "(I)I"))
    (define lam (compile-method m))
    (check-pred Lambda? lam)
    (define r (region-of lam))
    ;; Parent region still has exactly one Gamma node.
    (define gamma
      (for/or ([kv (in-ordered-map (Region-node->value r))])
        (and (Gamma? (cdr kv)) (cdr kv))))
    (check-pred Gamma? gamma)
    ;; The multi-block arm lives in Gamma-regions index 1: IFEQ's
    ;; fallthrough (predicate falsy) is the else-arm of Term:cond,
    ;; which maps to the second sub-region.  It should contain
    ;; BOTH ICONST_1 and ICONST_2 (one per source block).
    (define fallthrough-region (cadr (Gamma-regions gamma)))
    (define fallthrough-ops
      (for/list ([kv (in-ordered-map (Region-node->value fallthrough-region))])
        (define v (cdr kv))
        (cond [(Simple? v) (Simple-op v)] [else 'other])))
    (check-not-false (memq 'ICONST_1 fallthrough-ops))
    (check-not-false (memq 'ICONST_2 fallthrough-ops))
    (check-not-false (memq 'ISTORE fallthrough-ops))
    ;; And the parent must still terminate with a return.
    (check-not-false (memq 'return (node-ops r))))

  ;; ----- Gamma recovery: nested if-inside-if -----
  (test-case "Gamma lowers with nested if inside then-arm"
    ;; if (arg0) {
    ;;   if (arg1) { slot0 = 1; } else { slot0 = 2; }
    ;; } else {
    ;;   slot0 = 3;
    ;; }
    ;; return slot0;
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L0")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IFEQ "L_OUTER_ELSE")
                       (mk-insn 'CUTIEDENG-LABEL "L_OUTER_THEN")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'IFEQ "L_INNER_ELSE")
                       (mk-insn 'CUTIEDENG-LABEL "L_INNER_THEN")
                       (mk-insn 'ICONST_1)
                       (mk-insn 'ISTORE 0)
                       (mk-insn 'GOTO "L_INNER_JOIN")
                       (mk-insn 'CUTIEDENG-LABEL "L_INNER_ELSE")
                       (mk-insn 'ICONST_2)
                       (mk-insn 'ISTORE 0)
                       (mk-insn 'CUTIEDENG-LABEL "L_INNER_JOIN")
                       (mk-insn 'GOTO "L_OUTER_JOIN")
                       (mk-insn 'CUTIEDENG-LABEL "L_OUTER_ELSE")
                       (mk-insn 'ICONST_3)
                       (mk-insn 'ISTORE 0)
                       (mk-insn 'CUTIEDENG-LABEL "L_OUTER_JOIN")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IRETURN))
                 #:desc "(II)I"))
    (define lam (compile-method m))
    (check-pred Lambda? lam)
    (define r (region-of lam))
    ;; Parent region has exactly one outer Gamma.
    (define outer-gammas
      (for/list ([kv (in-ordered-map (Region-node->value r))]
                 #:when (Gamma? (cdr kv)))
        (cdr kv)))
    (check-equal? (length outer-gammas) 1
                  "parent region should contain exactly one Gamma")
    (define outer-gamma (car outer-gammas))
    ;; One of the outer Gamma's sub-regions must itself contain an
    ;; inner Gamma (the nested if).  Regardless of which arm IFEQ
    ;; mapped the outer-then/else onto, *some* sub-region has a
    ;; Gamma node inside.
    (define nested-gamma-count
      (for/sum ([sub (in-list (Gamma-regions outer-gamma))])
        (for/sum ([kv (in-ordered-map (Region-node->value sub))])
          (if (Gamma? (cdr kv)) 1 0))))
    (check-equal? nested-gamma-count 1
                  "exactly one of the outer Gamma's sub-regions should contain an inner Gamma")
    ;; Parent region finishes with a return.
    (check-not-false (memq 'return (node-ops r))))

  ;; ----- Theta recovery: simple while-loop -----
  (test-case "while-loop lowers to a Theta node"
    ;; slot0 = 0; while (slot1 != 0) { slot0 = slot0 + 1; } return slot0;
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L_ENTRY")
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
    (define lam (compile-method m))
    (check-pred Lambda? lam)
    (define r (region-of lam))
    ;; Parent region contains a Theta node.
    (define has-theta?
      (for/or ([kv (in-ordered-map (Region-node->value r))])
        (Theta? (cdr kv))))
    (check-true has-theta?)
    ;; IFEQ body is on the else arm, so Theta polarity-flip inserts
    ;; a `Simple 'not` inside the theta's region.
    (define theta
      (for/or ([kv (in-ordered-map (Region-node->value r))])
        (and (Theta? (cdr kv)) (cdr kv))))
    (define theta-sub (Theta-region theta))
    (define has-not?
      (for/or ([kv (in-ordered-map (Region-node->value theta-sub))])
        (define v (cdr kv))
        (and (Simple? v) (equal? (Simple-op v) 'not))))
    (check-true has-not?)
    ;; Parent must still terminate with a return.
    (check-not-false (memq 'return (node-ops r))))

  ;; ----- Theta recovery: multi-block body -----
  (test-case "while-loop lowers to a Theta node with multi-block body"
    ;; slot0 = 0;
    ;; while (slot1 != 0) {
    ;;   slot0 = slot0 + 1;     ;; body_a
    ;;   slot1 = slot1 - 1;     ;; body_b (latch, jumps back to head)
    ;; }
    ;; return slot0;
    ;;
    ;; An explicit GOTO between the two body portions forces a block
    ;; split, so the loop body spans body_a and body_b — the body-arm
    ;; must walk a Term:jump chain to reach the latch.
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L_ENTRY")
                       (mk-insn 'ICONST_0)
                       (mk-insn 'ISTORE 0)
                       (mk-insn 'CUTIEDENG-LABEL "L_HEAD")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'IFEQ "L_EXIT")
                       (mk-insn 'CUTIEDENG-LABEL "L_BODY_A")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'ICONST_1)
                       (mk-insn 'IADD)
                       (mk-insn 'ISTORE 0)
                       (mk-insn 'GOTO "L_BODY_B")
                       (mk-insn 'CUTIEDENG-LABEL "L_BODY_B")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'ICONST_1)
                       (mk-insn 'ISUB)
                       (mk-insn 'ISTORE 1)
                       (mk-insn 'GOTO "L_HEAD")
                       (mk-insn 'CUTIEDENG-LABEL "L_EXIT")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IRETURN))
                 #:desc "(I)I"))
    (define lam (compile-method m))
    (check-pred Lambda? lam)
    (define r (region-of lam))
    ;; Parent region contains exactly one Theta node.
    (define thetas
      (for/list ([kv (in-ordered-map (Region-node->value r))]
                 #:when (Theta? (cdr kv)))
        (cdr kv)))
    (check-equal? (length thetas) 1
                  "parent region should contain exactly one Theta")
    (define theta-sub (Theta-region (car thetas)))
    ;; Both body blocks' arithmetic ops must appear inside the
    ;; Theta's sub-region: IADD from body_a and ISUB from body_b.
    (define sub-ops
      (for/list ([kv (in-ordered-map (Region-node->value theta-sub))])
        (define v (cdr kv))
        (cond [(Simple? v) (Simple-op v)] [else 'other])))
    (check-not-false (memq 'IADD sub-ops)
                     "IADD from body_a missing from Theta sub-region")
    (check-not-false (memq 'ISUB sub-ops)
                     "ISUB from body_b missing from Theta sub-region")
    ;; Parent must still terminate with a return.
    (check-not-false (memq 'return (node-ops r))))

  ;; ----- Theta recovery: body containing an inner if -----
  (test-case "while-loop body may contain an inner Gamma"
    ;; slot0 = 0;
    ;; while (slot1 != 0) {
    ;;   if (slot2 != 0) { slot0 += 1; } else { slot0 += 2; }
    ;;   slot1 -= 1;            ;; latch
    ;; }
    ;; return slot0;
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L_ENTRY")
                       (mk-insn 'ICONST_0)
                       (mk-insn 'ISTORE 0)
                       (mk-insn 'CUTIEDENG-LABEL "L_HEAD")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'IFEQ "L_EXIT")
                       (mk-insn 'CUTIEDENG-LABEL "L_BODY_COND")
                       (mk-insn 'ILOAD 2)
                       (mk-insn 'IFEQ "L_BODY_ELSE")
                       (mk-insn 'CUTIEDENG-LABEL "L_BODY_THEN")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'ICONST_1)
                       (mk-insn 'IADD)
                       (mk-insn 'ISTORE 0)
                       (mk-insn 'GOTO "L_BODY_JOIN")
                       (mk-insn 'CUTIEDENG-LABEL "L_BODY_ELSE")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'ICONST_2)
                       (mk-insn 'IADD)
                       (mk-insn 'ISTORE 0)
                       (mk-insn 'CUTIEDENG-LABEL "L_BODY_JOIN")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'ICONST_1)
                       (mk-insn 'ISUB)
                       (mk-insn 'ISTORE 1)
                       (mk-insn 'GOTO "L_HEAD")
                       (mk-insn 'CUTIEDENG-LABEL "L_EXIT")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IRETURN))
                 #:desc "(II)I"))
    (define lam (compile-method m))
    (check-pred Lambda? lam)
    (define r (region-of lam))
    ;; Parent region contains exactly one Theta node.
    (define thetas
      (for/list ([kv (in-ordered-map (Region-node->value r))]
                 #:when (Theta? (cdr kv)))
        (cdr kv)))
    (check-equal? (length thetas) 1
                  "parent region should contain exactly one Theta")
    (define theta-sub (Theta-region (car thetas)))
    ;; The Theta's sub-region must itself host exactly one inner
    ;; Gamma (the body if/else).  That inner Gamma covers the IADD
    ;; from each arm; ISUB from the latch block lives in the Theta
    ;; sub-region directly.
    (define inner-gammas
      (for/list ([kv (in-ordered-map (Region-node->value theta-sub))]
                 #:when (Gamma? (cdr kv)))
        (cdr kv)))
    (check-equal? (length inner-gammas) 1
                  "Theta body should contain exactly one inner Gamma")
    (define sub-ops
      (for/list ([kv (in-ordered-map (Region-node->value theta-sub))])
        (define v (cdr kv))
        (cond [(Simple? v) (Simple-op v)] [else 'other])))
    (check-not-false (memq 'ISUB sub-ops)
                     "ISUB from latch missing from Theta sub-region")
    ;; Parent must still terminate with a return.
    (check-not-false (memq 'return (node-ops r))))

  ;; ----- Theta recovery: nested loops -----
  (test-case "nested while-loops lower to nested Theta nodes"
    ;; slot0 = 0;
    ;; while (slot1 != 0) {         ;; outer header
    ;;   slot2 = 3;
    ;;   while (slot2 != 0) {       ;; inner header
    ;;     slot0 += 1;
    ;;     slot2 -= 1;              ;; inner latch
    ;;   }
    ;;   slot1 -= 1;                ;; outer latch
    ;; }
    ;; return slot0;
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L_ENTRY")
                       (mk-insn 'ICONST_0)
                       (mk-insn 'ISTORE 0)
                       (mk-insn 'CUTIEDENG-LABEL "L_OUTER_HEAD")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'IFEQ "L_OUTER_EXIT")
                       (mk-insn 'CUTIEDENG-LABEL "L_OUTER_BODY")
                       (mk-insn 'ICONST_3)
                       (mk-insn 'ISTORE 2)
                       (mk-insn 'CUTIEDENG-LABEL "L_INNER_HEAD")
                       (mk-insn 'ILOAD 2)
                       (mk-insn 'IFEQ "L_INNER_EXIT")
                       (mk-insn 'CUTIEDENG-LABEL "L_INNER_BODY")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'ICONST_1)
                       (mk-insn 'IADD)
                       (mk-insn 'ISTORE 0)
                       (mk-insn 'ILOAD 2)
                       (mk-insn 'ICONST_1)
                       (mk-insn 'ISUB)
                       (mk-insn 'ISTORE 2)
                       (mk-insn 'GOTO "L_INNER_HEAD")
                       (mk-insn 'CUTIEDENG-LABEL "L_INNER_EXIT")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'ICONST_1)
                       (mk-insn 'ISUB)
                       (mk-insn 'ISTORE 1)
                       (mk-insn 'GOTO "L_OUTER_HEAD")
                       (mk-insn 'CUTIEDENG-LABEL "L_OUTER_EXIT")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IRETURN))
                 #:desc "(II)I"))
    (define lam (compile-method m))
    (check-pred Lambda? lam)
    (define r (region-of lam))
    ;; Parent region contains exactly one (outer) Theta node.
    (define outer-thetas
      (for/list ([kv (in-ordered-map (Region-node->value r))]
                 #:when (Theta? (cdr kv)))
        (cdr kv)))
    (check-equal? (length outer-thetas) 1
                  "parent region should contain exactly one outer Theta")
    (define outer-sub (Theta-region (car outer-thetas)))
    ;; Outer Theta's sub-region hosts exactly one inner Theta plus
    ;; the outer-latch ISUB.
    (define inner-thetas
      (for/list ([kv (in-ordered-map (Region-node->value outer-sub))]
                 #:when (Theta? (cdr kv)))
        (cdr kv)))
    (check-equal? (length inner-thetas) 1
                  "outer Theta body should contain exactly one inner Theta")
    (define outer-sub-ops
      (for/list ([kv (in-ordered-map (Region-node->value outer-sub))])
        (define v (cdr kv))
        (cond [(Simple? v) (Simple-op v)] [else 'other])))
    (check-not-false (memq 'ISUB outer-sub-ops)
                     "outer-latch ISUB missing from outer Theta sub-region")
    ;; Inner Theta's sub-region hosts the IADD (slot0 += 1) and the
    ;; inner-latch ISUB (slot2 -= 1).
    (define inner-sub (Theta-region (car inner-thetas)))
    (define inner-sub-ops
      (for/list ([kv (in-ordered-map (Region-node->value inner-sub))])
        (define v (cdr kv))
        (cond [(Simple? v) (Simple-op v)] [else 'other])))
    (check-not-false (memq 'IADD inner-sub-ops)
                     "IADD missing from inner Theta sub-region")
    (check-not-false (memq 'ISUB inner-sub-ops)
                     "inner-latch ISUB missing from inner Theta sub-region")
    ;; Parent must still terminate with a return.
    (check-not-false (memq 'return (node-ops r))))

  ;; ----- fixture init (linear jump chain) -----
  (test-case "fixture: init method translates to RVSDG"
    (define klass (read-jvm-class-file fixture-class-transform))
    (define init-m
      (for/or ([mth (JvmClass-methods klass)])
        (and (equal? (JvmMethod-name mth) "init") mth)))
    (check-not-false init-m)
    (define lam (compile-method init-m))
    (check-pred Lambda? lam)
    (define r (region-of lam))
    ;; Should include GETSTATIC, INVOKESTATIC, and a return.
    (define ops (node-ops r))
    (check-not-false (memq 'GETSTATIC ops))
    (check-not-false (memq 'INVOKESTATIC ops))
    (check-not-false (memq 'return ops))))
