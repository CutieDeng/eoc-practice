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

  ;; ----- Gamma early-exit: both arms terminate -----
  (test-case "if (c) return X; else return Y; lowers to terminal Gamma"
    ;; int m(int x) {
    ;;   if (x == 0) return 42;
    ;;   else        return x + 1;
    ;; }
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L_ENTRY")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IFEQ "L_THEN")
                       (mk-insn 'CUTIEDENG-LABEL "L_ELSE")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'ICONST_1)
                       (mk-insn 'IADD)
                       (mk-insn 'IRETURN)
                       (mk-insn 'CUTIEDENG-LABEL "L_THEN")
                       (mk-insn 'BIPUSH 42)
                       (mk-insn 'IRETURN))
                 #:desc "(I)I"))
    (define lam (compile-method m))
    (check-pred Lambda? lam)
    (define r (region-of lam))
    ;; Parent region contains exactly one Gamma node.
    (define gammas
      (for/list ([kv (in-ordered-map (Region-node->value r))]
                 #:when (Gamma? (cdr kv)))
        (cdr kv)))
    (check-equal? (length gammas) 1
                  "parent region should contain exactly one terminal Gamma")
    ;; Parent region must NOT install its own top-level return; the
    ;; Gamma's two sub-regions each terminate internally.
    (check-false (memq 'return (node-ops r))
                 "terminal Gamma should not have a top-level return alongside it")
    ;; Gamma has two sub-regions; each hosts its own Simple 'return.
    (define gamma (car gammas))
    (define sub-regions (Gamma-regions gamma))
    (check-equal? (length sub-regions) 2)
    (for ([sub (in-list sub-regions)]
          [which (in-list '(then else))])
      (define ops
        (for/list ([kv (in-ordered-map (Region-node->value sub))])
          (define v (cdr kv))
          (cond [(Simple? v) (Simple-op v)] [else 'other])))
      (check-not-false (memq 'return ops)
                       (format "~a sub-region missing 'return sink" which))))

  ;; ----- Gamma early-exit: asymmetric (one arm exits) -----
  (test-case "if (c) return X; <then continue with an if/else>"
    ;; int m(int x, int y) {
    ;;   if (x == 0) return 42;    // early exit
    ;;   if (y <= 0) y = y * 3;    // inner diamond
    ;;   else        y = y * 2;
    ;;   return y;
    ;; }
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L_ENTRY")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IFEQ "L_EARLY")
                       (mk-insn 'CUTIEDENG-LABEL "L_INNER")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'IFLE "L_NEG")
                       (mk-insn 'CUTIEDENG-LABEL "L_POS")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'ICONST_2)
                       (mk-insn 'IMUL)
                       (mk-insn 'ISTORE 1)
                       (mk-insn 'GOTO "L_END")
                       (mk-insn 'CUTIEDENG-LABEL "L_NEG")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'ICONST_3)
                       (mk-insn 'IMUL)
                       (mk-insn 'ISTORE 1)
                       (mk-insn 'CUTIEDENG-LABEL "L_END")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'IRETURN)
                       (mk-insn 'CUTIEDENG-LABEL "L_EARLY")
                       (mk-insn 'BIPUSH 42)
                       (mk-insn 'IRETURN))
                 #:desc "(II)I"))
    (define lam (compile-method m))
    (check-pred Lambda? lam)
    (define r (region-of lam))
    ;; Parent region contains exactly two Gamma nodes:
    ;;   - the outer asymmetric early-exit Gamma (0 outputs)
    ;;   - the inner if/else diamond for y*2 vs y*3
    (define gammas
      (for/list ([kv (in-ordered-map (Region-node->value r))]
                 #:when (Gamma? (cdr kv)))
        (cdr kv)))
    (check-equal? (length gammas) 2
                  "parent region should contain two Gamma nodes")
    ;; Exactly one of the two Gammas has an exit-arm sub-region
    ;; hosting a 'return (the asymmetric early-exit Gamma).
    (define (sub-has-return? sub)
      (for/or ([kv (in-ordered-map (Region-node->value sub))])
        (define v (cdr kv))
        (and (Simple? v) (eq? (Simple-op v) 'return))))
    (define (gamma-has-any-exit-arm? g)
      (for/or ([sub (in-list (Gamma-regions g))])
        (sub-has-return? sub)))
    (define exit-gammas (filter gamma-has-any-exit-arm? gammas))
    (check-equal? (length exit-gammas) 1
                  "exactly one Gamma should carry an early-exit sub-region")
    ;; Parent region must still have its own top-level 'return
    ;; materialised from L_END's IRETURN (after the inner Gamma
    ;; merges y).
    (check-not-false (memq 'return (node-ops r))
                     "parent region should still have a top-level return from L_END"))

  ;; ----- Gamma early-exit: multi-block arms (terminal Gamma) -----
  (test-case "multi-block exit arms both end in ret → terminal Gamma"
    ;; int m(int x, int y) {
    ;;   if (x == 0) {                            // L_THEN arm (multi-block)
    ;;     if (y > 0) return y;                   //   inner cond, both branches ret
    ;;     else       return -y;
    ;;   } else {                                 // L_ELSE arm (multi-block)
    ;;     if (y > 0) return y * 2;               //   inner cond, both branches ret
    ;;     else       return y * 3;
    ;;   }
    ;; }
    ;;
    ;; Both outer arms' reach-sets contain an inner Term:cond plus two
    ;; ret blocks.  Reach-sets are disjoint (no shared join) → the
    ;; outer cond must lower as a terminal Gamma whose two sub-regions
    ;; each host an inner terminal Gamma and install their own
    ;; returns internally.
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L_ENTRY")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IFEQ "L_THEN")
                       (mk-insn 'CUTIEDENG-LABEL "L_ELSE")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'IFLE "L_ELSE_NEG")
                       (mk-insn 'CUTIEDENG-LABEL "L_ELSE_POS")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'ICONST_2)
                       (mk-insn 'IMUL)
                       (mk-insn 'IRETURN)
                       (mk-insn 'CUTIEDENG-LABEL "L_ELSE_NEG")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'ICONST_3)
                       (mk-insn 'IMUL)
                       (mk-insn 'IRETURN)
                       (mk-insn 'CUTIEDENG-LABEL "L_THEN")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'IFLE "L_THEN_NEG")
                       (mk-insn 'CUTIEDENG-LABEL "L_THEN_POS")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'IRETURN)
                       (mk-insn 'CUTIEDENG-LABEL "L_THEN_NEG")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'INEG)
                       (mk-insn 'IRETURN))
                 #:desc "(II)I"))
    (define lam (compile-method m))
    (check-pred Lambda? lam)
    (define r (region-of lam))
    ;; Parent region holds exactly one outer Gamma.
    (define gammas
      (for/list ([kv (in-ordered-map (Region-node->value r))]
                 #:when (Gamma? (cdr kv)))
        (cdr kv)))
    (check-equal? (length gammas) 1
                  "parent region should contain exactly one outer Gamma")
    ;; No top-level return at the parent — both arms terminate inside.
    (check-false (memq 'return (node-ops r))
                 "multi-block terminal Gamma should not leak a top-level return")
    (define outer-gamma (car gammas))
    (define sub-regions (Gamma-regions outer-gamma))
    (check-equal? (length sub-regions) 2)
    ;; Each outer sub-region must itself host exactly one inner Gamma
    ;; (the nested if/else) and at least one 'return sink reachable
    ;; through one of its own sub-regions.
    (for ([sub (in-list sub-regions)]
          [which (in-list '(then else))])
      (define inner-gammas
        (for/list ([kv (in-ordered-map (Region-node->value sub))]
                   #:when (Gamma? (cdr kv)))
          (cdr kv)))
      (check-equal? (length inner-gammas) 1
                    (format "outer ~a sub-region should host one inner Gamma" which))
      (define inner-sub-regions (Gamma-regions (car inner-gammas)))
      (define inner-return-count
        (for/sum ([ir (in-list inner-sub-regions)])
          (for/sum ([kv (in-ordered-map (Region-node->value ir))])
            (define v (cdr kv))
            (if (and (Simple? v) (eq? (Simple-op v) 'return)) 1 0))))
      (check-equal? inner-return-count 2
                    (format "inner Gamma in ~a arm should hold two 'return sinks"
                            which))))

  ;; ----- Gamma early-exit: inner single-block early-exit inside
  ;; an outer standard diamond arm -----
  (test-case "inner single-block early-exit inside outer diamond arm"
    ;; int m(int x, int y) {
    ;;   if (y > 0) {
    ;;     if (x == 0) return 42;      // inner asymmetric early-exit
    ;;     y = y * 2;                  //   continues inside outer-then
    ;;   } else {
    ;;     y = 10;                     // outer-else
    ;;   }
    ;;   return y;
    ;; }
    ;;
    ;; Outer cond is a standard diamond whose join is L_END (both outer
    ;; arms converge through either a ret/throw or a fallthrough that
    ;; reaches L_END).  The inner cond inside the outer-then arm has
    ;; ONE single-block early-exit leaf (L_RET42) and ONE continuing
    ;; arm (L_CONT) that reaches the outer join.  The outer walk must
    ;; skip past the inner exit to find L_END, and the inner cond
    ;; lowers as an asymmetric early-exit Gamma inside the outer-then
    ;; sub-region.  Parent region keeps its own top-level return for
    ;; L_END's IRETURN of the merged y.
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L_ENTRY")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'IFLE "L_ELSE")
                       (mk-insn 'CUTIEDENG-LABEL "L_OUTER_THEN")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IFNE "L_CONT")
                       (mk-insn 'CUTIEDENG-LABEL "L_RET42")
                       (mk-insn 'BIPUSH 42)
                       (mk-insn 'IRETURN)
                       (mk-insn 'CUTIEDENG-LABEL "L_CONT")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'ICONST_2)
                       (mk-insn 'IMUL)
                       (mk-insn 'ISTORE 1)
                       (mk-insn 'GOTO "L_END")
                       (mk-insn 'CUTIEDENG-LABEL "L_ELSE")
                       (mk-insn 'BIPUSH 10)
                       (mk-insn 'ISTORE 1)
                       (mk-insn 'CUTIEDENG-LABEL "L_END")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'IRETURN))
                 #:desc "(II)I"))
    (define lam (compile-method m))
    (check-pred Lambda? lam)
    (define r (region-of lam))
    ;; Parent region: exactly one outer Gamma + top-level return from
    ;; L_END.  The inner Gamma lives inside the outer-then sub-region,
    ;; not at the parent.
    (define gammas
      (for/list ([kv (in-ordered-map (Region-node->value r))]
                 #:when (Gamma? (cdr kv)))
        (cdr kv)))
    (check-equal? (length gammas) 1
                  "parent region should contain exactly one outer Gamma")
    (check-not-false (memq 'return (node-ops r))
                     "parent region should keep its top-level return from L_END")
    ;; Outer Gamma has two sub-regions; exactly one of them hosts an
    ;; inner Gamma, and that inner Gamma has exactly one exit sub-
    ;; region with a 'return sink (asymmetric early-exit shape).
    (define outer (car gammas))
    (define sub-regions (Gamma-regions outer))
    (check-equal? (length sub-regions) 2)
    (define (region-inner-gammas sub)
      (for/list ([kv (in-ordered-map (Region-node->value sub))]
                 #:when (Gamma? (cdr kv)))
        (cdr kv)))
    (define inner-counts (map (lambda (s) (length (region-inner-gammas s))) sub-regions))
    (check-equal? (sort inner-counts <) '(0 1)
                  "exactly one outer sub-region should host the inner Gamma")
    (define inner-g
      (for/or ([sub (in-list sub-regions)])
        (define igs (region-inner-gammas sub))
        (and (pair? igs) (car igs))))
    (check-not-false inner-g "inner Gamma not found")
    (define inner-subs (Gamma-regions inner-g))
    (check-equal? (length inner-subs) 2)
    (define inner-return-count
      (for/sum ([isub (in-list inner-subs)])
        (for/sum ([kv (in-ordered-map (Region-node->value isub))])
          (define v (cdr kv))
          (if (and (Simple? v) (eq? (Simple-op v) 'return)) 1 0))))
    (check-equal? inner-return-count 1
                  "inner asymmetric Gamma should host exactly one 'return sink"))

  ;; ----- Gamma early-exit: inner MULTI-block early-exit inside
  ;; an outer standard diamond arm -----
  (test-case "inner multi-block early-exit inside outer diamond arm"
    ;; int m(int x, int y) {
    ;;   if (y > 0) {
    ;;     if (x == 0) {
    ;;       y = 99;
    ;;       return 42;                 // inner MULTI-block exit
    ;;     }                            //   (L_EXIT_ENTRY -> L_EXIT_RET)
    ;;     y = y * 2;
    ;;   } else {
    ;;     y = 10;
    ;;   }
    ;;   return y;
    ;; }
    ;;
    ;; Differs from the single-block variant: the inner cond's exit
    ;; arm spans two blocks rather than one, so the V1 `terminal-
    ;; block?` check cannot classify it.  The outer `translate-gamma`
    ;; now installs a `current-shared-set` before walking the arms so
    ;; `arm-advance` can treat the two-block exit subtree as an exit
    ;; leaf and skip past it to reach the outer join.  Inside the
    ;; outer-then sub-region, the inner Term:cond lowers via the new
    ;; multi-block asymmetric path (stop-bid ∈ exactly one reach-set)
    ;; as an asymmetric early-exit Gamma whose exit sub-region spans
    ;; both exit blocks.
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L_ENTRY")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'IFLE "L_ELSE")
                       (mk-insn 'CUTIEDENG-LABEL "L_OUTER_THEN")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IFNE "L_CONT")
                       (mk-insn 'CUTIEDENG-LABEL "L_EXIT_ENTRY")
                       (mk-insn 'BIPUSH 99)
                       (mk-insn 'ISTORE 1)
                       (mk-insn 'GOTO "L_EXIT_RET")
                       (mk-insn 'CUTIEDENG-LABEL "L_EXIT_RET")
                       (mk-insn 'BIPUSH 42)
                       (mk-insn 'IRETURN)
                       (mk-insn 'CUTIEDENG-LABEL "L_CONT")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'ICONST_2)
                       (mk-insn 'IMUL)
                       (mk-insn 'ISTORE 1)
                       (mk-insn 'GOTO "L_END")
                       (mk-insn 'CUTIEDENG-LABEL "L_ELSE")
                       (mk-insn 'BIPUSH 10)
                       (mk-insn 'ISTORE 1)
                       (mk-insn 'CUTIEDENG-LABEL "L_END")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'IRETURN))
                 #:desc "(II)I"))
    (define lam (compile-method m))
    (check-pred Lambda? lam)
    (define r (region-of lam))
    (define gammas
      (for/list ([kv (in-ordered-map (Region-node->value r))]
                 #:when (Gamma? (cdr kv)))
        (cdr kv)))
    (check-equal? (length gammas) 1
                  "parent region should contain exactly one outer Gamma")
    (check-not-false (memq 'return (node-ops r))
                     "parent region should keep its top-level return from L_END")
    (define outer (car gammas))
    (define sub-regions (Gamma-regions outer))
    (check-equal? (length sub-regions) 2)
    (define (region-inner-gammas sub)
      (for/list ([kv (in-ordered-map (Region-node->value sub))]
                 #:when (Gamma? (cdr kv)))
        (cdr kv)))
    (define inner-counts (map (lambda (s) (length (region-inner-gammas s))) sub-regions))
    (check-equal? (sort inner-counts <) '(0 1)
                  "exactly one outer sub-region should host the inner Gamma")
    (define inner-g
      (for/or ([sub (in-list sub-regions)])
        (define igs (region-inner-gammas sub))
        (and (pair? igs) (car igs))))
    (check-not-false inner-g "inner Gamma not found")
    (define inner-subs (Gamma-regions inner-g))
    (check-equal? (length inner-subs) 2)
    (define inner-return-count
      (for/sum ([isub (in-list inner-subs)])
        (for/sum ([kv (in-ordered-map (Region-node->value isub))])
          (define v (cdr kv))
          (if (and (Simple? v) (eq? (Simple-op v) 'return)) 1 0))))
    (check-equal? inner-return-count 1
                  "inner asymmetric Gamma should host exactly one 'return sink")
    ;; The exit sub-region must span both exit blocks: its body
    ;; should contain at least one ISTORE (from L_EXIT_ENTRY) plus
    ;; the return sink (from L_EXIT_RET).
    (define exit-sub
      (for/or ([isub (in-list inner-subs)])
        (define ops
          (for/list ([kv (in-ordered-map (Region-node->value isub))])
            (define v (cdr kv))
            (cond [(Simple? v) (Simple-op v)] [else 'other])))
        (and (memq 'return ops) isub)))
    (check-not-false exit-sub)
    (define exit-ops
      (for/list ([kv (in-ordered-map (Region-node->value exit-sub))])
        (define v (cdr kv))
        (cond [(Simple? v) (Simple-op v)] [else 'other])))
    (check-not-false (memq 'ISTORE exit-ops)
                     "multi-block exit sub-region should contain the first block's ISTORE"))

  ;; ----- early-exit inside Theta body: single-block exit arm -----
  (test-case "single-block early-exit inside Theta body"
    ;; while (slot1 != 0) {
    ;;   if (slot0 == 0) return 42;   // single-block early exit
    ;;   slot1 = slot1 - 1;
    ;; }
    ;; return slot0;
    ;;
    ;; The inner Term:cond sits inside the loop body; one arm is a
    ;; single block ending in Term:ret.  translate-segment (now with
    ;; scope-aware arm-reach-set) classifies the cond as asymmetric
    ;; and installs an early-exit Gamma directly inside the Theta's
    ;; body sub-region; the continue arm resumes toward the latch.
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L_ENTRY")
                       (mk-insn 'CUTIEDENG-LABEL "L_HEAD")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'IFEQ "L_EXIT")
                       (mk-insn 'CUTIEDENG-LABEL "L_BODY")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IFNE "L_CONTINUE")
                       (mk-insn 'CUTIEDENG-LABEL "L_EARLY_RET")
                       (mk-insn 'BIPUSH 42)
                       (mk-insn 'IRETURN)
                       (mk-insn 'CUTIEDENG-LABEL "L_CONTINUE")
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
    ;; Parent region has exactly one Theta and a final return.
    (define thetas
      (for/list ([kv (in-ordered-map (Region-node->value r))]
                 #:when (Theta? (cdr kv)))
        (cdr kv)))
    (check-equal? (length thetas) 1
                  "parent region should contain exactly one Theta")
    (check-not-false (memq 'return (node-ops r))
                     "parent region should terminate with a return")
    ;; The Theta's body must host exactly one inner Gamma (the early-
    ;; exit); that Gamma's two sub-regions contain exactly one 'return
    ;; sink in total.
    (define body (Theta-region (car thetas)))
    (define inner-gammas
      (for/list ([kv (in-ordered-map (Region-node->value body))]
                 #:when (Gamma? (cdr kv)))
        (cdr kv)))
    (check-equal? (length inner-gammas) 1
                  "Theta body should host exactly one early-exit Gamma")
    (define inner-subs (Gamma-regions (car inner-gammas)))
    (define inner-return-count
      (for/sum ([isub (in-list inner-subs)])
        (for/sum ([kv (in-ordered-map (Region-node->value isub))])
          (define v (cdr kv))
          (if (and (Simple? v) (eq? (Simple-op v) 'return)) 1 0))))
    (check-equal? inner-return-count 1
                  "inner Gamma should host exactly one 'return sink"))

  ;; ----- early-exit inside Theta body: multi-block exit arm -----
  (test-case "multi-block early-exit inside Theta body"
    ;; while (slot1 != 0) {
    ;;   if (slot0 == 0) {
    ;;     slot2 = 99;                // multi-block exit
    ;;     return 42;
    ;;   }
    ;;   slot1 = slot1 - 1;
    ;; }
    ;; return slot0;
    ;;
    ;; The exit arm now spans two blocks (L_EXIT_A -> L_EXIT_B).
    ;; The scoped arm-reach-set stops at the back-edge into L_HEAD
    ;; (outside body-blocks) and `reach-set-reaches-stop?` classifies
    ;; the continue arm by its Term:jump-to-header boundary terminator.
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L_ENTRY")
                       (mk-insn 'CUTIEDENG-LABEL "L_HEAD")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'IFEQ "L_EXIT")
                       (mk-insn 'CUTIEDENG-LABEL "L_BODY")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IFNE "L_CONTINUE")
                       (mk-insn 'CUTIEDENG-LABEL "L_EXIT_A")
                       (mk-insn 'BIPUSH 99)
                       (mk-insn 'ISTORE 2)
                       (mk-insn 'GOTO "L_EXIT_B")
                       (mk-insn 'CUTIEDENG-LABEL "L_EXIT_B")
                       (mk-insn 'BIPUSH 42)
                       (mk-insn 'IRETURN)
                       (mk-insn 'CUTIEDENG-LABEL "L_CONTINUE")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'ICONST_1)
                       (mk-insn 'ISUB)
                       (mk-insn 'ISTORE 1)
                       (mk-insn 'GOTO "L_HEAD")
                       (mk-insn 'CUTIEDENG-LABEL "L_EXIT")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IRETURN))
                 #:desc "(III)I"))
    (define lam (compile-method m))
    (check-pred Lambda? lam)
    (define r (region-of lam))
    (define thetas
      (for/list ([kv (in-ordered-map (Region-node->value r))]
                 #:when (Theta? (cdr kv)))
        (cdr kv)))
    (check-equal? (length thetas) 1
                  "parent region should contain exactly one Theta")
    (define body (Theta-region (car thetas)))
    (define inner-gammas
      (for/list ([kv (in-ordered-map (Region-node->value body))]
                 #:when (Gamma? (cdr kv)))
        (cdr kv)))
    (check-equal? (length inner-gammas) 1
                  "Theta body should host exactly one early-exit Gamma")
    (define inner-subs (Gamma-regions (car inner-gammas)))
    (define inner-return-count
      (for/sum ([isub (in-list inner-subs)])
        (for/sum ([kv (in-ordered-map (Region-node->value isub))])
          (define v (cdr kv))
          (if (and (Simple? v) (eq? (Simple-op v) 'return)) 1 0))))
    (check-equal? inner-return-count 1
                  "inner Gamma should host exactly one 'return sink")
    ;; The exit sub-region must span both exit blocks: the first
    ;; block's ISTORE (slot2 = 99) lives alongside the return sink.
    (define exit-sub
      (for/or ([isub (in-list inner-subs)])
        (define ops
          (for/list ([kv (in-ordered-map (Region-node->value isub))])
            (define v (cdr kv))
            (cond [(Simple? v) (Simple-op v)] [else 'other])))
        (and (memq 'return ops) isub)))
    (check-not-false exit-sub)
    (define exit-ops
      (for/list ([kv (in-ordered-map (Region-node->value exit-sub))])
        (define v (cdr kv))
        (cond [(Simple? v) (Simple-op v)] [else 'other])))
    (check-not-false (memq 'ISTORE exit-ops)
                     "multi-block exit sub-region should contain the first block's ISTORE"))

  ;; ----- multi-latch loop: 2-arm diamond, both arms distinct latches -----
  (test-case "2-latch diamond while-loop lowers to a Theta with a merge Gamma in its body"
    ;; while (slot1 != 0) {
    ;;   if (slot0 == 0) { slot1 = slot1 - 1; }   ; latch 1
    ;;   else            { slot1 = slot1 - 2; }   ; latch 2
    ;; }
    ;; return slot1;
    ;;
    ;; Two back-edges share the header L_HEAD: one from L_THEN and
    ;; one from L_ELSE.  The body-entry cond fans out to these two
    ;; latches, and `translate-theta-two-latch-body` must materialise
    ;; a merge Gamma inside the Theta's body so that both arms'
    ;; contributions to slot1 collapse into a single region-result.
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L_ENTRY")
                       (mk-insn 'CUTIEDENG-LABEL "L_HEAD")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'IFEQ "L_EXIT")
                       (mk-insn 'CUTIEDENG-LABEL "L_BODY")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IFEQ "L_ELSE")
                       (mk-insn 'CUTIEDENG-LABEL "L_THEN")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'ICONST_1)
                       (mk-insn 'ISUB)
                       (mk-insn 'ISTORE 1)
                       (mk-insn 'GOTO "L_HEAD")
                       (mk-insn 'CUTIEDENG-LABEL "L_ELSE")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'ICONST_2)
                       (mk-insn 'ISUB)
                       (mk-insn 'ISTORE 1)
                       (mk-insn 'GOTO "L_HEAD")
                       (mk-insn 'CUTIEDENG-LABEL "L_EXIT")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'IRETURN))
                 #:desc "(II)I"))
    (define lam (compile-method m))
    (check-pred Lambda? lam)
    (define r (region-of lam))
    ;; Parent region: exactly one Theta + final return.
    (define thetas
      (for/list ([kv (in-ordered-map (Region-node->value r))]
                 #:when (Theta? (cdr kv)))
        (cdr kv)))
    (check-equal? (length thetas) 1
                  "parent region should contain exactly one Theta")
    (check-not-false (memq 'return (node-ops r)))
    ;; Theta body: exactly one inner Gamma (the latch merge).
    (define body (Theta-region (car thetas)))
    (define merges
      (for/list ([kv (in-ordered-map (Region-node->value body))]
                 #:when (Gamma? (cdr kv)))
        (cdr kv)))
    (check-equal? (length merges) 1
                  "Theta body should host exactly one merge Gamma")
    (define merge-g (car merges))
    (define merge-subs (Gamma-regions merge-g))
    (check-equal? (length merge-subs) 2
                  "merge Gamma should have two sub-regions (one per latch)")
    ;; Each merge sub-region must contain an ISUB + an ISTORE and a
    ;; region-result (but no 'return sink).
    (for ([sub (in-list merge-subs)] [i (in-naturals)])
      (define ops
        (for/list ([kv (in-ordered-map (Region-node->value sub))])
          (define v (cdr kv))
          (cond [(Simple? v) (Simple-op v)] [else 'other])))
      (check-not-false (memq 'ISUB ops)
                       (format "merge sub-region ~a missing ISUB" i))
      (check-not-false (memq 'ISTORE ops)
                       (format "merge sub-region ~a missing ISTORE" i))
      (check-false (memq 'return ops)
                   (format "merge sub-region ~a should NOT contain a 'return" i))))

  ;; ----- multi-latch loop: continue-style pure back-edge arm -----
  (test-case "2-latch continue-style while-loop lowers to a Theta with a merge Gamma"
    ;; while (slot1 != 0) {
    ;;   if (slot0 != 0) { slot1 = slot1 - 1; }   ; fall-through latch
    ;;   // else: plain continue (no body work, direct back-edge)
    ;; }
    ;; return slot1;
    ;;
    ;; L_CONT is a pure back-edge block (Term:jump only) and is still
    ;; a valid latch: its only outgoing edge is the header jump.  The
    ;; merge Gamma's "continue" sub-region is therefore nearly empty
    ;; (region-arg + region-result) while the other arm carries the
    ;; ISUB / ISTORE.
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L_ENTRY")
                       (mk-insn 'CUTIEDENG-LABEL "L_HEAD")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'IFEQ "L_EXIT")
                       (mk-insn 'CUTIEDENG-LABEL "L_BODY")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'IFNE "L_FT")
                       (mk-insn 'CUTIEDENG-LABEL "L_CONT")
                       (mk-insn 'GOTO "L_HEAD")
                       (mk-insn 'CUTIEDENG-LABEL "L_FT")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'ICONST_1)
                       (mk-insn 'ISUB)
                       (mk-insn 'ISTORE 1)
                       (mk-insn 'GOTO "L_HEAD")
                       (mk-insn 'CUTIEDENG-LABEL "L_EXIT")
                       (mk-insn 'ILOAD 1)
                       (mk-insn 'IRETURN))
                 #:desc "(II)I"))
    (define lam (compile-method m))
    (check-pred Lambda? lam)
    (define r (region-of lam))
    (define thetas
      (for/list ([kv (in-ordered-map (Region-node->value r))]
                 #:when (Theta? (cdr kv)))
        (cdr kv)))
    (check-equal? (length thetas) 1)
    (define body (Theta-region (car thetas)))
    (define merges
      (for/list ([kv (in-ordered-map (Region-node->value body))]
                 #:when (Gamma? (cdr kv)))
        (cdr kv)))
    (check-equal? (length merges) 1)
    (define merge-subs (Gamma-regions (car merges)))
    (check-equal? (length merge-subs) 2)
    ;; Exactly one sub-region contains ISUB/ISTORE (the fall-through
    ;; latch); the other is the pure-continue arm.
    (define istores-per-sub
      (for/list ([sub (in-list merge-subs)])
        (for/sum ([kv (in-ordered-map (Region-node->value sub))])
          (define v (cdr kv))
          (if (and (Simple? v) (eq? (Simple-op v) 'ISTORE)) 1 0))))
    (check-equal? (sort istores-per-sub <) '(0 1)
                  "exactly one merge sub-region should host the fall-through ISTORE"))

  ;; ----- terminal TABLESWITCH -----
  (test-case "TABLESWITCH with all-return arms lowers to an N+1-arm Gamma"
    ;; switch (slot0) {
    ;;   case 0: return 1;
    ;;   case 1: return 2;
    ;;   case 2: return 3;
    ;;   default: return -1;
    ;; }
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L_ENTRY")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'TABLESWITCH
                                0 2
                                (list "L_DEFAULT" "L_C0" "L_C1" "L_C2"))
                       (mk-insn 'CUTIEDENG-LABEL "L_C0")
                       (mk-insn 'ICONST_1)
                       (mk-insn 'IRETURN)
                       (mk-insn 'CUTIEDENG-LABEL "L_C1")
                       (mk-insn 'ICONST_2)
                       (mk-insn 'IRETURN)
                       (mk-insn 'CUTIEDENG-LABEL "L_C2")
                       (mk-insn 'ICONST_3)
                       (mk-insn 'IRETURN)
                       (mk-insn 'CUTIEDENG-LABEL "L_DEFAULT")
                       (mk-insn 'ICONST_M1)
                       (mk-insn 'IRETURN))
                 #:desc "(I)I"))
    (define lam (compile-method m))
    (check-pred Lambda? lam)
    (define r (region-of lam))
    (define gammas
      (for/list ([kv (in-ordered-map (Region-node->value r))]
                 #:when (Gamma? (cdr kv)))
        (cdr kv)))
    (check-equal? (length gammas) 1
                  "parent region should contain exactly one Gamma")
    (define subs (Gamma-regions (car gammas)))
    (check-equal? (length subs) 4
                  "Gamma should have 4 sub-regions (default + 3 cases)")
    ;; Every sub-region must install a 'return sink.
    (for ([sub (in-list subs)] [i (in-naturals)])
      (define ops
        (for/list ([kv (in-ordered-map (Region-node->value sub))])
          (define v (cdr kv))
          (cond [(Simple? v) (Simple-op v)] [else 'other])))
      (check-not-false (memq 'return ops)
                       (format "sub-region ~a missing 'return" i)))
    ;; Keys recorded in each sub-region's info.  Order: default, 0,
    ;; 1, 2.
    (define keys
      (for/list ([sub (in-list subs)])
        (ordered-map-ref (Region-info sub) 'java/switch-case-key #f)))
    (check-equal? keys '(default 0 1 2)
                  "sub-regions should carry default / case-key info in order"))

  ;; ----- terminal LOOKUPSWITCH -----
  (test-case "LOOKUPSWITCH with all-return arms lowers to a keyed Gamma"
    ;; switch (slot0) {
    ;;   case 10: return 1;
    ;;   case 20: return 2;
    ;;   default: return 0;
    ;; }
    (define m
      (mk-method (list (mk-insn 'CUTIEDENG-LABEL "L_ENTRY")
                       (mk-insn 'ILOAD 0)
                       (mk-insn 'LOOKUPSWITCH
                                "L_DEFAULT"
                                (list 10 20)
                                (list "L_C10" "L_C20"))
                       (mk-insn 'CUTIEDENG-LABEL "L_C10")
                       (mk-insn 'ICONST_1)
                       (mk-insn 'IRETURN)
                       (mk-insn 'CUTIEDENG-LABEL "L_C20")
                       (mk-insn 'ICONST_2)
                       (mk-insn 'IRETURN)
                       (mk-insn 'CUTIEDENG-LABEL "L_DEFAULT")
                       (mk-insn 'ICONST_0)
                       (mk-insn 'IRETURN))
                 #:desc "(I)I"))
    (define lam (compile-method m))
    (check-pred Lambda? lam)
    (define r (region-of lam))
    (define gammas
      (for/list ([kv (in-ordered-map (Region-node->value r))]
                 #:when (Gamma? (cdr kv)))
        (cdr kv)))
    (check-equal? (length gammas) 1)
    (define subs (Gamma-regions (car gammas)))
    (check-equal? (length subs) 3)
    (define keys
      (for/list ([sub (in-list subs)])
        (ordered-map-ref (Region-info sub) 'java/switch-case-key #f)))
    (check-equal? keys '(default 10 20)))

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
