#lang racket/base

;; ============================================================
;; opt: the affine subset of scalar evolutions and loop iteration
;; analysis (gcc/tree-scalar-evolution.cc simple_iv,
;; gcc/tree-ssa-loop-niter.cc number_of_iterations_exit / finite_loop_p)
;; ============================================================
;;
;;   (simple-iv st m cfg loop t) -> (values base step) | (values #f #f)
;;     T as an affine induction variable of LOOP: BASE a tree (an
;;     integer constant, an invariant SSA name, or a binary expression
;;     over them) and STEP an exact integer (0: loop invariant).  The
;;     chain is followed through copies, +/- constant and conversions;
;;     the step comes from the header PHI's latch argument.
;;   (invert-tree-comparison code), (swap-tree-comparison* code)
;;   (loop-exit-niter st m cfg loop src dest) -> number | #f
;;     the number of iterations before the exit edge SRC->DEST is taken
;;     when base and bound are constants (the closed form), else #f
;;   (loop-finite? st m cfg loop) -> boolean
;;     finite_loop_p without the undefined-behavior and -ffinite-loops
;;     cases: some exit condition compares an affine IV with an
;;     invariant such that number_of_iterations_cond succeeds without
;;     assumptions: < / <= with a positive step, > / >= with a negative
;;     step, != with a unit step (or any step on a type whose overflow
;;     is undefined).
;; ============================================================

(require "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../cfg.rkt"
         "ssa-ops.rkt"
         "fold.rkt"
         "mfn.rkt"
         "loops.rkt")

(provide simple-iv invert-tree-comparison swap-tree-comparison* loop-exit-niter loop-finite?)

(define (icst? t) (and (Tree? t) (eq? (tree-code t) 'integer_cst)))

(define (swap-tree-comparison* c)
  (case c [(lt_expr) 'gt_expr] [(gt_expr) 'lt_expr] [(le_expr) 'ge_expr] [(ge_expr) 'le_expr] [else c]))
(define (invert-tree-comparison c)
  (case c [(eq_expr) 'ne_expr] [(ne_expr) 'eq_expr] [(lt_expr) 'ge_expr] [(ge_expr) 'lt_expr] [(gt_expr) 'le_expr] [(le_expr) 'gt_expr] [else #f]))

(define (simple-iv st m cfg l t)
  (define (invariant? x) (or (not (ssa-name? x)) (ssa-default-def? x)
                             (let ([id (mfn-def m x)]) (and id (not (loop-inside? l (mfn-bb m id)))))))
  (define (header-phi x)
    (define id (mfn-def m x))
    (define g (and id (mfn-stmt m id)))
    (and g (gphi? g) (= (mfn-bb m id) (Loop-header l)) g))
  (cond
    [(icst? t) (values t 0)]
    [(not (ssa-name? t)) (values #f #f)]
    [(invariant? t) (values t 0)]
    [else
     (let analyze ([x t] [depth 0])
       (cond
         [(> depth 16) (values #f #f)]
         [(invariant? x) (values x 0)]
         [(header-phi x)
          => (lambda (ph)
               (define pre (loop-preheader-edge cfg l))
               (define latch (car (Loop-latches l)))
               (define init (and pre (cdr (for/first ([a (in-pvector (gphi-args ph))] #:when (= (car a) (car pre))) a))))
               (define next (cdr (for/first ([a (in-pvector (gphi-args ph))] #:when (= (car a) latch)) a)))
               (cond
                 [(not (and init (invariant? init))) (values #f #f)]
                 [else
                  ;; next = x + c through copies
                  (define step
                    (let loop ([y next] [d 0])
                      (cond
                        [(> d 16) #f]
                        [(not (ssa-name? y)) #f]
                        [(ssa-name=? y x) 0]
                        [else
                         (define g (mfn-def-stmt m y))
                         (cond
                           [(not (and g (gassign? g))) #f]
                           [(eq? (gassign-code g) 'ssa_name) (loop (gassign-rhs1 g) (add1 d))]
                           [(and (memq (gassign-code g) '(plus_expr pointer_plus_expr)) (ssa-name? (gassign-rhs1 g)) (icst? (gassign-rhs2 g)))
                            (define s (loop (gassign-rhs1 g) (add1 d)))
                            (and s (+ s (integer-cst-value (gassign-rhs2 g))))]
                           [(and (eq? (gassign-code g) 'minus_expr) (ssa-name? (gassign-rhs1 g)) (icst? (gassign-rhs2 g)))
                            (define s (loop (gassign-rhs1 g) (add1 d)))
                            (and s (- s (integer-cst-value (gassign-rhs2 g))))]
                           [else #f])])))
                  (if step (values init step) (values #f #f))]))]
         [else
          (define g (mfn-def-stmt m x))
          (cond
            [(not (and g (gassign? g))) (values #f #f)]
            [(eq? (gassign-code g) 'ssa_name) (analyze (gassign-rhs1 g) (add1 depth))]
            [(and (memq (gassign-code g) '(plus_expr minus_expr pointer_plus_expr)) (icst? (gassign-rhs2 g)))
             (define-values (b s) (analyze (gassign-rhs1 g) (add1 depth)))
             (cond
               [(not b) (values #f #f)]
               [(icst? b)
                (define c (integer-cst-value (gassign-rhs2 g)))
                (values (build-int-cst st (tree-type b) (if (eq? (gassign-code g) 'minus_expr) (- (integer-cst-value b) c) (+ (integer-cst-value b) c))) s)]
               [else (values (build2 st (gassign-code g) (tree-type x) b (gassign-rhs2 g)) s)])]
            [(memq (gassign-code g) '(nop_expr convert_expr))
             (define-values (b s) (analyze (gassign-rhs1 g) (add1 depth)))
             (if (and b (eqv? s 0)) (values x 0) (values #f #f))]
            [else (values #f #f)])]))]))

;; the exit condition of SRC->DEST normalized to IV cmp BOUND:
;; (values code base step bound-tree iv-type) with CODE the condition
;; under which the loop continues, or (values #f ...) when not affine
(define (exit-condition st m cfg l src dest)
  (define g (bb-last-stmt cfg src))
  (define (fail) (values #f #f #f #f #f))
  (cond
    [(not (and g (gcond? g))) (fail)]
    [else
     (define e (for/first ([e (in-pvector (cfg-succs cfg src))] #:when (= (GEdge-dest e) dest)) e))
     (define true? (not (zero? (bitwise-and (GEdge-flags e) EDGE-TRUE-VALUE))))
     (define code (if true? (invert-tree-comparison (gcond-code g)) (gcond-code g)))
     (define-values (b0 s0) (simple-iv st m cfg l (gcond-lhs g)))
     (define-values (b1 s1) (simple-iv st m cfg l (gcond-rhs g)))
     (cond
       [(not (and code b0 b1)) (fail)]
       [(or (and (zero? s0) (zero? s1)) (and (not (zero? s0)) (not (zero? s1)))) (fail)]
       [(zero? s0) (values (swap-tree-comparison* code) b1 s1 b0 (tree-type (gcond-rhs g)))]
       [else (values code b0 s0 b1 (tree-type (gcond-lhs g)))])]))

(define (loop-exit-niter st m cfg l src dest)
  (define-values (c b step bd type) (exit-condition st m cfg l src dest))
  (cond
    [(not (and c (icst? b) (icst? bd))) #f]
    [else
     (define base (integer-cst-value b)) (define bound (integer-cst-value bd))
     (define (holds? k) (let ([v (+ base (* k step))])
                          (case c [(lt_expr) (< v bound)] [(le_expr) (<= v bound)] [(gt_expr) (> v bound)]
                            [(ge_expr) (>= v bound)] [(ne_expr) (not (= v bound))] [(eq_expr) (= v bound)] [else #f])))
     (case c
       [(lt_expr le_expr)
        (cond
          [(<= step 0) (if (holds? 0) #f 0)]
          [(not (holds? 0)) 0]
          [else (define diff (- bound base))
                (if (eq? c 'lt_expr) (quotient (+ diff step -1) step) (add1 (quotient diff step)))])]
       [(gt_expr ge_expr)
        (cond
          [(>= step 0) (if (holds? 0) #f 0)]
          [(not (holds? 0)) 0]
          [else (define diff (- base bound)) (define s (- step))
                (if (eq? c 'gt_expr) (quotient (+ diff s -1) s) (add1 (quotient diff s)))])]
       [(ne_expr)
        (cond
          [(= base bound) 0]
          [(zero? step) #f]
          [(and (zero? (remainder (- bound base) step)) (> (quotient (- bound base) step) 0)) (quotient (- bound base) step)]
          [else #f])]
       [else #f])]))

(define (loop-finite? st m cfg l)
  (for/or ([e (in-list (loop-exits cfg l))])
    (define-values (c b step bd type) (exit-condition st m cfg l (car e) (cdr e)))
    (and c
         (let ([undefined-overflow? (and (eq? (type-code st type) 'integer_type) (not (type-unsigned?* st type)))])
           (case c
             [(lt_expr le_expr) (> step 0)]
             [(gt_expr ge_expr) (< step 0)]
             [(ne_expr) (or (= (abs step) 1) (and undefined-overflow? (not (zero? step))))]
             [else #f])))))
