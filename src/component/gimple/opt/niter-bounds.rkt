#lang racket/base

;; ============================================================
;; opt: loop iteration bounds (gcc/tree-ssa-loop-niter.cc
;; estimate_numbers_of_iterations and its records, cfgloop.cc
;; get_max_loop_iterations, tree-ssa-loop-ivcanon.cc
;; adjust_loop_info_after_peeling, tree-scalar-evolution.cc
;; iv_can_overflow_p), GCC 12 and 15
;; ============================================================
;;
;; The bounds GCC keeps on a loop (any_upper_bound /
;; nb_iterations_upper_bound, the likely upper bound, the estimate)
;; live in the function's 'loop-bounds declaration flag, a hash from
;; the loop number to (lbnd upper likely estimate) (#f = not known).
;; They survive the passes like GCC's loop structures do and only go
;; down (record_niter_bound); free_numbers_of_iterations_estimates only
;; forgets that a loop was estimated.
;;
;; Inside a scev.rkt context (with-loops):
;;   (estimate-numbers-of-iterations! l)   once per loop and context:
;;     the bound of every exit whose number of iterations is known
;;     (number_of_iterations_exit, every_iteration = false), the
;;     undefined-behavior bounds (infer_loop_bounds_from_undefined: array
;;     indices, signed induction variables, pointer arithmetic in blocks
;;     that dominate the latch), discover_iteration_bound_by_body_walk,
;;     maybe_lower_iteration_bound, the exact count of a constant loop.
;;   (max-loop-iterations l) / (likely-max-loop-iterations l) /
;;   (estimated-loop-iterations l)   get_*_loop_iterations (no estimation)
;; The storage is loop-bounds.rkt; iv_can_overflow_p is in scev.rkt.
;;
;; NITER_DEBUG=1 prints the TDF_DETAILS lines of the estimation to
;; stderr ("Statement ... is executed at most ...", "Induction variable
;; ... does not wrap ...", "Found better loop bound", "Reducing loop
;; iteration estimate by 1 ...").
;;
;; Not ported: the profile-based estimate (a guessed profile is never
;; reliable), __builtin_expect_with_probability bounds,
;; record_control_iv / record_nonwrapping_chrec (the scev nowrap
;; caches), array_ref_flexible_size_p beyond "no upper bound in the
;; domain", irreducible regions.
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../cfg.rkt"
         (only-in "loops.rkt" loops-innermost-first loops-preorder)
         "ssa-ops.rkt"
         "generic-fold.rkt"
         "scev.rkt"
         "irange.rkt"
         "loop-bounds.rkt"
         (only-in "range-info.rkt" gimple-range-global)
         (only-in "../print.rkt" print-expr print-type stmt->string))

(provide estimate-numbers-of-iterations! max-loop-iterations likely-max-loop-iterations estimated-loop-iterations
         estimate-all-loops estimate-one-loop)

;; estimate_numbers_of_iterations for every loop of CFG, in
;; loops_list (fn, 0) preorder, in a fresh context: -> st
(define (estimate-all-loops st decl cfg #:only [only (lambda (l) #t)])
  (define-values (_r st1)
    (with-loops st decl cfg
      (lambda ()
        (for ([l (in-list (loops-preorder (ctx-loops)))] #:when (only l))
          (estimate-numbers-of-iterations! l)))))
  st1)

;; estimate_numbers_of_iterations (loop) for the loop of CFG whose header
;; is HEADER, in a fresh context: -> (values st records), RECORDS the
;; loop's loop->bounds (newest first) as (vector bound bb stmt is-exit?)
(define (estimate-one-loop st decl cfg header)
  (define-values (r st1)
    (with-loops st decl cfg
      (lambda ()
        (define l (for/first ([l (in-list (ctx-loops))] #:when (= (Loop-header l) header)) l))
        (cond
          [l (estimate-numbers-of-iterations! l)
             (hash-ref (hash-ref bound-records (current-loop-ctx) (hasheqv)) (loop-num l) '())]
          [else '()]))))
  (values st1 r))

;; ---- storage (loop-bounds.rkt) ------------------------------------------
(define (bounds-of l) (loop-bound (gs) (ctx-decl) (loop-num l)))
(define (max-loop-iterations l) (lbnd-upper (bounds-of l)))
(define (likely-max-loop-iterations l) (lbnd-likely (bounds-of l)))
(define (estimated-loop-iterations l) (lbnd-estimate (bounds-of l)))
(define (record-niter-bound! l bound realistic? upper?)
  (gs! (record-niter-bound (gs) (ctx-decl) (loop-num l) bound realistic? upper?)))

;; integer_nonzerop
(define (integer-nonzerop x) (and (icst? x) (not (zero? (icst-val x)))))

;; ---- debug output ---------------------------------------------------------
(define debug? (and (getenv "NITER_DEBUG") #t))
(define (dbg fmt . args) (when debug? (eprintf "~a: ~a" (decl-print-name* (ctx-decl)) (apply format fmt args))))
(define (decl-print-name* d) (tree-attr d 'name "?"))
(define (pe t) (print-expr (gs) t))
(define (ps g) (stmt->string (gs) (ctx-cfg) #f g))

;; ---- estimate_numbers_of_iterations -------------------------------------
(define estimated (make-weak-hasheq))           ; context -> loop number -> #t (estimate_state)
(define bound-records (make-weak-hasheq))       ; context -> loop number -> loop->bounds

(define (estimate-numbers-of-iterations! l)
  (define done (hash-ref! estimated (current-loop-ctx) make-hasheqv))
  (unless (hash-ref done (loop-num l) #f)
    (hash-set! done (loop-num l) #t)
    (dbg "Estimating # of iterations of loop ~a\n" (loop-num l))
    (define nb (number-of-latch-executions l))
    (define body (loop-body-blocks l))
    (define exits (for*/list ([bb (in-list body)] [e (in-pvector (cfg-succs (ctx-cfg) bb))]
                              #:unless (loop-inside? l (GEdge-dest e)))
                    (cons bb (GEdge-dest e))))
    (define likely-exit (single-likely-exit l exits))
    (define records '())                            ; loop->bounds: (vector bound stmt-bb stmt is-exit), newest first
    ;; record_estimate
    (define (record-estimate! bound i-bound at-bb at-stmt is-exit? realistic? upper?)
      (dbg "Statement ~a~a\n is ~aexecuted at most ~a (bounded by ~a) + 1 times in loop ~a.\n"
           (if is-exit? "(exit)" "") (ps at-stmt) (if upper? "" "probably ") (pe bound) i-bound (loop-num l))
      (define realistic (and realistic? (icst? bound)))
      (when (and upper? (or is-exit? (not (icst? nb))))
        (set! records (cons (vector i-bound at-bb at-stmt is-exit?) records)))
      (define upper (and upper? (dominated-by? (loop-latch l) at-bb)))
      (define delta (if is-exit? 0 1))
      (define new-bound (+ i-bound delta))
      (record-niter-bound! l new-bound realistic upper))
    ;; the exits
    (for ([ex (in-list exits)])
      (define d (number-of-iterations-exit-assumptions l ex #f))
      (when (and d (integer-nonzerop (niter-desc-assumptions d)) (niter-desc-max d))
        (define niter0 (niter-desc-niter d))
        (define mbz (niter-desc-may-be-zero d))
        (define niter (if (icst? mbz) niter0 (gf-build3 'cond_expr (tree-type niter0) mbz (icst (tree-type niter0) 0) niter0)))
        (define src (car ex))
        (record-estimate! niter (niter-desc-max d) src (bb-last-stmt (ctx-cfg) src) #t (equal? ex likely-exit) #t)))
    ;; infer_loop_bounds_from_undefined (flag_aggressive_loop_optimizations)
    (for ([bb (in-list body)])
      (define reliable (dominated-by? (loop-latch l) bb))
      (for ([g (in-pvector (GBlock-stmts (cfg-block (ctx-cfg) bb)))])
        (infer-loop-bounds-from-array l bb g record-estimate!)
        (when reliable
          (infer-loop-bounds-from-signedness l bb g record-estimate!)
          (infer-loop-bounds-from-pointer-arith l bb g record-estimate!))))
    (discover-iteration-bound-by-body-walk! l records)
    (maybe-lower-iteration-bound! l records)
    (hash-set! (hash-ref! bound-records (current-loop-ctx) make-hasheqv) (loop-num l) records)
    ;; a constant number of iterations is the upper bound
    (when (icst? nb)
      (gs! (set-loop-upper-bound (gs) (ctx-decl) (loop-num l) (icst-val nb))))))

;; single_likely_exit: the single exit, else the only exit that is not
;; probably never executed (probably_never_executed_edge_p: its source's
;; count is zero or its probability never; the profile quality is not
;; modelled, a count of 0 is taken as profile_count::zero) and whose
;; probability is above very_unlikely
(define (single-likely-exit l exits)
  (cond
    [(= 1 (length exits)) (car exits)]
    [else
     (let/ec return
       (for/fold ([found #f]) ([ex (in-list exits)])
         (define p (prof-prob (ctx-cfg) (car ex) (cdr ex)))
         (cond
           [(eqv? (prof-count (ctx-cfg) (car ex)) 0) found]
           [(and p (<= p VERY-UNLIKELY)) found]
           [found (return #f)]
           [else ex])))]))
;; profile_probability::very_unlikely: RDIV (2^27, 2000) = 67109, then m_val--
(define VERY-UNLIKELY (sub1 (quotient (+ 134217728 1000) 2000)))

;; ---- undefined behavior ---------------------------------------------------
;; infer_loop_bounds_from_signedness
(define (infer-loop-bounds-from-signedness l bb g record-estimate!)
  (when (gassign? g)
    (define def (gassign-lhs g))
    (when (and (ssa-name? def) (ty-int? (tree-type def)) (ty-undef? (tree-type def)))
      (define type (tree-type def))
      (define ev (instantiate-parameters l (analyze-scalar-evolution l def)))
      (unless (chrec-contains-undetermined? ev)
        (define base (initial-condition-in-loop-num ev (loop-num l)))
        (define step (evolution-part-in-loop-num ev (loop-num l)))
        (when (and base step (icst? step) (not (tree-contains-chrecs? base))
                   (not (chrec-contains-symbols-defined-in-loop? base (loop-num l))))
          (define r (gimple-range-global (gs) (ctx-decl) def))
          (define-values (low high)
            (if (and r (not (ir-varying? r)) (not (ir-undefined? r)))
                (values (icst type (ir-lower r)) (icst type (ir-upper r)))
                (values (icst type (ty-min type)) (icst type (ty-max type)))))
          (record-nonwrapping-iv! l base step bb g low high #f #t record-estimate!))))))

;; infer_loop_bounds_from_pointer_arith
(define (infer-loop-bounds-from-pointer-arith l bb g record-estimate!)
  (when (and (gassign? g) (eq? (gassign-code g) 'pointer_plus_expr))
    (define def (gassign-lhs g))
    (define type (tree-type def))
    (define ptr (gassign-rhs1 g))
    (define var (gassign-rhs2 g))
    (when (and (ssa-name? def) (nowrap-type-p type) (expr-invariant-in-loop?* l ptr)
               (= (ty-prec type) (ty-prec (tree-type var))))
      (define uloop (bb-loop bb))
      (define ev (instantiate-parameters l (analyze-scalar-evolution uloop def)))
      (unless (chrec-contains-undetermined? ev)
        (define base (initial-condition-in-loop-num ev (loop-num l)))
        (define step (evolution-part-in-loop-num ev (loop-num l)))
        (when (and base step (icst? step) (not (tree-contains-chrecs? base))
                   (not (chrec-contains-symbols-defined-in-loop? base (loop-num l))))
          ;; flag_delete_null_pointer_checks: no object at address 0
          (define align (quotient (or (type-attr (gs) (pointed-type type) 'align #f) 8) 8))
          (define low (icst type align))
          (define high (icst type (ty-max type)))
          (record-nonwrapping-iv! l base step bb g low high #f #t record-estimate!))))))
(define (pointed-type t) (type-type (gs) t))
(define (expr-invariant-in-loop?* l e)
  (cond
    [(not (Tree? e)) #t]
    [(ssa-name? e) (let ([b (def-bb-of e)]) (or (not b) (not (loop-inside? l b))))]
    [(memq (tree-code e) '(integer_cst addr_expr)) #t]
    [else (for/and ([i (in-range (tree-operand-length e))]) (expr-invariant-in-loop?* l (tree-operand e i)))]))

;; infer_loop_bounds_from_array: the ARRAY_REF indices of the references
(define (infer-loop-bounds-from-array l bb g record-estimate!)
  (define refs
    (cond
      [(gassign? g) (list (gassign-lhs g) (gassign-rhs1 g))]
      [(gcall? g) (cons (gcall-lhs g) (pvector->list (gcall-args g)))]
      [else '()]))
  (for ([ref (in-list refs)] #:when (and ref (Tree? ref) (reference-class? ref)))
    ;; for_each_index: the handled components from the outside in
    (let walk ([t ref])
      (when (and (Tree? t) (handled-component? t))
        (when (eq? (tree-code t) 'array_ref) (idx-infer-loop-bounds! l bb g t record-estimate!))
        (walk (tree-operand t 0))))))
(define (reference-class? t) (memq (tree-code t) '(array_ref array_range_ref component_ref bit_field_ref mem_ref target_mem_ref indirect_ref realpart_expr imagpart_expr view_convert_expr)))
(define (handled-component? t) (memq (tree-code t) '(array_ref array_range_ref component_ref bit_field_ref realpart_expr imagpart_expr view_convert_expr)))
;; idx_infer_loop_bounds
(define (idx-infer-loop-bounds! l bb g aref record-estimate!)
  (define arr-type (tree-type (tree-operand aref 0)))
  (define domain (type-attr (gs) arr-type 'domain #f))
  (define dmin (and domain (type-attr (gs) domain 'min #f)))
  (define dmax (and domain (type-attr (gs) domain 'max #f)))
  (define flexible? (not (and dmax (icst? dmax))))
  (define upper? (not flexible?))
  (define idx (tree-operand aref 1))
  (define dloop (bb-loop bb))
  (when dloop
    (define ev (instantiate-parameters l (analyze-scalar-evolution dloop idx)))
    (define init (initial-condition ev))
    (define step (evolution-part-in-loop-num ev (loop-num l)))
    (when (and init step (icst? step) (not (integer-zerop step)) (not (tree-contains-chrecs? init))
               (not (chrec-contains-symbols-defined-in-loop? init (loop-num l))))
      (define low0 (or dmin (and domain (icst domain 0))))
      (when (and low0 (icst? low0) dmax (icst? dmax))
        (define type (tree-type step))
        (when (and (not (and flexible? (= (icst-val low0) (icst-val dmax))))
                   (int-fits-type? (icst-val dmax) type) (int-fits-type? (icst-val low0) type))
          (define low (icst type (icst-val low0)))
          (define high (icst type (icst-val dmax)))
          (define sign? (< (signed-val step) 0))
          (define next (+ (if sign? (icst-val low) (icst-val high)) (signed-val step)))
          (unless (and (<= (icst-val low) next) (<= next (icst-val high)))
            ;; an access not executed on every iteration: scev_probably_wraps_p is not ported
            (define upper (if (dominated-by? (loop-latch l) bb) upper? #f))
            (record-nonwrapping-iv! l init step bb g low high #f upper record-estimate!)))))))
(define (int-fits-type? v t) (and (<= (ty-min t) v) (<= v (ty-max t))))
(define (signed-val c)
  (define t (tree-type c)) (define v (icst-val c)) (define p (ty-prec t))
  (if (and (ty-uns? t) (bitwise-bit-set? v (sub1 p))) (- v (arithmetic-shift 1 p)) v))

;; record_nonwrapping_iv
(define (record-nonwrapping-iv! l base0 step0 bb g low high realistic? upper? record-estimate!)
  (when (and (icst? step0) (not (integer-zerop step0)))
    (define type (tree-type base0))
    (dbg "Induction variable (~a) ~a + ~a * iteration does not wrap in statement ~a\n in loop ~a.\n"
         (print-type (gs) type) (pe base0) (pe step0) (ps g) (loop-num l))
    (define utype (utype-for type))
    (define prec (ty-prec utype))
    (define (u v) (bitwise-and v (sub1 (arithmetic-shift 1 prec))))
    (define base-u (gf-convert utype base0))
    (define step-u (gf-convert utype step0))
    (define r (and (ssa-name? base0) (ty-int? type) (gimple-range-global (gs) (ctx-decl) base0)))
    (define range-ok? (and r (not (ir-varying? r)) (not (ir-undefined? r))))
    (define latch-dom? (dominated-by? (loop-latch l) bb))
    (define-values (delta step)
      (cond
        [(bitwise-bit-set? (icst-val step-u) (sub1 prec))
         ;; a negative step
         (define max (or (and range-ok? (ir-upper r)) (and (ssa-name? base0) (icst? high) (cst-init-from-scev base0 #f))))
         (define extreme (u (icst-val (gf-convert utype low))))
         (define base
           (cond
             [(and (ssa-name? base0) (icst? high) (ty-int? type) max (> (signed-val high) max)) (u max)]
             [(and (not (icst? base-u)) latch-dom?) (u (icst-val (gf-convert utype high)))]
             [(icst? base-u) (icst-val base-u)]
             [else #f]))
         (values (and base (u (- base extreme))) (u (- (icst-val step-u))))]
        [else
         (define min (or (and range-ok? (ir-lower r)) (and (ssa-name? base0) (icst? low) (cst-init-from-scev base0 #t))))
         (define extreme (u (icst-val (gf-convert utype high))))
         (define base
           (cond
             [(and (ssa-name? base0) (icst? low) (ty-int? type) min (> min (signed-val low))) (u min)]
             [(and (not (icst? base-u)) latch-dom?) (u (icst-val (gf-convert utype low)))]
             [(icst? base-u) (icst-val base-u)]
             [else #f]))
         (values (and base (u (- extreme base))) (icst-val step-u))]))
    ;; a symbolic delta: derive_constant_upper_bound of (extreme - base) / step
    (define niter-bound
      (if delta
          (icst utype (quotient delta step))
          (gf-build2 'floor_div_expr utype
                     (if (bitwise-bit-set? (icst-val step-u) (sub1 prec))
                         (gf-build2 'minus_expr utype base-u (gf-convert utype low))
                         (gf-build2 'minus_expr utype (gf-convert utype high) base-u))
                     (icst utype step))))
    (define max (derive-constant-upper-bound niter-bound))
    (record-estimate! niter-bound max bb g #f realistic? upper?)))

;; get_cst_init_from_scev
(define (cst-init-from-scev var min?)
  (define dl (stmt-loop-of-name var))
  (and dl
       (let ([iv (simple-iv dl dl var #f)])
         (and iv (affine-iv-no-overflow iv) (icst? (affine-iv-base iv)) (icst? (affine-iv-step iv))
              (not (eq? min? (< (signed-val (affine-iv-step iv)) 0)))
              (signed-val (affine-iv-base iv))))))

;; derive_constant_upper_bound
(define (derive-constant-upper-bound val)
  (cond
    [(icst? val) (icst-val val)]
    [else
     (define code (tree-code val))
     (derive-ops (tree-type val) (if (>= (tree-operand-length val) 1) (tree-operand val 0) val) code
                 (if (>= (tree-operand-length val) 2) (tree-operand val 1) #f))]))
(define (derive-ops type op0 code op1)
  (define max (ty-max type))
  (case code
    [(integer_cst) (icst-val op0)]
    [(nop_expr convert_expr)
     (define subtype (tree-type op0))
     (cond
       [(and (not (ty-uns? subtype)) (ty-uns? type) (not (tree-expr-nonnegative? op0))) max]
       [else (let ([bnd (derive-constant-upper-bound op0)]) (if (< max bnd) max bnd))])]
    [(plus_expr pointer_plus_expr minus_expr)
     (cond
       [(or (not (icst? op1)) (not (tree-expr-nonnegative? op0))) max]
       [else
        (define cst0 (let* ([p (ty-prec type)] [v (bitwise-and (icst-val op1) (sub1 (arithmetic-shift 1 p)))])
                       (if (bitwise-bit-set? v (sub1 p)) (- v (arithmetic-shift 1 p)) v)))
        (define cst (if (eq? code 'minus_expr) cst0 (- cst0)))
        (define bnd (derive-constant-upper-bound op0))
        (cond
          [(< cst 0)
           (define c (- cst))
           (if (<= bnd (- max c)) max (+ bnd c))]
          [(< bnd cst) max]
          [(and (ty-uns? type)
                (let ([tem (gf-binary 'ge_expr (boolean-type (gs)) op0 (icst type cst))])
                  (or (not tem) (integer-nonzerop tem))))
           max]
          [else (- bnd cst)])])]
    [(floor_div_expr exact_div_expr)
     (if (or (not (icst? op1)) (< (signed-val op1) 0)) max (quotient (derive-constant-upper-bound op0) (icst-val op1)))]
    [(bit_and_expr) (if (or (not (icst? op1)) (< (signed-val op1) 0)) max (icst-val op1))]
    [(ssa_name)
     (define g (def-stmt-of op0))
     (if (and g (gassign? g) (ssa-name? (gassign-lhs g)) (ssa-name=? (gassign-lhs g) op0))
         (derive-ops (tree-type op0) (gassign-rhs1 g) (gassign-code g) (gassign-rhs2 g))
         max)]
    [else max]))

;; tree_expr_nonnegative_p (the cases derive_constant_upper_bound meets)
(define (tree-expr-nonnegative? t)
  (define type (tree-type t))
  (cond
    [(icst? t) (>= (signed-val t) 0)]
    [(and (ty-int? type) (ty-uns? type)) #t]
    [(ssa-name? t)
     (define r (and (ty-int? type) (gimple-range-global (gs) (ctx-decl) t)))
     (and r (not (ir-undefined? r)) (>= (ir-lower r) 0))]
    [else
     (case (tree-code t)
       [(nop_expr convert_expr)
        (define inner (tree-type (tree-operand t 0)))
        (or (and (ty-int? inner) (ty-uns? inner) (< (ty-prec inner) (ty-prec type)))
            (tree-expr-nonnegative? (tree-operand t 0)))]
       [(plus_expr mult_expr)
        (and (ty-undef? type) (tree-expr-nonnegative? (tree-operand t 0)) (tree-expr-nonnegative? (tree-operand t 1)))]
       [(cond_expr) (and (tree-expr-nonnegative? (tree-operand t 1)) (tree-expr-nonnegative? (tree-operand t 2)))]
       [else #f])]))

;; ---- discover_iteration_bound_by_body_walk ----------------------------------
(define (discover-iteration-bound-by-body-walk! l records)
  (define upper (max-loop-iterations l))
  (define (rec-bound r) (if (vector-ref r 3) (vector-ref r 0) (add1 (vector-ref r 0))))
  (define candidates (for/list ([r (in-list records)] #:when (or (not upper) (< (rec-bound r) upper))) r))
  (unless (null? candidates)
    (dbg " Trying to walk loop body to reduce the bound.\n")
    (define bounds (sort (remove-duplicates (map rec-bound candidates)) <))
    (define (bound-index b) (index-of bounds b))
    (define bb-bounds (make-hasheqv))
    (for ([r (in-list candidates)])
      (define i (bound-index (rec-bound r)))
      (define bb (vector-ref r 1))
      (hash-set! bb-bounds bb (min i (hash-ref bb-bounds bb i))))
    (define cfg (ctx-cfg))
    (define latch (loop-latch l))
    (define n (length bounds))
    (define queues (make-vector (add1 n) '()))      ; each a stack (the last pushed popped first)
    (define priority (make-hasheqv))
    (vector-set! queues n (list (Loop-header l)))
    (hash-set! priority (Loop-header l) n)
    (define latch-index
      (for/fold ([latch-index -1]) ([qi (in-range n -1 -1)])
        (cond
          [(< latch-index qi)
           (let loop ([latch-index latch-index])
             (define q (vector-ref queues qi))
             (cond
               [(null? q) latch-index]
               [else
                (define bb (car q))
                (vector-set! queues qi (cdr q))
                (cond
                  [(> (hash-ref priority bb) qi) (loop latch-index)]
                  [else
                   (define bi (min qi (hash-ref bb-bounds bb qi)))
                   (loop
                    (for/fold ([li latch-index]) ([e (in-pvector (cfg-succs cfg bb))])
                      (define d (GEdge-dest e))
                      (cond
                        [(not (loop-inside? l d)) li]
                        [(and (= bb latch) (= d (Loop-header l))) (if (< li bi) bi li)]
                        [else
                         (define p (hash-ref priority d #f))
                         (when (or (not p) (< p bi))
                           (hash-set! priority d bi)
                           (vector-set! queues bi (cons d (vector-ref queues bi))))
                         li])))])]))]
          [else latch-index])))
    (when (< latch-index n)
      (dbg "Found better loop bound ~a\n" (list-ref bounds latch-index))
      (record-niter-bound! l (list-ref bounds latch-index) #f #t))))

;; ---- maybe_lower_iteration_bound ---------------------------------------------
(define (maybe-lower-iteration-bound! l records)
  (define upper (max-loop-iterations l))
  (define stmts (for/list ([r (in-list records)] #:when (and (not (vector-ref r 3)) upper (< (vector-ref r 0) upper))) (vector-ref r 2)))
  (unless (null? stmts)
    (define cfg (ctx-cfg))
    (define latch (loop-latch l))
    (define visited (make-hasheqv))
    (hash-set! visited (Loop-header l) #t)
    (define found-exit
      (let loop ([queue (list (Loop-header l))])
        (cond
          [(null? queue) #f]
          [else
           (define bb (car queue))
           (define-values (stmt-found exit)
             (for/fold ([sf #f] [ex #f]) ([g (in-pvector (GBlock-stmts (cfg-block cfg bb)))] #:break (or sf ex))
               (cond
                 [(memq g stmts) (values #t #f)]
                 [(stmt-has-side-effects? (gs) g) (values #f #t)]
                 [else (values #f #f)])))
           (cond
             [exit #t]
             [stmt-found (loop (cdr queue))]
             [else
              (let/ec return
                (define more
                  (for/fold ([acc '()]) ([e (in-pvector (cfg-succs cfg bb))])
                    (define d (GEdge-dest e))
                    (cond
                      [(or (not (loop-inside? l d)) (and (= bb latch) (= d (Loop-header l)))) (return #t)]
                      [(hash-ref visited d #f) acc]
                      [else (hash-set! visited d #t) (cons d acc)])))
                (loop (append more (cdr queue))))])])))
    (unless found-exit
      (dbg "Reducing loop iteration estimate by 1; undefined statement must be executed at the last iteration.\n")
      (record-niter-bound! l (sub1 upper) #f #t))))

