#lang racket/base

;; ============================================================
;; opt: the IPA stage at -O1 between release_ssa and all_optimizations,
;; reduced to what the local pipeline observes
;; ============================================================
;;
;; * ipa-fnsummary: per function the size entries of
;;   analyze_function_body (estimate_num_insns with eni_size_weights,
;;   eliminated_by_inlining_prob splitting a statement between the
;;   "not inlined" and the unconditional entry, the uninlined overhead
;;   param_uninlined_function_insns), per call edge the call statement
;;   size; predicates on parameters are not tracked (every entry counts
;;   as if its condition held).
;; * ipa-inline: only "inline into all callers" (flag_inline_functions_
;;   called_once at -O1: want_inline_function_to_all_callers_p with
;;   growth_positive_p, check_callers, can_remove_node_now_p); the small
;;   function path is off for functions not declared inline at -O1.
;;   The decision is a plan (caller -> callees); the body copy is done
;;   per function by opt/inline.rkt when the late pipeline starts.
;; * remove_unreachable_nodes: a static function inlined into all its
;;   callers leaves the unit.
;; * ipa_discover_variable_flags (pass_ipa_reference): a static
;;   variable never written nor address-taken becomes TREE_READONLY
;;   (the 'readonly declaration flag), which lets the late ccp fold
;;   its loads.
;;
;; Not ported: inlining of functions declared inline, IPA propagation
;; of pure/const / modref, profile propagation, cdtor merging, ICF,
;; devirtualization, IPA-CP / SRA (-O1 off), symbol visibility changes.
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../../../driver/dominance/dom.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../gimplify.rkt"
         "../cfg.rkt"
         "../ssa.rkt"
         "ssa-ops.rkt"
         "mfn.rkt"
         "loops.rkt"
         "pure-const.rkt")

(provide ipa-stage estimate-num-insns)

(define size-scale 2)
(define param-uninlined-function-insns 2)
(define param-inline-functions-called-once-insns 4000)
(define param-inline-functions-called-once-loop-depth 6)

;; ------------------------------------------------------------
;; estimate_num_insns (eni_size_weights)
;; ------------------------------------------------------------

(define MOVE-MAX-PIECES 16)
(define (type-size-bytes st ty)
  (define su (type-attr st ty 'size-unit #f))
  (cond
    [(and su (Tree? su) (eq? (tree-code su) 'integer_cst)) (integer-cst-value su)]
    [(type-attr st ty 'precision #f) => (lambda (p) (quotient (+ p 7) 8))]
    [else 8]))
(define (estimate-move-cost st ty)
  (define size (type-size-bytes st ty))
  (if (or (< size 0) (> size (* MOVE-MAX-PIECES 2))) 4 (quotient (+ size MOVE-MAX-PIECES -1) MOVE-MAX-PIECES)))
(define (operator-cost code op2)
  (case code
    [(nop_expr convert_expr complex_expr paren_expr view_convert_expr) 0]
    [(trunc_div_expr ceil_div_expr floor_div_expr round_div_expr exact_div_expr
      trunc_mod_expr ceil_mod_expr floor_mod_expr round_mod_expr rdiv_expr)
     (if (and op2 (eq? (tree-code op2) 'integer_cst)) 1 1)]
    [(bit_insert_expr) 3]
    [(cond_expr vec_cond_expr plus_expr pointer_plus_expr pointer_diff_expr minus_expr mult_expr mult_highpart_expr
      fix_trunc_expr negate_expr float_expr min_expr max_expr abs_expr absu_expr lshift_expr rshift_expr lrotate_expr rrotate_expr
      bit_ior_expr bit_xor_expr bit_and_expr bit_not_expr truth_andif_expr truth_orif_expr truth_and_expr truth_or_expr truth_xor_expr
      truth_not_expr lt_expr le_expr gt_expr ge_expr eq_expr ne_expr ordered_expr unordered_expr unlt_expr unle_expr ungt_expr unge_expr
      uneq_expr ltgt_expr conj_expr) 1]
    [else 0]))
(define (estimate-num-insns st g)
  (cond
    [(gassign? g)
     (cond
       [(tree-clobber-p (gassign-rhs1 g)) 0]
       [else
        (define lhs (gassign-lhs g)) (define rhs (gassign-rhs1 g))
        (define store? (not (is-gimple-reg st lhs)))
        (define load? (and (gassign-single-p st g)
                           (let ([r (if (handled-component-p rhs) (tree-operand rhs 0) rhs)])
                             (or (handled-component-p r) (decl-p st r) (memq (tree-code r) '(mem_ref target_mem_ref))))))
        (+ (if store? (estimate-move-cost st (tree-type lhs)) 0)
           (if load? (estimate-move-cost st (tree-type rhs)) 0)
           (operator-cost (gassign-code g) (gassign-rhs2 g)))])]
    [(gcond? g) (+ 1 (operator-cost (gcond-code g) (gcond-rhs g)))]
    [(gswitch? g) (* 2 (add1 (pvector-length (gswitch-cases g))))]
    [(gcall? g)
     (cond
       [(gcall-internal? g) 0]
       [else
        (define d (gcall-fndecl g))
        (+ (if d 1 3)
           (if (gcall-lhs g) (estimate-move-cost st (tree-type (gcall-lhs g))) 0)
           (for/sum ([a (in-pvector (gcall-args g))]) (estimate-move-cost st (tree-type a))))])]
    [(greturn? g) 1]
    [else 0]))

;; ------------------------------------------------------------
;; function summaries
;; ------------------------------------------------------------

;; sizes in size_scale units
(struct fsum (decl uncond not-inlined calls) #:transparent)   ; calls: list of (vector callee-decl call-size bb)
(define (fsum-self-size s) (quotient (+ (fsum-uncond s) (fsum-not-inlined s) (* size-scale (for/sum ([c (in-list (fsum-calls s))]) (vector-ref c 1))) 1) size-scale))
(define (fsum-min-size s) (quotient (+ (fsum-uncond s) (* size-scale (for/sum ([c (in-list (fsum-calls s))]) (vector-ref c 1))) 1) size-scale))
(define (fsum-inlined-size s) (quotient (+ (fsum-uncond s) (* size-scale (for/sum ([c (in-list (fsum-calls s))]) (vector-ref c 1))) 1) size-scale))

(define (eliminated-by-inlining-prob st fndecl g)
  (define params (pvector->list (decl-arguments fndecl)))
  (define (parm? d) (and d (decl-p st d) (eq? (tree-code d) 'parm_decl)))
  (define (unmodified-parm? t)
    (or (parm? t) (and (ssa-name? t) (ssa-default-def? t) (parm? (ssa-name-var t)))))
  (define (base t) (get-base-address* t))
  (cond
    [(greturn? g) 2]
    [(and (gassign? g) (not (gassign-rhs2 g)))
     (define code (gassign-code g))
     (define rhs (gassign-rhs1 g)) (define lhs (gassign-lhs g))
     (cond
       [(or (memq code '(nop_expr convert_expr view_convert_expr addr_expr)) (eq? (gimple-rhs-class st code) 'single))
        (define inner-rhs (or (base rhs) rhs))
        (define inner-lhs (or (base lhs) lhs))
        (define rhs-free
          (or (unmodified-parm? inner-rhs)
              (and (eq? (tree-code inner-rhs) 'addr_expr)
                   (let ([op (base (tree-operand inner-rhs 0))])
                     (or (parm? op)
                         (and (eq? (tree-code op) 'mem_ref) (unmodified-parm? (tree-operand op 0))))))))
        (cond
          [(and rhs-free (is-gimple-reg st lhs)) 2]
          [else
           (define rhs-free2 (or rhs-free (and (eq? (tree-code inner-rhs) 'mem_ref) (unmodified-parm? (tree-operand inner-rhs 0)))))
           (define lhs-free1 (and rhs-free2 (is-gimple-reg st lhs)))
           (define lhs-free
             (or lhs-free1
                 (eq? (tree-code inner-lhs) 'parm_decl) (eq? (tree-code inner-lhs) 'result_decl)
                 (and (eq? (tree-code inner-lhs) 'mem_ref)
                      (or (unmodified-parm? (tree-operand inner-lhs 0))
                          (and (ssa-name? (tree-operand inner-lhs 0)) (ssa-name-var (tree-operand inner-lhs 0))
                               (eq? (tree-code (ssa-name-var (tree-operand inner-lhs 0))) 'result_decl))))))
           (define rhs-free3 (or rhs-free2 (and lhs-free (or (is-gimple-reg st rhs) (is-gimple-min-invariant st rhs)))))
           (if (and lhs-free rhs-free3) 1 0)])]
       [else 0])]
    [else 0]))

(define (summarize st fndecl cfg)
  (define st0 (gs-set-fndecl st fndecl))
  (define uncond 0) (define not-inlined (* size-scale param-uninlined-function-insns))
  (define calls '())
  (for* ([bb (in-pvector (cfg-bbs cfg))] [g (in-pvector (GBlock-stmts (cfg-block cfg bb)))])
    (define sz (estimate-num-insns st0 g))
    (cond
      [(gcall? g)
       (when (positive? sz)
         (set! calls (append calls (list (vector (gcall-fndecl g) sz bb)))))]
      [(positive? sz)
       (define prob (eliminated-by-inlining-prob st0 fndecl g))
       (set! not-inlined (+ not-inlined (* sz prob)))
       (set! uncond (+ uncond (* sz (- 2 prob))))])
    ;; address calculations
    (for ([t (in-list (append (stmt-use-trees* g) (let ([l (stmt-get-lhs g)]) (if l (list l) '()))))])
      (let loop ([t t])
        (when (and t (Tree? t) (handled-component-p t))
          (when (and (memq (tree-code t) '(array_ref array_range_ref)) (ssa-name? (tree-operand t 1)))
            (set! uncond (+ uncond size-scale)))
          (loop (tree-operand t 0))))))
  (fsum fndecl uncond not-inlined calls))
(define (stmt-use-trees* g)
  (cond
    [(gassign? g) (filter values (list (gassign-rhs1 g) (gassign-rhs2 g) (gassign-rhs3 g)))]
    [(gcall? g) (pvector->list (gcall-args g))]
    [(gcond? g) (list (gcond-lhs g) (gcond-rhs g))]
    [(greturn? g) (filter values (list (greturn-retval g)))]
    [else '()]))

;; ------------------------------------------------------------
;; the stage
;; ------------------------------------------------------------

;; fns: list of (vector decl cfg locals ssa-version); returns
;; (values st remaining-fns plan) with plan : hash caller-uid -> hash callee-uid -> fn record
(define (ipa-stage st fns)
  (define by-uid (for/hasheqv ([f (in-list fns)]) (values (decl-uid (vector-ref f 0)) f)))
  (define sums (for/hasheqv ([f (in-list fns)]) (values (decl-uid (vector-ref f 0)) (summarize st (vector-ref f 0) (vector-ref f 1)))))
  ;; callers: callee uid -> list of (vector caller-uid call-size bb)
  (define callers (make-hasheqv))
  (define address-taken (make-hasheqv))
  (for ([f (in-list fns)])
    (define caller (decl-uid (vector-ref f 0)))
    (for ([c (in-list (fsum-calls (hash-ref sums caller)))])
      (when (vector-ref c 0)
        (hash-update! callers (decl-uid (vector-ref c 0)) (lambda (l) (append l (list (vector caller (vector-ref c 1) (vector-ref c 2))))) '())))
    ;; functions whose address is taken (ADDR_EXPR outside a call position)
    (define (scan t)
      (when (and t (Tree? t))
        (when (and (eq? (tree-code t) 'addr_expr) (eq? (tree-code (tree-operand t 0)) 'function_decl))
          (hash-set! address-taken (decl-uid (tree-operand t 0)) #t))
        (for ([o (in-pvector (tree-ops t))]) (scan o))))
    (for* ([bb (in-pvector (cfg-bbs (vector-ref f 1)))] [g (in-pvector (GBlock-stmts (cfg-block (vector-ref f 1) bb)))])
      (cond
        [(gcall? g) (for ([a (in-pvector (gcall-args g))]) (scan a)) (unless (gcall-fndecl g) (scan (gcall-fn g)))]
        [else (for ([t (in-list (stmt-use-trees* g))]) (scan t)) (let ([l (stmt-get-lhs g)]) (when l (scan l)))])))
  (define (removable? d) (and (not (tree-attr d 'public #f)) (not (hash-ref address-taken (decl-uid d) #f))))
  ;; maybe_hot_p: a call block executed at least 1/1000 of the entry
  (define (hot-edge? caller-rec bb)
    (define cfg (vector-ref caller-rec 1))
    (define c (prof-count cfg bb)) (define e (prof-count cfg ENTRY))
    (or (not c) (not e) (zero? e) (> (* c 1000) e)))
  (define (loop-depth caller-rec bb)
    (define cfg (vector-ref caller-rec 1))
    (define dom (compute-dom (pvector-cons-left (pvector-cons-left (cfg-bbs cfg) EXIT) ENTRY) ENTRY
                             (lambda (i) (for/pvector ([e (in-pvector (cfg-succs cfg i))]) (GEdge-dest e)))))
    (define-values (loops _b) (find-loops cfg dom))
    (define l (loop-of loops bb))
    (if l (Loop-depth l) 0))
  ;; want_inline_function_to_all_callers_p
  (define (want-all-callers? d cold?)
    (define uid (decl-uid d))
    (define s (hash-ref sums uid))
    (define cs (hash-ref callers uid '()))
    (cond
      [(null? cs) #f]
      [(for/or ([c (in-list cs)]) (= (vector-ref c 0) uid)) #f]          ; recursive
      [else
       ;; growth_positive_p (node, NULL, INT_MIN)
       (define offline (if (removable? d) (fsum-self-size s) 0))
       (define min-size (fsum-min-size s))
       (define edge-size (fsum-inlined-size s))
       (define min-growth (for/sum ([c (in-list cs)]) (- min-size (vector-ref c 1))))
       (define growth (for/sum ([c (in-list cs)]) (- edge-size (vector-ref c 1))))
       (cond
         [(> min-growth offline) #f]
         [(> growth offline) #f]
         [else
          ;; check_callers
          (define has-hot #f)
          (define ok
            (for/and ([c (in-list cs)])
              (define caller (hash-ref by-uid (vector-ref c 0) #f))
              (and caller
                   (<= (loop-depth caller (vector-ref c 2)) param-inline-functions-called-once-loop-depth)
                   (<= (+ (fsum-self-size (hash-ref sums (vector-ref c 0))) (- edge-size (vector-ref c 1))) param-inline-functions-called-once-insns)
                   (begin (when (hot-edge? caller (vector-ref c 2)) (set! has-hot #t)) #t))))
          (and ok (or cold? has-hot))])]))
  (define plan (make-hasheqv))                ; caller uid -> hash callee uid -> rec
  (define inlined (make-hasheqv))             ; callee uid -> #t (all callers took it)
  (for* ([cold (in-list '(#f #t))] [f (in-list (reverse fns))])
    (define d (vector-ref f 0))
    (when (and (not (hash-ref inlined (decl-uid d) #f)) (want-all-callers? d cold))
      (hash-set! inlined (decl-uid d) #t)
      (for ([c (in-list (hash-ref callers (decl-uid d) '()))])
        (hash-update! plan (vector-ref c 0) (lambda (h) (hash-set h (decl-uid d) f)) (hasheqv)))))
  ;; remove_unreachable_nodes: inlined static functions leave the unit
  (define remaining (for/list ([f (in-list fns)] #:unless (and (hash-ref inlined (decl-uid (vector-ref f 0)) #f) (removable? (vector-ref f 0)))) f))
  ;; ipa_discover_variable_flags over every body (the inlined ones included)
  (define written (make-hasheqv)) (define addr (make-hasheqv)) (define statics (make-hasheqv))
  (define (note-static! d) (when (and (var-p d) (tree-static d) (not (tree-attr d 'public #f)) (not (decl-external d))) (hash-set! statics (decl-uid d) d)))
  (define (scan-addr t)
    (when (and t (Tree? t))
      (when (and (eq? (tree-code t) 'addr_expr) (var-p (get-base-address* (tree-operand t 0))))
        (define b (get-base-address* (tree-operand t 0)))
        (note-static! b) (hash-set! addr (decl-uid b) #t))
      (for ([o (in-pvector (tree-ops t))]) (scan-addr o))))
  (for ([f (in-list fns)])
    (define cfg (vector-ref f 1))
    (for* ([bb (in-pvector (cfg-bbs cfg))] [g (in-pvector (GBlock-stmts (cfg-block cfg bb)))])
      (walk-stmt-loads-stores st g
                              (lambda (b t) (when (var-p b) (note-static! b)))
                              (lambda (b t) (when (var-p b) (note-static! b) (hash-set! written (decl-uid b) #t))))
      (for ([t (in-list (stmt-use-trees* g))]) (scan-addr t))
      (let ([l (stmt-get-lhs g)]) (when l (scan-addr l)))))
  (define st1
    (for/fold ([st st]) ([(uid d) (in-hash statics)])
      (if (and (not (hash-ref written uid #f)) (not (hash-ref addr uid #f)) (not (decl-flag st d 'readonly #f)))
          (set-decl-flag st d 'readonly #t)
          st)))
  (values st1 remaining plan))
