#lang racket/base

;; ============================================================
;; opt: RPO value numbering with elimination (gcc/tree-ssa-sccvn.cc),
;; pass_fre first instance "fre1"
;; ============================================================
;;
;; At -O1 pass_fre does not iterate (iterate_p = may_iterate && optimize
;; > 1), so this is do_rpo_vn_1 with iterate = false, eliminate = true:
;;   - blocks in the SCC-contiguous reverse postorder of
;;     rev_post_order_and_mark_dfs_back_seme (back edges marked,
;;     top-level SCC extents);
;;   - a worklist in RPO index order; an edge from a not yet visited
;;     predecessor inside the current top-level SCC counts as executable
;;     (its value is unknown);
;;   - process_bb: PHIs (visit_phi with back-edge arguments varying),
;;     statements (visit_stmt: copies, constants through
;;     gimple_fold_stmt_to_constant, n-ary / reference / call tables),
;;     eliminate_stmt inline (leaders through the rpo_elim availability
;;     lists: value -> (block . leader) pushed in RPO), the taken edge of
;;     a condition whose value is known, predicates recorded on the two
;;     edges of a condition (vn_nary_op_insert_pieces_predicated);
;;   - eliminate_cleanup: the queued statements/PHIs removed in reverse,
;;     TODO_cleanup_cfg when anything was removed or a condition folded.
;;
;; Loads and stores: references are lists of vn_reference_op records
;; (copy_reference_ops_from_ref) with SSA operands valueized and
;; addresses forwarded through `p = &x` / `p = q p+ C` definitions;
;; lookups walk the virtual use chain through non-clobbering
;; definitions (opt/alias.rkt walk_non_aliased_vuses) with
;; vn_reference_lookup_2 (the table) and the "store of an SSA name or
;; constant covering the load" case of vn_reference_lookup_3.
;;
;; Not ported: the iterating driver (-O2), unwinding, insertion of new
;; expressions (needs_insertion), the conversion / narrowing / BIT_AND
;; load / BIT_FIELD_REF / negate / shift special cases of visit_nary_op,
;; partial definitions and constant memory folding of lookup_3, calls
;; with modref summaries, PHI equivalence across different blocks
;; (vn_phi_eq with controlling conditions), value ids.
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
         "../print.rkt"
         "ssa-ops.rkt"
         "fold.rkt"
         "mfn.rkt"
         "simplify.rkt"
         "alias.rkt")

(provide fre)

(define (icst? t) (and (Tree? t) (eq? (tree-code t) 'integer_cst)))
(define VN-TOP 'vn-top)
(define (vn-top? x) (eq? x VN-TOP))

;; ------------------------------------------------------------
;; rev_post_order_and_mark_dfs_back_seme (for_iteration = true)
;; -> (values rpo-vector back-edges toplevel-scc-extents)
;;   back-edges : hash (src . dest) -> #t
;;   extents    : list of (first . last) RPO index pairs
;; ------------------------------------------------------------

(define (scc-rpo cfg entry-bb)
  (define succs (lambda (b) (for/list ([e (in-pvector (cfg-succs cfg b))]) (GEdge-dest e))))
  (define depth (make-hasheqv)) (define pre (make-hasheqv)) (define scc (make-hasheqv))
  (define scc-exits (make-hasheqv)) (define header? (make-hasheqv)) (define visited (make-hasheqv))
  (define back (make-hash))
  (define pre-num 0)
  (define post '())        ; postorder of the first DFS (reversed later)
  (define (tag-header! b h)
    (unless (or (= h -1) (= b h))
      (let loop ([cur1 b] [cur2 h])
        (define ih (hash-ref scc cur1 -1))
        (cond
          [(= ih -1) (hash-set! scc cur1 cur2)]
          [(= ih cur2) (void)]
          [(< (hash-ref depth ih) (hash-ref depth cur2))
           (hash-set! scc cur1 cur2)
           (loop cur2 ih)]
          [else (loop ih cur2)]))))
  ;; first DFS: loops (headers, scc tags), back edges
  (let find-loops ([dest entry-bb] [d 1])
    (hash-set! pre dest pre-num) (set! pre-num (add1 pre-num))
    (hash-set! depth dest d)
    (hash-set! scc dest -1)
    (hash-set! visited dest #t)
    (for ([s (in-list (succs dest))])
      (cond
        [(= s EXIT) (void)]
        [(not (hash-ref visited s #f))
         (find-loops s (add1 d))
         (define header (hash-ref scc s))
         (tag-header! dest header)]
        [else
         (cond
           [(> (hash-ref depth s) 0)
            (hash-set! back (cons dest s) #t)
            (hash-set! header? s #t)
            (hash-ref! scc-exits s (lambda () '()))
            (tag-header! dest s)]
           [(= (hash-ref scc s) -1) (void)]
           [else
            (define h0 (hash-ref scc s))
            (cond
              [(> (hash-ref depth h0) 0) (tag-header! dest h0)]
              [else
               (let loop ([header h0])
                 (when (not (= (hash-ref scc header -1) -1))
                   (define h (hash-ref scc header))
                   (if (> (hash-ref depth h) 0) (tag-header! dest h) (loop h))))])])]))
    (set! post (cons dest post))
    (hash-set! depth dest (- (hash-ref depth dest))))
  (define rpo1 (list->vector post))                 ; reverse postorder of the first DFS
  (define n (vector-length rpo1))
  (cond
    [(zero? (hash-count header?)) (values rpo1 back '())]
    [else
     ;; collect SCC exits
     (for ([bb (in-vector rpo1)])
       (hash-remove! visited bb)
       (for ([d (in-list (succs bb))] #:unless (= d EXIT))
         (define src-scc (if (hash-ref header? bb #f) bb (hash-ref scc bb)))
         (unless (= src-scc -1)
           (define dest-scc (if (hash-ref header? d #f) d (hash-ref scc d)))
           (unless (= src-scc dest-scc)
             ;; is src-scc an ancestor of dest-scc?
             (define-values (dest-depth inside?)
               (let loop ([t dest-scc] [k 0])
                 (cond [(= t -1) (values k #f)]
                       [else (define t2 (hash-ref scc t -1))
                             (if (= t2 src-scc) (values (add1 k) #t) (loop t2 (add1 k)))])))
             (unless inside?
               ;; an SCC containing src whose parent is dest-scc: exit from it
               (define found
                 (let loop ([t src-scc] [k 0])
                   (cond [(= t -1) (cons #f k)]
                         [(= (hash-ref scc t -1) dest-scc) (cons t k)]
                         [else (loop (hash-ref scc t) (add1 k))])))
               (cond
                 [(car found) (hash-update! scc-exits (car found) (lambda (l) (cons d l)) '())]
                 [else
                  (define src-depth (cdr found))
                  (let loop ([s src-scc] [sd src-depth] [t dest-scc] [td dest-depth])
                    (cond
                      [(> sd td) (loop (hash-ref scc s) (sub1 sd) t td)]
                      [(> td sd) (loop s sd (hash-ref scc t) (sub1 td))]
                      [(not (= (hash-ref scc s -1) (hash-ref scc t -1))) (loop (hash-ref scc s) sd (hash-ref scc t) td)]
                      [else (hash-update! scc-exits s (lambda (l) (cons d l)) '())]))]))))))
     ;; second DFS: SCC exits after the SCC body
     (define rpo (make-vector n #f))
     (define idx n)
     (define scc-end (make-hasheqv))
     (define estack '())      ; list of (src . dest)
     (define (push-succs! dest)
       (for ([s (in-list (reverse (succs dest)))] #:unless (= s EXIT))
         (set! estack (cons (cons dest s) estack))))
     (let dfs ([dest entry-bb])
       (hash-set! visited dest #t)
       (hash-set! scc-end dest -1)
       (cond
         [(and (hash-ref header? dest #f) (pair? (hash-ref scc-exits dest '())))
          (define exits (sort (remove-duplicates (hash-ref scc-exits dest)) < #:key (lambda (b) (hash-ref pre b))))
          (for ([x (in-list (reverse exits))]) (set! estack (cons (cons dest x) estack)))]
         [else
          (when (hash-ref header? dest #f) (hash-set! scc-end dest (sub1 idx)))
          (push-succs! dest)])
       (let loop ()
         (when (and (pair? estack) (= (car (car estack)) dest))
           (define edest (cdr (car estack)))
           (when (not (hash-ref visited edest #f)) (dfs edest))
           (set! estack (cdr estack))
           (when (and (hash-ref header? dest #f) (= (hash-ref scc-end dest) -1)
                      (or (null? estack) (not (= (car (car estack)) dest))))
             (hash-set! scc-end dest (sub1 idx))
             (push-succs! dest))
           (loop)))
       (set! idx (sub1 idx))
       (vector-set! rpo idx dest))
     (define extents
       (let loop ([i 0] [acc '()])
         (cond
           [(>= i n) (reverse acc)]
           [(hash-ref header? (vector-ref rpo i) #f)
            (define e (hash-ref scc-end (vector-ref rpo i)))
            (loop (add1 e) (cons (cons i e) acc))]
           [else (loop (add1 i) acc)])))
     (values rpo back extents)]))

;; ------------------------------------------------------------
;; pass state
;; ------------------------------------------------------------

(struct vninfo (valnum visited avail) #:mutable)     ; avail: list of (bb . leader-name), newest first
(struct V (st m cfg dom rpo bb->rpo back exec bb-exec infos nary refs phis exec-idom
             [to-remove #:mutable] [cfg-changed #:mutable] [context-bb #:mutable]))

(define (type-key v ty) (type-main-variant (V-st v) ty))
(define (ver t) (ssa-name-version t))

(define (vn-info v x)
  (hash-ref! (V-infos v) (ver x)
             (lambda () (if (ssa-default-def? x) (vninfo x #t '()) (vninfo x #f '())))))
;; SSA_VAL (x, &visited)
(define (ssa-val v x)
  (define i (hash-ref (V-infos v) (ver x) #f))
  (cond
    [(ssa-default-def? x) (values x #t)]
    [(and i (vninfo-visited i)) (values (vninfo-valnum i) #t)]
    [else (values x #f)]))
(define (SSA-VAL v x) (let-values ([(val _) (ssa-val v x)]) val))

;; ssa_undefined_value_p: a default definition of a VAR_DECL
(define (ssa-undefined-value? v x)
  (and (ssa-default-def? x) (not (ssa-virtual? x))
       (let ([d (ssa-name-var x)]) (and d (var-p d)))))

(define (expressions-equal? a b)
  (cond
    [(and (ssa-name? a) (ssa-name? b)) (ssa-name=? a b)]
    [(or (ssa-name? a) (ssa-name? b)) #f]
    [(and (vn-top? a) (vn-top? b)) #t]
    [(or (vn-top? a) (vn-top? b)) #f]
    [else (equal? a b)]))

;; set_ssa_val_to: #t when the value changed
(define (set-ssa-val-to! v from to0)
  (define st (V-st v))
  (define info (vn-info v from))
  (define currval (vninfo-valnum info))
  (define to
    (cond
      [(vn-top? to0) (if (vn-top? currval) VN-TOP from)]
      [else to0]))
  (define curr-undefined (and (ssa-name? currval) (not (ssa-virtual? currval)) (ssa-undefined-value? v currval)))
  (define curr-invariant (and (not (vn-top? currval)) (not (ssa-name? currval)) (is-gimple-min-invariant st currval)))
  (define to1
    (cond
      [(vn-top? to) to]
      [(equal? from to) to]
      [(and (not (vn-top? currval)) (not curr-invariant) (not curr-undefined) (not (ssa-name? to)) (is-gimple-min-invariant st to)) from]
      [(and (not (vn-top? currval)) (not curr-undefined) (ssa-name? to) (not (ssa-virtual? to)) (ssa-undefined-value? v to)) from]
      [else to]))
  (cond
    [(and (not (vn-top? to1)) (not (vn-top? currval)) (ssa-name? currval) (ssa-name=? currval from) (not (ssa-name=? to1 from)) #f) #f]
    [(and (not (expressions-equal? currval to1))
          (not (and curr-undefined (ssa-name? to1) (not (ssa-virtual? to1)) (ssa-undefined-value? v to1))))
     (define to2
       (if (and (not (equal? to1 from)) (not (vn-top? currval)) (not curr-undefined) curr-invariant
                (not (ssa-name? to1)) (not (vn-top? to1)) (is-gimple-min-invariant st to1))
           from
           to1))
     (set-vninfo-valnum! info to2)
     #t]
    [else #f]))

(define (defs-to-varying! v g)
  (for/fold ([ch #f]) ([d (in-list (stmt-def-names g))]) (or (set-ssa-val-to! v d d) ch)))
(define (visit-copy! v lhs rhs) (set-ssa-val-to! v lhs (SSA-VAL v rhs)))

;; ------------------------------------------------------------
;; availability (rpo_elim)
;; ------------------------------------------------------------

(define (dominates? v a b) (dom-dominates? (V-dom v) (dom-index (V-dom v) a) (dom-index (V-dom v) b)))

;; dominated_by_p_w_unex
(define (dominated-by-w-unex? v bb1 bb2)
  (or (dominates? v bb2 bb1)
      (let ([rpo2 (hash-ref (V-bb->rpo v) bb2)] [rpo1 (hash-ref (V-bb->rpo v) bb1)])
        (let loop ([curr rpo1] [limit 32])
          (cond
            [(or (<= curr rpo2) (<= limit 0)) (= curr rpo2)]
            [else (loop (vector-ref (V-exec-idom v) curr) (sub1 limit))])))))

(define (update-exec-idom! v bb)
  (define rpo (hash-ref (V-bb->rpo v) bb))
  (unless (zero? rpo)
    (define idoms (V-exec-idom v))
    (define (intersect r1 r2)
      (let loop ([r1 r1] [r2 r2])
        (cond [(= r1 r2) r1] [(> r1 r2) (loop (vector-ref idoms r1) r2)] [else (loop r1 (vector-ref idoms r2))])))
    (define idom
      (let/ec k
        (for/fold ([idom -1]) ([src (in-pvector (cfg-preds (V-cfg v) bb))])
          (define pr (hash-ref (V-bb->rpo v) src #f))
          (when (or (not pr) (>= pr rpo)) (k #f))
          (if (hash-ref (V-exec v) (cons src bb) #f)
              (if (= idom -1) pr (intersect idom pr))
              idom))))
    (when (and idom (not (= idom -1))) (vector-set! idoms rpo idom))))

;; rpo_elim::eliminate_avail
(define (eliminate-avail v bb op)
  (define-values (valnum visited) (ssa-val v op))
  (cond
    [(not visited) op]
    [(ssa-name? valnum)
     (cond
       [(ssa-default-def? valnum) valnum]
       [else
        (define vi (vn-info v valnum))
        (define av (vninfo-avail vi))
        (cond
          [(null? av) (if (vninfo-visited vi) #f valnum)]
          [(= (car (car av)) bb) (cdr (car av))]
          [else
           (or (for/first ([a (in-list av)] #:when (dominated-by-w-unex? v bb (car a))) (cdr a))
               (if (vninfo-visited vi) #f valnum))])])]
    [(vn-top? valnum) #f]
    [else valnum]))

;; rpo_elim::eliminate_push_avail
(define (eliminate-push-avail! v bb leader)
  (define valnum (vninfo-valnum (vn-info v leader)))
  (unless (or (vn-top? valnum) (not (ssa-name? valnum)))
    (define vi (vn-info v valnum))
    (set-vninfo-avail! vi (cons (cons bb leader) (vninfo-avail vi)))))

;; rpo_vn_valueize
(define (vn-valueize v name)
  (cond
    [(ssa-name? name)
     (define i (hash-ref (V-infos v) (ver name) #f))
     (define tem (and i (vninfo-valnum i)))
     (cond
       [(or (not tem) (vn-top? tem) (and (ssa-name? tem) (ssa-name=? tem name))) name]
       [(not (ssa-name? tem)) tem]
       [else (or (eliminate-avail v (V-context-bb v) tem) name)])]
    [else name]))

;; ------------------------------------------------------------
;; n-ary table
;; ------------------------------------------------------------

(struct nary (result predicated) #:mutable)    ; predicated: list of (result . list of bb) or #f

(define (canonical-ops v code ops)
  (define st (V-st v))
  (cond
    [(and (= 2 (length ops)) (or (commutative-code? code) (comparison-code? st code))
          (tree-swap-operands-p st (car ops) (cadr ops)))
     (values (if (comparison-code? st code) (swap-tree-comparison code) code) (list (cadr ops) (car ops)))]
    [else (values code ops)]))
(define (comparison-code? st code) (eq? (tree-code-class st code) 'tcc_comparison))

(define (nary-key v code type ops)
  (define vops (for/list ([o (in-list ops)]) (if (ssa-name? o) (SSA-VAL v o) o)))
  (define-values (code1 ops1) (canonical-ops v code vops))
  (list code1 (type-key v type) (for/list ([o (in-list ops1)]) (if (ssa-name? o) (cons 'ssa (ver o)) o))))

(define (nary-ops-of-stmt g)
  (case (gassign-code g)
    [(realpart_expr imagpart_expr view_convert_expr) (list (tree-operand (gassign-rhs1 g) 0))]
    [(bit_field_ref) (list (tree-operand (gassign-rhs1 g) 0) (tree-operand (gassign-rhs1 g) 1) (tree-operand (gassign-rhs1 g) 2))]
    [(constructor) (for/list ([e (in-pvector (constructor-elts (gassign-rhs1 g)))]) (if (pair? e) (cdr e) e))]
    [else (filter values (list (gassign-rhs1 g) (gassign-rhs2 g) (gassign-rhs3 g)))]))

;; vn_nary_op_lookup_pieces: (values result entry)
(define (nary-lookup v code type ops)
  (define e (hash-ref (V-nary v) (nary-key v code type ops) #f))
  (cond
    [(not e) (values #f #f)]
    [(nary-predicated e) (values #f e)]
    [else (values (nary-result e) e)]))

(define (nary-insert! v code type ops result)
  (define key (nary-key v code type ops))
  (define e (hash-ref (V-nary v) key #f))
  (cond
    [(and e (nary-predicated e)) (hash-set! (V-nary v) key (nary result #f))]
    [e (void)]
    [else (hash-set! (V-nary v) key (nary result #f))]))

;; vn_nary_op_insert_pieces_predicated
(define (nary-insert-predicated! v code type ops result dest-bb)
  (define key (nary-key v code type ops))
  (define e (hash-ref (V-nary v) key #f))
  (cond
    [(not e) (hash-set! (V-nary v) key (nary #f (list (cons result (list dest-bb)))))]
    [(not (nary-predicated e)) (void)]
    [else
     (define vals (nary-predicated e))
     (define same (assf (lambda (r) (expressions-equal? r result)) vals))
     (cond
       [(and same (for/or ([b (in-list (cdr same))]) (dominates? v b dest-bb))) (void)]
       [same (set-nary-predicated! e (cons (cons result (append (cdr same) (list dest-bb))) (remq same vals)))]
       [else (set-nary-predicated! e (append vals (list (cons result (list dest-bb)))))])]))

;; vn_nary_op_get_predicated_value
(define (nary-predicated-value v e bb [back-edge? #f])
  (cond
    [(not (nary-predicated e)) (nary-result e)]
    [else
     (for*/first ([pv (in-list (nary-predicated e))] [cand (in-list (cdr pv))]
                  #:when (if back-edge? (dominates? v cand bb) (dominated-by-w-unex? v bb cand)))
       (car pv))]))

(define (can-track-predicate-on-edge? v src dest)
  (define cfg (V-cfg v))
  (or (= 1 (pvector-length (cfg-preds cfg dest)))
      (and (not (hash-ref (V-back v) (cons src dest) #f))
           (for/and ([p (in-pvector (cfg-preds cfg dest))])
             (or (= p src) (dominates? v dest p))))))

(define (insert-related-predicates! v code ops dest)
  (define st (V-st v)) (define bool (boolean-type st))
  (define T (build-int-cst st bool 1)) (define F (build-int-cst st bool 0))
  (define (ins c r) (nary-insert-predicated! v c bool ops r dest))
  (case code
    [(lt_expr) (ins 'ne_expr T) (ins 'le_expr T) (ins 'gt_expr F) (ins 'eq_expr F)]
    [(gt_expr) (ins 'ne_expr T) (ins 'ge_expr T) (ins 'lt_expr F) (ins 'eq_expr F)]
    [(eq_expr) (ins 'lt_expr F) (ins 'gt_expr F)]
    [else (void)]))

;; insert_predicates_for_cond
(define (insert-predicates-for-cond! v code0 lhs0 rhs0 true-dest false-dest)
  (define st (V-st v)) (define bool (boolean-type st))
  (when (or true-dest false-dest)
    (define-values (code lhs rhs)
      (if (tree-swap-operands-p st lhs0 rhs0) (values (swap-tree-comparison code0) rhs0 lhs0) (values code0 lhs0 rhs0)))
    (when (ssa-name? lhs)
      (define T (build-int-cst st bool 1)) (define F (build-int-cst st bool 0))
      (define ops (list lhs rhs))
      (define icode (invert-tree-comparison code))
      (when true-dest (nary-insert-predicated! v code bool ops T true-dest))
      (when false-dest (nary-insert-predicated! v code bool ops F false-dest))
      (when icode
        (when true-dest (nary-insert-predicated! v icode bool ops F true-dest))
        (when false-dest (nary-insert-predicated! v icode bool ops T false-dest)))
      (when (integral-type-p st (tree-type lhs))
        (when true-dest (insert-related-predicates! v code ops true-dest))
        (when (and false-dest icode) (insert-related-predicates! v icode ops false-dest)))
      (when (and (integer-zerop rhs) (memq code '(ne_expr eq_expr)))
        (define dg (mfn-def-stmt (V-m v) lhs))
        (when (and dg (gassign? dg))
          (cond
            [(comparison-code? st (gassign-code dg))
             (define nlhs (vn-valueize v (gassign-rhs1 dg))) (define nrhs (vn-valueize v (gassign-rhs2 dg)))
             (define-values (nt nf) (if (eq? code 'eq_expr) (values false-dest true-dest) (values true-dest false-dest)))
             (unless (equal? lhs nlhs) (insert-predicates-for-cond! v (gassign-code dg) nlhs nrhs nt nf))]
            [(eq? (gassign-code dg) 'bit_ior_expr)
             (define e (if (eq? code 'eq_expr) true-dest false-dest))
             (for ([o (list (gassign-rhs1 dg) (gassign-rhs2 dg))])
               (define nl (vn-valueize v o))
               (unless (equal? nl lhs) (insert-predicates-for-cond! v 'eq_expr nl rhs e #f)))]
            [else (void)]))))))

;; ------------------------------------------------------------
;; references
;; ------------------------------------------------------------

;; vn_reference_op_s
(struct vro (opcode type op0 op1 op2 off) #:transparent)

;; copy_reference_ops_from_ref: a list, innermost base last
(define (copy-reference-ops v ref0)
  (define st (V-st v))
  (let loop ([ref ref0] [acc '()])
    (cond
      [(not ref) (reverse acc)]
      [else
       (define code (tree-code ref))
       (define ty (tree-type ref))
       (define-values (entries next)
         (case code
           [(mem_ref)
            (values (list (vro 'mem_ref ty (tree-operand ref 1) #f #f (mem-ref-offset ref))) (tree-operand ref 0))]
           [(bit_field_ref)
            (define bo (tree-operand ref 2))
            (values (list (vro 'bit_field_ref ty (tree-operand ref 1) bo #f
                               (if (and (icst? bo) (zero? (remainder (integer-cst-value bo) 8))) (quotient (integer-cst-value bo) 8) -1)))
                    (tree-operand ref 0))]
           [(component_ref)
            (define field (tree-operand ref 1))
            (define foff (tree-attr field 'offset #f)) (define boff (tree-attr field 'bit-offset #f))
            (define off
              (if (and (icst? foff) (icst? boff) (zero? (remainder (integer-cst-value boff) 8)))
                  (+ (integer-cst-value foff) (quotient (integer-cst-value boff) 8))
                  -1))
            (define off* (if (and (eq? (tree-code ref0) 'addr_expr)
                                  (not (let ([ts (type-size-bits st ty)]) (and ts (not (zero? ts)) (not (zero? off))))))
                             -1 off))
            (values (list (vro 'component_ref ty field (tree-operand ref 2) #f off*)) (tree-operand ref 0))]
           [(array_ref array_range_ref)
            (define arr-ty (tree-type (tree-operand ref 0)))
            (define elt-ty (type-type st arr-ty))
            (define domain (type-attr st arr-ty 'domain #f))
            (define low (or (and domain (type-attr st domain 'min #f)) (build-int-cst st (sizetype st) 0)))
            (define unit (type-attr st elt-ty 'size-unit #f))
            (define index (tree-operand ref 1))
            (define avoid-oob
              (and (eq? (tree-code ref0) 'addr_expr)
                   (not (and (icst? index)
                             (let ([ub (and domain (type-attr st domain 'max #f))])
                               (and (icst? ub) (not (integer-minus-onep ub)) (<= (integer-cst-value index) (integer-cst-value ub))))))))
            (define off
              (if (and (icst? index) (icst? low) (icst? unit) (not avoid-oob))
                  (* (- (integer-cst-value index) (integer-cst-value low)) (integer-cst-value unit))
                  -1))
            (values (list (vro code ty index low unit off)) (tree-operand ref 0))]
           [(var_decl parm_decl const_decl result_decl)
            (values (list (vro 'mem_ref ty (build-int-cst st (ptr-type st) 0) #f #f 0)
                          (vro 'addr_expr (ptr-type st) (make-tree 'addr_expr #:type (ptr-type st) #:ops (list ref)) #f #f -1))
                    #f)]
           [(string_cst integer_cst real_cst complex_cst vector_cst constructor ssa_name)
            (values (list (vro code ty ref #f #f -1)) #f)]
           [(addr_expr)
            (if (is-gimple-min-invariant st ref)
                (values (list (vro 'addr_expr ty ref #f #f -1)) #f)
                (values (list (vro 'addr_expr ty #f #f #f -1)) (tree-operand ref 0)))]
           [(realpart_expr view_convert_expr) (values (list (vro code ty #f #f #f 0)) (tree-operand ref 0))]
           [(imagpart_expr) (values (list (vro code ty #f #f #f (quotient (or (type-size-bits st ty) 0) 8))) (tree-operand ref 0))]
           [else (error 'copy-reference-ops "unexpected reference ~a" code)]))
       (loop next (append (reverse entries) acc))])))

;; vn_reference_maybe_forwprop_address / vn_reference_fold_indirect on
;; ops[i] (an SSA / ADDR base right after a MEM_REF at i-1): new ops list
(define (forwprop-address v ops i)
  (define st (V-st v)) (define m (V-m v))
  (let loop ([ops ops] [changed #f])
    (define op (list-ref ops i)) (define mem (list-ref ops (sub1 i)))
    (define (set-ops op* mem*) (append (take ops (sub1 i)) (list mem* op*) (drop ops (add1 i))))
    (cond
      [(ssa-name? (vro-op0 op))
       (define g (mfn-def-stmt m (vro-op0 op)))
       (cond
         [(not (and g (gassign? g) (memq (gassign-code g) '(addr_expr pointer_plus_expr)))) (values ops changed)]
         [(eq? (gassign-code g) 'addr_expr)
          (define-values (base boff) (get-addr-base-and-unit-offset st (tree-operand (gassign-rhs1 g) 0)))
          (cond
            [(not (and base (eq? (tree-code base) 'mem_ref))) (values ops changed)]
            [else
             (define off (+ (integer-cst-value (vro-op0 mem)) boff (mem-ref-offset base)))
             (define nop0 (let ([p (tree-operand base 0)]) (if (ssa-name? p) (SSA-VAL v p) p)))
             (define mem* (vro 'mem_ref (vro-type mem) (build-int-cst st (tree-type (vro-op0 mem)) off) #f #f off))
             (define op* (vro (if (ssa-name? nop0) 'ssa_name (tree-code nop0)) (vro-type op) nop0 #f #f -1))
             (if (ssa-name? nop0) (loop (set-ops op* mem*) #t) (values (fold-indirect v (set-ops op* mem*) i) #t))])]
         [else
          (define ptr (gassign-rhs1 g)) (define ptroff (gassign-rhs2 g))
          (cond
            [(or (not (ssa-name? ptr)) (not (icst? ptroff)) (ssa-name=? (SSA-VAL v ptr) (vro-op0 op))) (values ops changed)]
            [else
             (define off (+ (integer-cst-value (vro-op0 mem)) (integer-cst-value ptroff)))
             (define nop0 (SSA-VAL v ptr))
             (define mem* (vro 'mem_ref (vro-type mem) (build-int-cst st (tree-type (vro-op0 mem)) off) #f #f off))
             (define op* (vro (if (ssa-name? nop0) 'ssa_name (tree-code nop0)) (vro-type op) nop0 #f #f -1))
             (if (ssa-name? nop0) (loop (set-ops op* mem*) #t) (values (fold-indirect v (set-ops op* mem*) i) #t))])])]
      [else (values ops changed)])))

;; vn_reference_fold_indirect: MEM[&a.b + c] -> MEM[&a + c']
(define (fold-indirect v ops i)
  (define st (V-st v))
  (define op (list-ref ops i)) (define mem (list-ref ops (sub1 i)))
  (cond
    [(and (eq? (vro-opcode op) 'addr_expr) (vro-op0 op))
     (define inner (tree-operand (vro-op0 op) 0))
     (define-values (base boff) (get-addr-base-and-unit-offset st inner))
     (cond
       [(and base (not (equal? base inner)) (not (eq? (tree-code base) 'mem_ref)))
        (define off (+ (integer-cst-value (vro-op0 mem)) boff))
        (append (take ops (sub1 i))
                (list (vro 'mem_ref (vro-type mem) (build-int-cst st (tree-type (vro-op0 mem)) off) #f #f off)
                      (vro 'addr_expr (vro-type op) (build-fold-addr-expr st base) #f #f -1))
                (drop ops (add1 i)))]
       [else ops])]
    [else ops]))

;; valueize_refs_1 (with-avail: through vn_valueize instead of SSA_VAL)
(define (valueize-refs v ops0 [with-avail #f])
  (define (val t) (if with-avail (vn-valueize v t) (SSA-VAL v t)))
  (let loop ([i 0] [ops ops0] [changed #f])
    (cond
      [(>= i (length ops)) (values ops changed)]
      [else
       (define o (list-ref ops i))
       (define op0 (if (and (vro-op0 o) (ssa-name? (vro-op0 o))) (val (vro-op0 o)) (vro-op0 o)))
       (define op1 (if (and (vro-op1 o) (ssa-name? (vro-op1 o))) (val (vro-op1 o)) (vro-op1 o)))
       (define op2 (if (and (vro-op2 o) (ssa-name? (vro-op2 o))) (val (vro-op2 o)) (vro-op2 o)))
       (define ch (or changed (not (equal? op0 (vro-op0 o))) (not (equal? op1 (vro-op1 o))) (not (equal? op2 (vro-op2 o)))))
       (define opcode (if (and (eq? (vro-opcode o) 'ssa_name) (not (ssa-name? op0))) (tree-code op0) (vro-opcode o)))
       (define o1 (vro opcode (vro-type o) op0 op1 op2 (vro-off o)))
       (define ops1 (list-set ops i o1))
       (cond
         [(and (> i 0) op0 (eq? (tree-code op0) 'addr_expr) (eq? (vro-opcode (list-ref ops1 (sub1 i))) 'mem_ref))
          (define ops2 (fold-indirect v ops1 i))
          (loop (add1 i) ops2 (or ch (not (equal? ops2 ops1))))]
         [(and (> i 0) (eq? opcode 'ssa_name) (eq? (vro-opcode (list-ref ops1 (sub1 i))) 'mem_ref))
          (define-values (ops2 ch2) (forwprop-address v ops1 i))
          (if ch2 (loop i ops2 #t) (loop (add1 i) ops1 ch))]
         [(and (memq opcode '(array_ref array_range_ref)) (= (vro-off o1) -1)
               (icst? op0) (icst? op1) (icst? op2))
          (define o2 (vro opcode (vro-type o1) op0 op1 op2 (* (- (integer-cst-value op0) (integer-cst-value op1)) (integer-cst-value op2))))
          (loop (add1 i) (list-set ops1 i o2) ch)]
         [else (loop (add1 i) ops1 ch)])])))

;; vn_reference_eq (lexical = false), after the vuse and type checks
(define (reference-ops-equal? v ops1 ops2)
  (define (tree=? a b) (cond [(and (ssa-name? a) (ssa-name? b)) (ssa-name=? a b)] [else (equal? a b)]))
  (define (vro=? a b)
    (and (eq? (vro-opcode a) (vro-opcode b))
         (let ([x (vro-op0 a)] [y (vro-op0 b)]) (or (and (not x) (not y)) (and x y (tree=? x y))))
         (let ([x (vro-op1 a)] [y (vro-op1 b)]) (or (and (not x) (not y)) (and x y (tree=? x y))))
         (let ([x (vro-op2 a)] [y (vro-op2 b)]) (or (and (not x) (not y)) (and x y (tree=? x y))))))
  (define (deref-addr o) (let ([inner (tree-operand (vro-op0 o) 0)]) (vro (tree-code inner) (tree-type inner) inner #f #f -1)))
  (let loop ([l1 ops1] [l2 ops2])
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(or (null? l1) (null? l2)) #f]
      [else
       ;; sum the constant offsets up to the next unknown one
       (define (advance l)
         (let scan ([l l] [off 0] [deref #f])
           (cond
             [(null? l) (values l off deref #f)]
             [else
              (define o (car l))
              (define deref1 (or deref (eq? (vro-opcode o) 'mem_ref)))
              (if (= (vro-off o) -1) (values l off deref1 o) (scan (cdr l) (+ off (vro-off o)) deref1))])))
       (define-values (r1 off1 d1 o1) (advance l1))
       (define-values (r2 off2 d2 o2) (advance l2))
       (cond
         [(not (= off1 off2)) #f]
         [(and (null? r1) (null? r2)) #t]
         [(or (null? r1) (null? r2)) #f]
         [else
          (define-values (p1 dd1) (if (and d1 (eq? (vro-opcode o1) 'addr_expr) (vro-op0 o1)) (values (deref-addr o1) #f) (values o1 d1)))
          (define-values (p2 dd2) (if (and d2 (eq? (vro-opcode o2) 'addr_expr) (vro-op0 o2)) (values (deref-addr o2) #f) (values o2 d2)))
          (and (eq? dd1 dd2) (vro=? p1 p2) (loop (cdr r1) (cdr r2)))])])))

(struct vref (vuse ops type result result-vdef) #:mutable)

(define (ref-type-compatible? v t1 t2)
  (define st (V-st v))
  (cond
    [(or (not t1) (not t2)) (eqv? t1 t2)]
    [(eqv? t1 t2) #t]
    [(and (integral-type-p st t1) (integral-type-p st t2)) (= (type-precision* st t1) (type-precision* st t2))]
    [(and (integral-type-p st t1) (not (eqv? (type-precision* st t1) (type-size-bits st t1)))) #f]
    [(and (integral-type-p st t2) (not (eqv? (type-precision* st t2) (type-size-bits st t2)))) #f]
    [else (eqv? (type-size-bits st t1) (type-size-bits st t2))]))

(define (vuse-key x) (if x (ver x) 0))

;; vn_reference_lookup_1: the entry or #f
(define (reference-lookup-1 v vuse ops type)
  (for/first ([e (in-list (hash-ref (V-refs v) (vuse-key vuse) '()))]
              #:when (and (ref-type-compatible? v (vref-type e) type) (reference-ops-equal? v (vref-ops e) ops)))
    e))

(define (reference-insert! v vuse ops type result result-vdef)
  (unless (reference-lookup-1 v vuse ops type)
    (hash-update! (V-refs v) (vuse-key vuse) (lambda (l) (cons (vref vuse ops type result result-vdef) l)) '())))

;; vuse_ssa_val
(define (vuse-ssa-val v x) (and x (SSA-VAL v x)))

;; vn_reference_lookup: (values result entry last-vuse)
(define (reference-lookup v op vuse0 walk?)
  (define st (V-st v)) (define m (V-m v))
  (define vuse (vuse-ssa-val v vuse0))
  (define-values (ops _ch) (valueize-refs v (copy-reference-ops v op)))
  (define type (tree-type op))
  (define direct (reference-lookup-1 v vuse ops type))
  (cond
    [direct (values (vref-result direct) direct vuse)]
    [(not (and walk? vuse)) (values #f #f vuse)]
    [else
     (define r (make-ao-ref st op))
     (define last-vuse (box vuse))
     (define limit (box 1000))
     ;; vn_reference_lookup_2
     (define (walker vu)
       (set-box! last-vuse vu)
       (define cur (vuse-ssa-val v vu))
       (reference-lookup-1 v cur ops type))
     ;; vn_reference_lookup_3, the covering store case
     (define (translate vu)
       (define did (mfn-def m vu))
       (define g (and did (mfn-stmt m did)))
       (cond
         [(and g (gassign? g) (gassign-single-p st g) (not (ssa-name? (gassign-lhs g))) (not (stmt-has-volatile-ops? g))
               (>= (ao-ref-size r) 0) (= (ao-ref-size r) (ao-ref-max-size r))
               (is-gimple-reg-type st type))
          (define lhs (gassign-lhs g))
          (define lr (make-ao-ref st lhs))
          (define rhs (gassign-rhs1 g))
          (cond
            [(and (>= (ao-ref-max-size lr) 0) (= (ao-ref-max-size lr) (ao-ref-size lr))
                  (same-base? st (ao-ref-base r) (ao-ref-base lr))
                  (known-subrange? (ao-ref-offset r) (ao-ref-max-size r) (ao-ref-offset lr) (ao-ref-size lr))
                  (or (not (integral-type-p st type)) (= (ao-ref-size r) (type-precision* st type)))
                  (= (ao-ref-size r) (ao-ref-size lr))
                  (or (ssa-name? rhs) (is-gimple-min-invariant st rhs))
                  (useless-type-conversion-p st type (tree-type rhs)))
             (define val (if (ssa-name? rhs) (SSA-VAL v rhs) rhs))
             (reference-insert! v (unbox last-vuse) ops type val #f)
             (reference-lookup-1 v (vuse-ssa-val v (unbox last-vuse)) ops type)]
            [else 'stop])]
         [else 'stop]))
     (define res (walk-non-aliased-vuses st m (V-dom v) r vuse walker translate (lambda (x) (vuse-ssa-val v x)) limit))
     (cond
       [res (values (vref-result res) res (unbox last-vuse))]
       [else (values #f #f (unbox last-vuse))])]))

;; adjust_offsets_for_equal_base_address, the equal-base part
(define (same-base? st b1 b2)
  (cond
    [(and (eq? (tree-code b1) 'mem_ref) (eq? (tree-code b2) 'mem_ref))
     (and (equal? (tree-operand b1 0) (tree-operand b2 0)) (= (mem-ref-offset b1) (mem-ref-offset b2)))]
    [(and (decl-p st b1) (decl-p st b2)) (decl-eq? b1 b2)]
    [else (equal? b1 b2)]))

;; vn_reference_insert
(define (reference-insert v op result vuse vdef)
  (define-values (ops _ch) (valueize-refs v (copy-reference-ops v op)))
  (reference-insert! v (vuse-ssa-val v vuse) ops (tree-type op)
                     (if (ssa-name? result) (SSA-VAL v result) result) vdef))

;; calls: (fn . valueized args) as the key
(define (call-ops v g)
  (define fn (gcall-fn g))
  (cons (vro 'call_expr (gcall-fntype g) fn #f #f -1)
        (append* (for/list ([a (in-pvector (gcall-args g))])
                   (let-values ([(ops _) (valueize-refs v (copy-reference-ops v a))]) ops)))))

;; ------------------------------------------------------------
;; visitors
;; ------------------------------------------------------------

(define (vn-get-stmt-kind v g)
  (define st (V-st v))
  (cond
    [(gcall? g) 'reference]
    [(gphi? g) 'phi]
    [(gassign? g)
     (define code (gassign-code g)) (define rhs1 (gassign-rhs1 g))
     (case (gimple-rhs-class st code)
       [(unary binary ternary) 'nary]
       [(single)
        (case (tree-code-class st code)
          [(tcc_reference)
           (if (and (memq code '(realpart_expr imagpart_expr view_convert_expr bit_field_ref))
                    (let ([o (tree-operand rhs1 0)]) (or (ssa-name? o) (is-gimple-min-invariant st o))))
               'nary 'reference)]
          [(tcc_declaration) 'reference]
          [(tcc_constant) 'constant]
          [else (cond [(eq? code 'addr_expr) (if (is-gimple-min-invariant st rhs1) 'constant 'reference)]
                      [(eq? code 'constructor) 'nary]
                      [else 'none])])]
       [else 'none])]
    [else 'none]))

(define (visit-nary-op! v lhs g)
  (define type (tree-type lhs))
  (define ops (nary-ops-of-stmt g))
  (define-values (result e) (nary-lookup v (gassign-code g) type ops))
  (define result1 (or result (and e (nary-predicated-value v e (mfn-bb (V-m v) (mfn-def (V-m v) lhs))))))
  (cond
    [result1 (set-ssa-val-to! v lhs result1)]
    [else
     (define ch (set-ssa-val-to! v lhs lhs))
     (nary-insert! v (gassign-code g) type ops lhs)
     ch]))

(define (visit-reference-op-load! v lhs op g)
  (define st (V-st v))
  (define vuse (stmt-vuse g))
  (define-values (result e last-vuse) (reference-lookup v op vuse #t))
  (define result1 (and result (useless-type-conversion-p st (tree-type result) (tree-type op)) result))
  (cond
    [(and result (not result1)) (set-ssa-val-to! v lhs lhs)]
    [result1 (set-ssa-val-to! v lhs result1)]
    [else
     (define ch (set-ssa-val-to! v lhs lhs))
     (reference-insert v op lhs last-vuse #f)
     (when (and vuse last-vuse (not (ssa-name=? (SSA-VAL v last-vuse) (SSA-VAL v vuse))))
       (reference-insert v op lhs vuse #f))
     ch]))

(define (visit-reference-op-store! v lhs op0 g)
  (define st (V-st v))
  (define vuse (stmt-vuse g)) (define vdef (stmt-vdef g))
  (define op (if (ssa-name? op0) (SSA-VAL v op0) op0))
  (define-values (result e _lv) (reference-lookup v lhs vuse #f))
  (define resultsame (and e (vref-result e) (expressions-equal? (vref-result e) op)))
  (cond
    [(not resultsame)
     (define ch (if vdef (set-ssa-val-to! v vdef vdef) #f))
     (when (or (is-gimple-min-invariant st op) (ssa-name? op)) (reference-insert v lhs op vdef #f))
     ch]
    [else (set-ssa-val-to! v vdef (SSA-VAL v vuse))]))

(define (visit-reference-op-call! v lhs g)
  (define vuse (stmt-vuse g)) (define vdef (stmt-vdef g))
  (define ops (call-ops v g))
  (define vu (vuse-ssa-val v vuse))
  (define type (and lhs (tree-type lhs)))
  (define e (reference-lookup-1 v vu ops type))
  (cond
    [e
     (define ch1
       (cond
         [(not vdef) #f]
         [(vref-result-vdef e) (set-ssa-val-to! v vdef (vref-result-vdef e))]
         [(and (not lhs) (gcall-lhs g)) (set-ssa-val-to! v vdef vdef)]
         [else (set-ssa-val-to! v vdef (vuse-ssa-val v vuse))]))
     (when (and (not (vref-result e)) lhs) (set-vref-result! e lhs))
     (define ch2 (and (vref-result e) lhs (set-ssa-val-to! v lhs (vref-result e))))
     (or ch1 ch2)]
    [else
     (define ch1 (and vdef (set-ssa-val-to! v vdef vdef)))
     (define ch2 (and lhs (set-ssa-val-to! v lhs lhs)))
     (hash-update! (V-refs v) (vuse-key vu) (lambda (l) (cons (vref vu ops type lhs vdef) l)) '())
     (or ch1 ch2)]))

;; visit_phi with backedges_varying_p = #t
(define (visit-phi! v pid ph)
  (define st (V-st v)) (define cfg (V-cfg v))
  (define res (gphi-result ph))
  (define bb (mfn-bb (V-m v) pid))
  ;; preds: a non-constant argument first (trunk)
  (define args0 (pvector->list (gphi-args ph)))
  (define first-ssa (for/first ([a (in-list args0)] #:when (ssa-name? (cdr a))) a))
  (define args (if (and first-ssa (not (eq? first-ssa (car args0)))) (cons first-ssa (remq first-ssa args0)) args0))
  (define-values (sameval seen-undef seen-undef-visited n-exec backedge-val seen-non-backedge sameval-e stop?)
    (for/fold ([sameval VN-TOP] [seen-undef #f] [seen-undef-visited #f] [n 0] [backedge-val #f] [seen-nb #f] [sameval-e #f] [stop? #f])
              ([a (in-list args)])
      (define src (car a)) (define def0 (cdr a))
      (define back? (hash-ref (V-back v) (cons src bb) #f))
      (cond
        [stop? (values sameval seen-undef seen-undef-visited n backedge-val seen-nb sameval-e #t)]
        [(not (hash-ref (V-exec v) (cons src bb) #f)) (values sameval seen-undef seen-undef-visited n backedge-val seen-nb sameval-e #f)]
        [(and (ssa-name? def0) (ssa-name=? def0 res)) (values sameval seen-undef seen-undef-visited n backedge-val seen-nb sameval-e #f)]
        [else
         (define-values (def visited)
           (cond
             [(ssa-name? def0)
              (define-values (val vis) (ssa-val v def0))
              (values (if back? def0 val) (or vis (ssa-default-def? def0)))]
             [else (values def0 #t)]))
         (define backedge-val1 (if (and back? (ssa-name? def0)) def backedge-val))
         (define seen-nb1 (or seen-nb (not back?)))
         (define n1 (add1 n))
         (cond
           [(vn-top? def) (values sameval seen-undef seen-undef-visited n1 backedge-val1 seen-nb1 sameval-e #f)]
           [(and (ssa-name? def) (not (ssa-virtual? def)) (ssa-undefined-value? v def))
            (if (or (not seen-undef) (and (not seen-undef-visited) visited))
                (values sameval def visited n1 backedge-val1 seen-nb1 sameval-e #f)
                (values sameval seen-undef seen-undef-visited n1 backedge-val1 seen-nb1 sameval-e #f))]
           [(vn-top? sameval) (values def seen-undef seen-undef-visited n1 backedge-val1 seen-nb1 src #f)]
           [(expressions-equal? def sameval) (values sameval seen-undef seen-undef-visited n1 backedge-val1 seen-nb1 #f #f)]
           [(and (ssa-name? def) (ssa-virtual? def)) (values #f seen-undef seen-undef-visited n1 backedge-val1 seen-nb1 sameval-e #t)]
           [else
            ;; a recorded equality on the edge (predicated EQ)
            (define eq-known?
              (and (not (scalar-float-type-p st (tree-type def)))
                   (or (not (ssa-name? sameval)) (ssa-name=? (SSA-VAL v sameval) sameval))
                   (or (not (ssa-name? def)) (ssa-name=? (SSA-VAL v def) def))
                   (let-values ([(val e) (nary-lookup v 'eq_expr (boolean-type st) (list def sameval))])
                     (and (not val) e (nary-predicated e)
                          (let ([pv (nary-predicated-value v e src back?)])
                            (and pv (icst? pv) (not (integer-zerop pv))
                                 (not (and sameval-e (hash-ref (V-back v) (cons sameval-e bb) #f)))))))))
            (if eq-known?
                (values sameval seen-undef seen-undef-visited n1 backedge-val1 seen-nb1 sameval-e #f)
                (values #f seen-undef seen-undef-visited n1 backedge-val1 seen-nb1 sameval-e #t))])])))
  (define result
    (cond
      [(or (and backedge-val (not seen-non-backedge) (ssa-name? backedge-val) (and sameval (not (vn-top? sameval)) (expressions-equal? sameval backedge-val))
                (or (ssa-virtual? backedge-val) (not (ssa-name=? (SSA-VAL v backedge-val) backedge-val))))
           (and sameval (not (vn-top? sameval)) (ssa-name? sameval) (not (ssa-default-def? sameval)) (ssa-virtual? sameval)
                (let-values ([(_ vis) (ssa-val v sameval)]) (not vis))))
       res]
      [(<= n-exec 1) (or seen-undef sameval)]
      [(vn-top? sameval) (if (and seen-undef seen-undef-visited) seen-undef sameval)]
      [(phi-lookup v pid ph) => values]
      [(and sameval (or (not seen-undef) (is-gimple-min-invariant st sameval))) sameval]
      [else
       (phi-insert! v pid ph res)
       res]))
  (set-ssa-val-to! v res (if (not result) res result)))

;; PHIs are equivalent within their block when their valueized arguments agree
(define (phi-key v pid ph)
  (define bb (mfn-bb (V-m v) pid))
  (cons bb
        (for/list ([a (in-pvector (gphi-args ph))])
          (define d (cdr a)) (define back? (hash-ref (V-back v) (cons (car a) bb) #f))
          (define val
            (cond
              [(and (ssa-name? d) (not back?))
               (if (and (not (ssa-virtual? d)) (ssa-undefined-value? v d)) VN-TOP (SSA-VAL v d))]
              [else d]))
          (cons (car a) (if (ssa-name? val) (cons 'ssa (ver val)) val)))))
(define (phi-lookup v pid ph)
  (define e (hash-ref (V-phis v) (phi-key v pid ph) #f))
  (and e (eqv? (type-key v (tree-type (gphi-result ph))) (car e)) (cdr e)))
(define (phi-insert! v pid ph result)
  (hash-set! (V-phis v) (phi-key v pid ph) (cons (type-key v (tree-type (gphi-result ph))) result)))

(define (try-to-simplify v g)
  (define st (V-st v))
  (cond
    [(eq? (gassign-code g) 'ssa_name) #f]
    [else
     (define tem (gimple-fold-stmt-to-constant st (V-m v) g (lambda (t) (vn-valueize v t))))
     (and tem (or (ssa-name? tem) (is-gimple-min-invariant st tem)) tem)]))

(define (visit-stmt! v id g)
  (define st (V-st v))
  (cond
    [(gphi? g) (visit-phi! v id g)]
    [(stmt-has-volatile-ops? g) (defs-to-varying! v g)]
    [(gassign? g)
     (define code (gassign-code g)) (define lhs (gassign-lhs g)) (define rhs1 (gassign-rhs1 g))
     (cond
       [(and (eq? code 'ssa_name) (ssa-name? lhs)) (visit-copy! v lhs rhs1)]
       [else
        (define simplified (try-to-simplify v g))
        (cond
          [(and simplified (not (ssa-name? simplified)) (is-gimple-min-invariant st simplified) (ssa-name? lhs))
           (set-ssa-val-to! v lhs simplified)]
          [(and simplified (ssa-name? simplified) (ssa-name? lhs)) (visit-copy! v lhs simplified)]
          [(or (eq? (tree-code-class st (tree-code lhs)) 'tcc_reference) (decl-p st lhs))
           (visit-reference-op-store! v lhs rhs1 g)]
          [(ssa-name? lhs)
           (cond
             [(and (gassign-single-p st g) (is-gimple-min-invariant st rhs1)) (set-ssa-val-to! v lhs rhs1)]
             [else
              (case (vn-get-stmt-kind v g)
                [(nary) (visit-nary-op! v lhs g)]
                [(reference) (visit-reference-op-load! v lhs rhs1 g)]
                [else (defs-to-varying! v g)])])]
          [else (defs-to-varying! v g)])])]
    [(gcall? g)
     (define lhs (gcall-lhs g))
     (define ecf (call-ecf (V-st v) g))
     (cond
       [(or (not (zero? (bitwise-and ecf (bitwise-ior ECF-PURE ECF-CONST)))) (stmt-vdef g))
        (visit-reference-op-call! v (and (ssa-name? lhs) lhs) g)]
       [else (defs-to-varying! v g)])]
    [else (defs-to-varying! v g)]))

;; ------------------------------------------------------------
;; elimination (eliminate_dom_walker::eliminate_stmt, inline)
;; ------------------------------------------------------------

(define (queue-remove! v id) (set-V-to-remove! v (cons id (V-to-remove v))))

(define (block-view v bb)
  (define m (V-m v))
  (set-block (V-cfg v) (block-set-stmts (cfg-block (V-cfg v) bb) (for/pvector ([id (in-list (mfn-stmts m bb))]) (mfn-stmt m id)))))

(define (eliminate-stmt! v bb id)
  (define st (V-st v)) (define m (V-m v))
  (define g (mfn-stmt m id))
  (define lhs (stmt-get-lhs g))
  (define (leader-of use)
    (if (ssa-default-def? use)
        (eliminate-avail v bb use)
        (let ([did (mfn-def m use)]) (and did (eliminate-avail v (mfn-bb m did) use)))))
  (define done?
    (cond
      [(and lhs (ssa-name? lhs) (not (stmt-has-volatile-ops? g)))
       (define sprime (eliminate-avail v bb lhs))
       (cond
         [(not sprime) #f]
         [(may-propagate-copy st lhs sprime) (queue-remove! v id) #t]
         [(and (or (gassign-single-p st g)
                   (and (gassign? g) (memq (gassign-code g) '(nop_expr convert_expr view_convert_expr))))
               (equal? sprime (gassign-rhs1 g)))
          #t]
         [else
          ;; propagate_tree_value_into_stmt
          (define sp (if (useless-type-conversion-p st (tree-type lhs) (tree-type sprime)) sprime (fold-convert st (tree-type lhs) sprime)))
          (mfn-replace! m id (gassign (stmt-loc g) (stmt-flags g) lhs (tree-code sp) sp #f #f))
          #t])]
      [else #f]))
  (unless done?
    ;; a redundant store: the location already holds the value
    (define g1 (mfn-stmt m id))
    (define redundant?
      (and (gassign? g1) (gassign-single-p st g1) (not (stmt-has-volatile-ops? g1))
           (not (ssa-name? (gassign-lhs g1))) (not (is-gimple-reg st (gassign-lhs g1)))
           (or (ssa-name? (gassign-rhs1 g1)) (is-gimple-min-invariant st (gassign-rhs1 g1)))
           (let ([lhs1 (gassign-lhs g1)])
             (and (or (not (integral-type-p st (tree-type lhs1)))
                      (eqv? (type-precision* st (tree-type lhs1)) (type-size-bits st (tree-type lhs1))))
                  (let-values ([(val e _lv) (reference-lookup v lhs1 (stmt-vuse g1) #t)])
                    (define rhs (let ([r (gassign-rhs1 g1)]) (if (ssa-name? r) (vninfo-valnum (vn-info v r)) r)))
                    (and val (expressions-equal? val rhs)))))))
    (cond
      [redundant? (queue-remove! v id)]
      [else
       ;; a condition with one executable successor
       (define succs (cfg-succs (V-cfg v) bb))
       (define g2 (mfn-stmt m id))
       (define cond-fixed?
         (and (gcond? g2) (= 2 (pvector-length succs))
              (let* ([e0 (pvector-ref succs 0)] [e1 (pvector-ref succs 1)]
                     [x0 (hash-ref (V-exec v) (cons bb (GEdge-dest e0)) #f)]
                     [x1 (hash-ref (V-exec v) (cons bb (GEdge-dest e1)) #f)])
                (and (not (eq? (and x0 #t) (and x1 #t)))
                     (let ([true? (eq? (not (zero? (bitwise-and (GEdge-flags e0) EDGE-TRUE-VALUE))) (and x0 #t))])
                       (mfn-replace! m id (gcond (gcond-loc g2) (gcond-flags g2) 'ne_expr
                                                 (build-int-cst st (boolean-type st) (if true? 1 0)) (build-int-cst st (boolean-type st) 0)
                                                 (gcond-true-label g2) (gcond-false-label g2)))
                       (set-V-cfg-changed! v #t)
                       #t)))))
       (unless cond-fixed?
         ;; uses through their leaders, then fold
         (define modified #f)
         (define g3
           (stmt-map-uses st g2 (lambda (use)
                                  (define sprime (leader-of use))
                                  (cond
                                    [(and sprime (not (equal? sprime use)) (may-propagate-copy st use sprime)) (set! modified #t) sprime]
                                    [else use]))))
         (when modified
           (mfn-replace! m id g3)
           (fold-stmt st m id values)
           (define g4 (mfn-stmt m id))
           (when (or (and (gcond? g4) (icst? (gcond-lhs g4)) (icst? (gcond-rhs g4)))
                     (and (gswitch? g4) (icst? (gswitch-index g4))))
             (set-V-cfg-changed! v #t)))
         ;; definitions become available
         (define g5 (mfn-stmt m id))
         (for ([d (in-list (stmt-def-names g5))] #:unless (ssa-virtual? d))
           (unless (mfn-zero-uses? m d) (eliminate-push-avail! v bb d))))])))

;; ------------------------------------------------------------
;; process_bb (iterate = false, eliminate = true)
;; ------------------------------------------------------------

(define (mark-exec! v src dest)
  (hash-set! (V-exec v) (cons src dest) #t)
  (hash-set! (V-bb-exec v) dest #t))

(define (process-bb! v bb)
  (define st (V-st v)) (define m (V-m v)) (define cfg (V-cfg v))
  (set-V-context-bb! v bb)
  (update-exec-idom! v bb)
  ;; PHIs
  (for ([pid (in-list (mfn-phis m bb))])
    (define ph (mfn-stmt m pid))
    (define res (gphi-result ph))
    (define info (vn-info v res))
    (set-vninfo-valnum! info VN-TOP) (set-vninfo-visited! info #t)
    (visit-stmt! v pid ph)
    (unless (ssa-virtual? res)
      (define val (vninfo-valnum info))
      (define removed?
        (and (not (expressions-equal? res val))
             (let ([leader (eliminate-avail v bb res)])
               (and leader (not (equal? leader res)) (may-propagate-copy st res leader)
                    (begin (queue-remove! v pid) #t)))))
      (unless removed?
        (when (or (expressions-equal? res val) (not (eliminate-avail v bb res)))
          (eliminate-push-avail! v bb res)))))
  (define ids (mfn-stmts m bb))
  (when (null? ids)
    (for ([e (in-pvector (cfg-succs cfg bb))]) (mark-exec! v bb (GEdge-dest e))))
  (for ([id (in-list ids)] [k (in-naturals)])
    (define g (mfn-stmt m id))
    (for ([d (in-list (stmt-def-names g))])
      (define info (vn-info v d))
      (set-vninfo-valnum! info VN-TOP) (set-vninfo-visited! info #t))
    (visit-stmt! v id g)
    (define last (mfn-stmt m id))
    (define taken
      (cond
        [(gswitch? last)
         (define idx (vn-valueize v (gswitch-index last)))
         (and (icst? idx) (find-taken-edge st (block-view v bb) bb idx))]
        [(gcond? last)
         (define lhs0 (vn-valueize v (gcond-lhs last))) (define rhs0 (vn-valueize v (gcond-rhs last)))
         (define-values (code lhs rhs)
           (if (tree-swap-operands-p st lhs0 rhs0) (values (swap-tree-comparison (gcond-code last)) rhs0 lhs0) (values (gcond-code last) lhs0 rhs0)))
         (define val0
           (let ([r (simplify-cond st m (lambda (t) (vn-valueize v t)) code lhs rhs)])
             (and r (icst? (cadr r)) (icst? (caddr r)) (fold-const-compare st (car r) (boolean-type st) (cadr r) (caddr r)))))
         (define val
           (cond
             [val0 val0]
             [else
              (define-values (v1 e1) (nary-lookup v code (boolean-type st) (list lhs rhs)))
              (define-values (v2 e2)
                (if (and v1 (ssa-name? v1)) (nary-lookup v 'ne_expr (boolean-type st) (list v1 (build-int-cst st (tree-type v1) 0))) (values v1 e1)))
              (cond
                [v2 v2]
                [(and e2 (nary-predicated e2)) (nary-predicated-value v e2 bb)]
                [else #f])]))
         (define e (and val (icst? val) (find-taken-edge st (block-view v bb) bb val)))
         (unless e
           (define-values (te fe) (extract-true-false-edges cfg bb))
           (define td (and (can-track-predicate-on-edge? v bb (GEdge-dest te)) (GEdge-dest te)))
           (define fd (and (can-track-predicate-on-edge? v bb (GEdge-dest fe)) (GEdge-dest fe)))
           (insert-predicates-for-cond! v code lhs rhs td fd))
         e]
        [else #f]))
    (cond
      [taken (set-V-cfg-changed! v #t) (mark-exec! v bb (GEdge-dest taken))]
      [(= k (sub1 (length ids))) (for ([e (in-pvector (cfg-succs cfg bb))]) (mark-exec! v bb (GEdge-dest e)))])
    (eliminate-stmt! v bb id))
  ;; successor PHI arguments through leaders
  (for* ([e (in-pvector (cfg-succs cfg bb))] [pid (in-list (mfn-phis m (GEdge-dest e)))])
    (define ph (mfn-stmt m pid))
    (define args*
      (for/pvector ([a (in-pvector (gphi-args ph))])
        (define arg (cdr a))
        (cond
          [(and (= (car a) bb) (ssa-name? arg) (not (ssa-virtual? arg)))
           (define sprime
             (if (ssa-default-def? arg) (SSA-VAL v arg)
                 (let ([did (mfn-def m arg)]) (and did (eliminate-avail v (mfn-bb m did) arg)))))
           (if (and sprime (not (equal? sprime arg)) (may-propagate-copy st arg sprime)) (cons (car a) sprime) a)]
          [else a])))
    (unless (equal? args* (gphi-args ph)) (mfn-replace! m pid (phi-set-args ph args*))))
  (set-V-context-bb! v #f))

;; ------------------------------------------------------------
;; eliminate_cleanup: remove the queued statements (reverse order)
;; ------------------------------------------------------------

(define (eliminate-cleanup! v)
  (define m (V-m v))
  (define any? (pair? (V-to-remove v)))
  (for ([id (in-list (V-to-remove v))])          ; newest first = GCC's pop order
    (mfn-unlink-vdef! m id)
    (mfn-remove! m id))
  (when any? (set-V-cfg-changed! v #t)))

;; ------------------------------------------------------------
;; the driver (do_rpo_vn_1, iterate = false)
;; ------------------------------------------------------------

(define (fre st fndecl cfg0)
  ;; do_rpo_vn_1: VN_TOP = create_tmp_var_raw (void_type_node, "vn_top")
  ;; takes a temporary name number (create_tmp_var_name)
  (define st0 (let-values ([(_n s) (fresh-tmp-var-id (gs-set-fndecl st fndecl))]) s))
  (define cfg (cfg-clear-messages cfg0))
  (define m (gcfg->mfn cfg))
  (define dom (compute-dom (pvector-cons-left (pvector-cons-left (cfg-bbs cfg) EXIT) ENTRY) ENTRY
                           (lambda (i) (for/pvector ([e (in-pvector (cfg-succs cfg i))]) (GEdge-dest e)))))
  (define entry-bb (GEdge-dest (pvector-first (cfg-succs cfg ENTRY))))
  (define-values (rpo back extents) (scc-rpo cfg entry-bb))
  (define n (vector-length rpo))
  (define bb->rpo (for/hasheqv ([b (in-vector rpo)] [i (in-naturals)]) (values b i)))
  (define exec-idom (make-vector (max n 1) -1))
  (for ([i (in-range 1 n)])
    (define bb (vector-ref rpo i))
    (vector-set! exec-idom i (hash-ref bb->rpo (dom-node dom (dom-idom dom (dom-index dom bb))) 0)))
  (define v (V st0 m cfg dom rpo bb->rpo back (make-hash) (make-hasheqv) (make-hasheqv) (make-hash) (make-hasheqv) (make-hash) exec-idom
               '() #f #f))
  ;; max_rpo: the end of the enclosing top-level SCC
  (define max-rpo (for/vector ([i (in-range n)]) i))
  (for ([ext (in-list extents)])
    (for ([i (in-range (car ext) (add1 (cdr ext)))]) (vector-set! max-rpo i (cdr ext))))
  (hash-set! (V-exec v) (cons ENTRY entry-bb) #t)
  (hash-set! (V-bb-exec v) entry-bb #t)
  (define visited (make-vector n #f))
  (let loop ([worklist (intbits-set intbits-empty 0)])
    (unless (intbits-empty? worklist)
      (define idx (intbits-first worklist))
      (define bb (vector-ref rpo idx))
      ;; back edges from not yet visited blocks of the SCC are assumed executable
      (for ([src (in-pvector (cfg-preds cfg bb))])
        (define sr (hash-ref bb->rpo src #f))
        (when (and (not (hash-ref (V-exec v) (cons src bb) #f))
                   (or (= bb entry-bb)
                       (and sr (not (vector-ref visited sr)) (>= (vector-ref max-rpo sr) idx))))
          (hash-set! (V-exec v) (cons src bb) #t)))
      (process-bb! v bb)
      (vector-set! visited idx #t)
      (loop (for/fold ([wl (intbits-clear worklist idx)]) ([e (in-pvector (cfg-succs cfg bb))])
              (define d (GEdge-dest e))
              (define di (hash-ref bb->rpo d #f))
              (if (and (hash-ref (V-exec v) (cons bb d) #f) (not (= d EXIT)) di (not (vector-ref visited di)))
                  (intbits-set wl di)
                  wl)))))
  (eliminate-cleanup! v)
  (define cfg1 (mfn->gcfg m))
  (if (V-cfg-changed v) (cleanup-tree-cfg st0 cfg1 print-cond-rhs) (values cfg1 st0)))
