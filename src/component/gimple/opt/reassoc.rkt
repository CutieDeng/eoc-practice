#lang racket/base

;; ============================================================
;; opt: pass_reassoc (gcc/tree-ssa-reassoc.cc), the serial rewrite
;; ============================================================
;;
;; (reassoc-pass early?) -> (st fndecl cfg -> (values cfg st))
;;   EARLY? is the pass parameter (reassoc1: insert_powi_p, no
;;   loop-carried PHI bias; reassoc2: the bias)
;;
;; init_reassoc: default definitions get ranks 3, 4, ... walking the SSA
;; versions downwards, then every block (reverse post order) gets
;; ++rank << 16.  get_rank: a PHI has its block's rank (or the latch
;; rank + PHI_LOOP_BIAS for an accumulator in an innermost loop header
;; when biasing), an assignment 1 + the maximum rank of its SSA uses,
;; other definitions the block rank, constants 0.
;; do_reassoc: break_up_subtract_bb over the dominator tree (it numbers
;; the statements of each block: gimple_uid), then reassociate_bb over
;; the post-dominator tree, each block backwards: a binary statement of
;; an associative code on a type that may be reassociated is
;; linearized (linearize_expr_tree with the operand swap and
;; linearize_expr), the operands sorted (sort_by_operand_rank),
;; constants merged (optimize_ops_list's first rule and
;; eliminate_using_constants), the three-operand swap
;; (swap_ops_for_binary_stmt) applied, and the chain rewritten
;; (rewrite_expr_tree: a changed inner statement gets a new SSA name,
;; placed by find_insert_point); visited statements left without uses
;; are removed and their names released.
;;
;; Not ported (none fires on the fixtures): maybe_optimize_range_tests
;; / optimize_range_tests, undistribute_ops_list, the bit-ref vector
;; case, transform_add_to_multiply, the duplicate / not / plus-minus /
;; redundant-comparison pair eliminations, powi and copysign,
;; rank_ops_for_fma, rewrite_expr_tree_parallel (reassoc2 with a width
;; above 1), the breaking up of subtractions into negated additions and
;; repropagate_negates (no subtraction in an associative context on the
;; fixtures).
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../../../driver/dominance/dom.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         (only-in "../gimplify.rkt" is-gimple-min-invariant)
         "../cfg.rkt"
         (only-in "../print.rkt" print-cond-rhs)
         (only-in "../ssa.rkt" stmt-vdef)
         "ssa-ops.rkt"
         "mfn.rkt"
         "loops.rkt"
         (only-in "fold.rkt" fold-const-binop type-unsigned?* type-precision*))

(provide reassoc-pass)

(define PHI-LOOP-BIAS (arithmetic-shift 1 15))
(define (icst? t) (and (Tree? t) (eq? (tree-code t) 'integer_cst)))

;; an operand entry
(struct oe (op rank id) #:transparent)

(define ((reassoc-pass early?) st0 fndecl cfg0)
  (define bias? (not early?))
  (define st (gs-set-fndecl st0 fndecl))
  (define cfg (cfg-clear-messages cfg0))
  (define m (gcfg->mfn cfg))
  (define (succs-of bb) (for/pvector ([e (in-pvector (cfg-succs cfg bb))]) (GEdge-dest e)))
  (define all-nodes (pvector-cons-left (pvector-cons-left (cfg-bbs cfg) EXIT) ENTRY))
  (define dom (compute-dom all-nodes ENTRY succs-of))
  (define pdom (compute-post-dom all-nodes EXIT succs-of))
  (define-values (loops _b) (find-loops cfg dom))
  (define (loop-containing bb) (or (loop-of loops bb) 'root))
  (define (inside? l bb) (or (eq? l 'root) (loop-inside? l bb)))

  ;; ---- ranks ----------------------------------------------------------
  (define bb-rank (make-hasheqv))
  (define op-rank (make-hasheqv))                ; version -> rank
  (define biased (make-hasheqv))
  (define names (ssa-names-of cfg))
  (define rank0
    (for/fold ([rank 2]) ([n (in-list (sort (filter (lambda (n) (tree-attr n 'default-def #f)) names) > #:key ssa-name-version))])
      (hash-set! op-rank (ssa-name-version n) (add1 rank))
      (add1 rank)))
  (for/fold ([rank rank0]) ([bb (in-pvector (dom-rpo dom))] #:unless (or (= bb ENTRY) (= bb EXIT)))
    (hash-set! bb-rank bb (arithmetic-shift (add1 rank) 16))
    (add1 rank))

  (define (def-bb n) (let ([id (mfn-def m n)]) (and id (mfn-bb m id))))
  (define (phi-rank id)
    (define bb (mfn-bb m id))
    (define base (hash-ref bb-rank bb))
    (define l (loop-of loops bb))
    (cond
      [(not bias?) base]
      [(not (and l (= 1 (length (Loop-latches l))))) base]
      [(or (not (= bb (Loop-header l))) (for/or ([o (in-list loops)]) (eqv? (Loop-parent o) (Loop-num l)))) base]
      [else
       (define ph (mfn-stmt m id))
       (define res (gphi-result ph))
       (define uses (mfn-use-ids m res))
       (cond
         [(ssa-virtual? res) base]
         [(not (and (mfn-single-use? m res) (= 1 (length uses))
                    (eq? (loop-containing (mfn-bb m (car uses))) l)))
          base]
         [(for/or ([a (in-pvector (gphi-args ph))])
            (define arg (cdr a))
            (and (ssa-name? arg) (not (ssa-default-def? arg))
                 (let ([db (def-bb arg)]) (and db (eq? (loop-containing db) l)))))
          (+ (hash-ref bb-rank (car (Loop-latches l))) PHI-LOOP-BIAS)]
         [else base])]))
  (define (propagate-bias? id)
    (define g (mfn-stmt m id))
    (cond
      [(eq? (tree-code-class st (gassign-code g)) 'tcc_reference) #f]
      [else
       (define users
         (for/list ([u (in-list (mfn-use-ids m (gassign-lhs g)))]
                    #:when (let ([s (mfn-stmt m u)]) (and (gassign? s) (ssa-name? (gassign-lhs s)))))
           u))
       (and (= 1 (length (remove-duplicates users)))
            (eq? (loop-containing (mfn-bb m id)) (loop-containing (mfn-bb m (car users)))))]))
  (define (get-rank e)
    (cond
      [(not (ssa-name? e)) 0]
      [(hash-ref op-rank (ssa-name-version e) #f) => values]
      [else
       (define id (mfn-def m e))
       (define g (and id (mfn-stmt m id)))
       (define rank
         (cond
           [(not g) 0]
           [(gphi? g)
            (define r (phi-rank id))
            (unless (= r (hash-ref bb-rank (mfn-bb m id))) (hash-set! biased (ssa-name-version e) #t))
            r]
           [(not (gassign? g)) (hash-ref bb-rank (mfn-bb m id))]
           [else
            (define track (propagate-bias? id))
            (define b? #f)
            (define r
              (for/fold ([r 0]) ([op (in-list (stmt-real-uses g))])
                (define opr (get-rank op))
                (cond
                  [(hash-ref biased (ssa-name-version op) #f) (if track (begin (set! b? #t) (max r opr)) r)]
                  [else (max r opr)])))
            (when b? (hash-set! biased (ssa-name-version e) #t))
            (add1 r)]))
       (hash-set! op-rank (ssa-name-version e) rank)
       rank]))

  ;; ---- statement order (gimple_uid) and visited flags ----------------
  (define uid (make-hasheqv))
  (define visited (make-hasheqv))
  (for ([bb (in-pvector (cfg-bbs cfg))])
    (for ([id (in-list (mfn-stmts m bb))] [k (in-naturals 1)]) (hash-set! uid id k)))
  (define (stmt-dominates? a b)        ; reassoc_stmt_dominates_stmt_p over ids (#f = default def)
    (cond
      [(or (not a) (eqv? a b)) #t]
      [(not b) #f]
      [else
       (define ba (mfn-bb m a)) (define bb (mfn-bb m b))
       (cond
         [(= ba bb)
          (cond
            [(mfn-phi? m a) #t]
            [(mfn-phi? m b) #f]
            [(< (hash-ref uid a) (hash-ref uid b)) #t]
            [(> (hash-ref uid a) (hash-ref uid b)) #f]
            [else (let loop ([x (mfn-next m a)])
                    (cond [(not x) #f]
                          [(not (= (hash-ref uid x 0) (hash-ref uid a))) #f]
                          [(= x b) #t]
                          [else (loop (mfn-next m x))]))])]
         [else (dom-dominates? dom (dom-index dom ba) (dom-index dom bb))])]))

  (define (reassociable-type? ty)
    (and (integral-type-p st ty) (type-unsigned?* st ty) (not (pointer-type-p st ty))))
  (define (reassociable-op? id code l)
    (define g (mfn-stmt m id))
    (and (gassign? g) (inside? l (mfn-bb m id)) (eq? (gassign-code g) code)
         (ssa-name? (gassign-lhs g)) (mfn-single-use? m (gassign-lhs g))))

  ;; ---- operand vectors ------------------------------------------------
  (define next-id 0)
  (define (new-oe op) (begin0 (oe op (get-rank op) next-id) (set! next-id (add1 next-id))))
  (define (constant-type t) (if (integral-type-p st (tree-type t)) 1 3))
  (define (oe<? a b)                    ; sort_by_operand_rank: A before B
    (define c
      (cond
        [(not (= (oe-rank a) (oe-rank b))) (if (> (oe-rank b) (oe-rank a)) 1 -1)]
        [(= (oe-rank a) 0)
         (if (not (= (constant-type (oe-op a)) (constant-type (oe-op b))))
             (- (constant-type (oe-op a)) (constant-type (oe-op b)))
             (if (> (oe-id b) (oe-id a)) 1 -1))]
        [(not (ssa-name? (oe-op a))) (if (not (ssa-name? (oe-op b))) (if (> (oe-id b) (oe-id a)) 1 -1) 1)]
        [(not (ssa-name? (oe-op b))) -1]
        [(not (= (ssa-name-version (oe-op a)) (ssa-name-version (oe-op b))))
         (define sa (mfn-def m (oe-op a))) (define sb (mfn-def m (oe-op b)))
         (define ba (and sa (mfn-bb m sa))) (define bb (and sb (mfn-bb m sb)))
         (cond
           [(and (not (eqv? ba bb)) (not ba)) 1]
           [(and (not (eqv? ba bb)) (not bb)) -1]
           [(and (not (eqv? ba bb)) (not (= (hash-ref bb-rank bb) (hash-ref bb-rank ba))))
            (- (arithmetic-shift (hash-ref bb-rank bb) -16) (arithmetic-shift (hash-ref bb-rank ba) -16))]
           [else
            (define da (stmt-dominates? sa sb)) (define db (stmt-dominates? sb sa))
            (cond [(not (eq? da db)) (if da 1 -1)]
                  [else (if (> (ssa-name-version (oe-op b)) (ssa-name-version (oe-op a))) 1 -1)])])]
        [else (if (> (oe-id b) (oe-id a)) 1 -1)]))
    ;; qsort comparator: negative = A first
    (< c 0))

  (define (swap-operands! id)
    (define g (mfn-stmt m id))
    (mfn-replace! m id (gassign (gassign-loc g) (gassign-flags g) (gassign-lhs g) (gassign-code g)
                                (gassign-rhs2 g) (gassign-rhs1 g) #f)))

  ;; linearize_expr_tree -> list of oe (in push order)
  (define (linearize! id ops)
    (hash-set! visited id #t)
    (define g (mfn-stmt m id))
    (define code (gassign-code g))
    (define l (loop-containing (mfn-bb m id)))
    (define (reassoc-def t) (and (ssa-name? t) (let ([d (mfn-def m t)]) (and d (reassociable-op? d code l) d))))
    (define binlhs (gassign-rhs1 g)) (define binrhs (gassign-rhs2 g))
    (define lhs-r (reassoc-def binlhs)) (define rhs-r (reassoc-def binrhs))
    (cond
      [(and (not lhs-r) (not rhs-r))
       (append ops (list (new-oe binrhs) (new-oe binlhs)))]
      [(not lhs-r)
       ;; the RHS is reassociable: swap so that the LHS is
       (swap-operands! id)
       (linearize-rest! id ops binrhs binlhs)]
      [rhs-r
       (linearize-expr! id)
       (define g2 (mfn-stmt m id))
       (linearize-rest! id ops (gassign-rhs1 g2) (gassign-rhs2 g2))]
      [else (linearize-rest! id ops binlhs binrhs)]))
  (define (linearize-rest! id ops binlhs binrhs)
    (define ops1 (linearize! (mfn-def m binlhs) ops))
    (append ops1 (list (new-oe binrhs))))

  ;; linearize_expr: (a op b) op (c op d) -> ((a op b) op d) op c
  (define (linearize-expr! id)
    (define g (mfn-stmt m id))
    (define code (gassign-code g))
    (define lid (mfn-def m (gassign-rhs1 g))) (define rid (mfn-def m (gassign-rhs2 g)))
    (define lg (mfn-stmt m lid)) (define rg (mfn-stmt m rid))
    (define-values (nn st1) (make-ssa-name st (tree-type (gassign-lhs g))))
    (set! st st1)
    (define newb (gassign (gassign-loc rg) (hasheq) nn code (gassign-lhs lg) (gassign-rhs2 rg) #f))
    (define nid (mfn-insert-before! m id newb))
    (hash-set! uid nid (hash-ref uid id))
    (mfn-replace! m id (gassign (gassign-loc g) (gassign-flags g) (gassign-lhs g) code nn (gassign-rhs1 rg) #f))
    (release-stmt! rid)
    (hash-set! visited id #t) (hash-set! visited lid #t) (hash-set! visited nid #t)
    (define g2 (mfn-stmt m id))
    (define nb (and (ssa-name? (gassign-rhs2 g2)) (mfn-def m (gassign-rhs2 g2))))
    (when (and nb (reassociable-op? nb code (loop-containing (mfn-bb m id))))
      (linearize-expr! id)))

  (define (release-stmt! id)
    (define g (mfn-stmt m id))
    (for ([d (in-list (stmt-def-names g))])
      (set! st (ssa-queue-release st (ssa-name-version d))))
    (when (stmt-vdef g) (mfn-unlink-vdef! m id))
    (mfn-remove! m id))

  ;; optimize_ops_list (the constant rules)
  (define (optimize-ops code ops)
    (cond
      [(<= (length ops) 1) ops]
      [else
       (define last1 (last ops)) (define last2 (list-ref ops (- (length ops) 2)))
       (define merged
         (and (= 0 (oe-rank last1)) (is-gimple-min-invariant st (oe-op last1))
              (= 0 (oe-rank last2)) (is-gimple-min-invariant st (oe-op last2))
              (icst? (oe-op last1)) (icst? (oe-op last2))
              (fold-const-binop st code (tree-type (oe-op last2)) (oe-op last2) (oe-op last1))))
       (cond
         [(and merged (is-gimple-min-invariant st merged))
          (optimize-ops code (append (drop-right ops 2) (list (new-oe merged))))]
         [else (eliminate-using-constants code ops)])]))
  (define (eliminate-using-constants code ops)
    (define o (last ops))
    (define t (oe-op o))
    (cond
      [(not (and (= 0 (oe-rank o)) (icst? t))) ops]
      [(= 1 (length ops)) ops]
      [else
       (define all-ones (= (integer-cst-value t) (if (type-unsigned?* st (tree-type t)) (sub1 (expt 2 (type-precision* st (tree-type t)))) -1)))
       (case code
         [(bit_and_expr) (cond [(integer-zerop t) (list o)] [all-ones (drop-right ops 1)] [else ops])]
         [(bit_ior_expr) (cond [all-ones (list o)] [(integer-zerop t) (drop-right ops 1)] [else ops])]
         [(mult_expr) (cond [(integer-zerop t) (list o)] [(integer-onep t) (drop-right ops 1)] [else ops])]
         [(bit_xor_expr plus_expr minus_expr) (if (integer-zerop t) (drop-right ops 1) ops)]
         [else ops])]))

  ;; swap_ops_for_binary_stmt
  (define (swap-ops ops i)
    (define v (list->vector ops))
    (define o1 (vector-ref v i)) (define o2 (vector-ref v (+ i 1))) (define o3 (vector-ref v (+ i 2)))
    (cond
      [(and (= (oe-rank o1) (oe-rank o2)) (not (= (oe-rank o2) (oe-rank o3))))
       (vector-set! v i o3) (vector-set! v (+ i 2) o1)]
      [(and (= (oe-rank o1) (oe-rank o3)) (not (= (oe-rank o2) (oe-rank o3))))
       (vector-set! v i o2) (vector-set! v (+ i 1) o1)])
    (vector->list v))

  (define (same-op? a b) (and a b (or (and (ssa-name? a) (ssa-name? b) (ssa-name=? a b)) (eq? a b) (equal? a b))))

  ;; find_insert_point -> (values id insert-before?)
  (define (find-insert-point id rhs1 rhs2)
    (define-values (p before)
      (if (and (ssa-name? rhs1) (stmt-dominates? id (mfn-def m rhs1))) (values (mfn-def m rhs1) #f) (values id #t)))
    (if (and (ssa-name? rhs2) (stmt-dominates? p (mfn-def m rhs2))) (values (mfn-def m rhs2) #f) (values p before)))
  ;; insert_stmt_after (the insertion point is an assignment or PHI)
  (define (insert-new! at before? g u)
    (define nid
      (cond
        [before? (mfn-insert-before! m at g)]
        [(mfn-phi? m at)
         (define bb (mfn-bb m at))
         (define first (for/first ([x (in-list (mfn-stmts m bb))] #:unless (glabel? (mfn-stmt m x))) x))
         (if first (mfn-insert-before! m first g) (error 'reassoc "insertion into an empty block"))]
        [else (mfn-insert-after! m at g)]))
    (hash-set! uid nid u)
    (hash-set! visited nid #t)
    nid)

  (define (remove-visited-chain! var)
    (let loop ([var var])
      (when (and (ssa-name? var) (mfn-zero-uses? m var))
        (define id (mfn-def m var))
        (define g (and id (mfn-stmt m id)))
        (when (and g (gassign? g) (hash-ref visited id #f))
          (define next (gassign-rhs1 g))
          (release-stmt! id)
          (loop next)))))

  ;; rewrite_expr_tree -> the lhs
  (define (rewrite! id code opindex ops changed next-changed)
    (define g (mfn-stmt m id))
    (define rhs1 (gassign-rhs1 g)) (define rhs2 (gassign-rhs2 g)) (define lhs (gassign-lhs g))
    (cond
      [(= (+ opindex 2) (length ops))
       (define o1 (oe-op (list-ref ops opindex))) (define o2 (oe-op (list-ref ops (add1 opindex))))
       (cond
         [(and (same-op? rhs1 o1) (same-op? rhs2 o2)) lhs]
         [else
          (define new-lhs
            (cond
              [(or changed (and (not (and (same-op? rhs1 o2) (same-op? rhs2 o1))) (> opindex 0)))
               (define-values (at before?) (find-insert-point id o1 o2))
               (define-values (nn st1) (make-ssa-name st (tree-type lhs)))
               (set! st st1)
               (insert-new! at before? (gassign (gassign-loc g) (hasheq) nn code o1 o2 #f) (hash-ref uid id))
               nn]
              [else
               (mfn-replace! m id (gassign (gassign-loc g) (gassign-flags g) lhs code o1 o2 #f))
               lhs]))
          (when (and (not (same-op? rhs1 o1)) (not (same-op? rhs1 o2))) (remove-visited-chain! rhs1))
          new-lhs])]
      [else
       (define o (oe-op (list-ref ops opindex)))
       (define new-rhs1 (rewrite! (mfn-def m rhs1) code (add1 opindex) ops
                                  (or changed (not (same-op? o rhs2)) next-changed) #f))
       (cond
         [(or (not (same-op? o rhs2)) (not (same-op? new-rhs1 rhs1)))
          (cond
            [changed
             (define-values (at before?) (find-insert-point id new-rhs1 o))
             (define-values (nn st1) (make-ssa-name st (tree-type lhs)))
             (set! st st1)
             (insert-new! at before? (gassign (gassign-loc g) (hasheq) nn code new-rhs1 o #f) (hash-ref uid id))
             nn]
            [else
             (mfn-replace! m id (gassign (gassign-loc g) (gassign-flags g) lhs code new-rhs1 o #f))
             lhs])]
         [else lhs])]))

  (define (associative? code) (memq code '(plus_expr mult_expr bit_and_expr bit_ior_expr bit_xor_expr min_expr max_expr)))

  ;; reassociate_bb
  (define (reassociate-bb! bb)
    (let loop ([id (mfn-last m bb)])
      (when id
        (define prev0 (mfn-prev m id))
        (define g (mfn-stmt m id))
        (when (gassign? g)
          (define code (gassign-code g))
          (define lhs (gassign-lhs g))
          (cond
            [(hash-ref visited id #f)
             (when (and (ssa-name? lhs) (mfn-zero-uses? m lhs)) (release-stmt! id))]
            [(not (eq? (gimple-rhs-class st code) 'binary)) (void)]
            [(and (not (memq code '(bit_ior_expr bit_and_expr bit_xor_expr min_expr max_expr)))
                  (not (reassociable-type? (tree-type lhs))))
             (void)]
            [(not (associative? code)) (void)]
            [(and (ssa-name? lhs) (mfn-zero-uses? m lhs)) (void)]
            [else
             (hash-set! visited id #t)
             (define ops0 (linearize! id '()))
             (define orig-len (length ops0))
             (define ops1 (optimize-ops code (sort ops0 oe<?)))
             (cond
               [(= 1 (length ops1))
                (define op (oe-op (car ops1)))
                (mfn-replace! m id (gassign (gassign-loc g) (gassign-flags g) lhs (tree-code op) op #f #f))]
               [else
                (define ops2
                  (if (and (> (length ops1) 2) (memq code '(bit_and_expr bit_ior_expr bit_xor_expr)) (icst? (oe-op (last ops1))))
                      (let ([v (list->vector ops1)])
                        (define n (sub1 (vector-length v)))
                        (define t (vector-ref v 0)) (vector-set! v 0 (vector-ref v n)) (vector-set! v n t)
                        (vector->list v))
                      ops1))
                (define ops3 (if (>= (length ops2) 3) (swap-ops ops2 (- (length ops2) 3)) ops2))
                (rewrite! id code 0 ops3 #f (not (= (length ops3) orig-len)))])]))
        ;; gsi_prev from the current position (the removed statement's predecessor)
        (loop (if (mfn-live? m id) (mfn-prev m id) prev0)))))

  ;; the post-dominator walk (LIFO worklist of post-dominator sons)
  (define (pdom-sons n) (let ([i (dom-index pdom n)]) (if i (for/list ([k (in-pvector (dom-children pdom i))]) (dom-node pdom k)) '())))
  (let loop ([work (pdom-sons EXIT)])
    (unless (null? work)
      (define bb (last work))
      (define rest (drop-right work 1))
      (unless (or (= bb ENTRY) (= bb EXIT)) (reassociate-bb! bb))
      (loop (append rest (pdom-sons bb)))))
  (values (mfn->gcfg m) st))


;; every SSA name of the function (definitions and uses)
(define (ssa-names-of cfg)
  (define h (make-hasheqv))
  (for* ([bb (in-pvector (cfg-bbs cfg))]
         [g (in-sequences (in-pvector (GBlock-phis (cfg-block cfg bb))) (in-pvector (GBlock-stmts (cfg-block cfg bb))))])
    (for ([n (in-list (append (stmt-def-names g) (stmt-use-names g)))]) (hash-set! h (ssa-name-version n) n)))
  (hash-values h))
