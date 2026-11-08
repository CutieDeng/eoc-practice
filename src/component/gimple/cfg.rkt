#lang racket/base

;; ============================================================
;; cfg: pass_build_cfg (gcc/tree-cfg.cc) and the -O0 subset of
;; cleanup_tree_cfg (gcc/tree-cfgcleanup.cc), pure
;; ============================================================
;;
;; Port of GCC trunk (17.0.0) over the persistent GCfg of
;; kernel/ir/gimple/types.rkt:
;;   blocks : intmap index -> GBlock (index stmts prev next preds)
;;   edges  : intmap src -> pvector of GEdge (dest flags goto-locus)
;; An edge is identified by (src, dest); make_edge keeps that unique.
;; Every GCC function becomes `cfg -> cfg` (plus the state when a label
;; may be created or a LABEL_DECL_UID assigned).
;;
;; execute_build_cfg:
;;   make_blocks, cleanup_dead_labels, group_case_labels, make_edges,
;;   assign_discriminators, cleanup_dead_labels, cleanup_tree_cfg
;;   (cleanup_control_flow_pre; forwarder removal / merging with the
;;   -O0 location checks; end_recording_case_labels; compact_blocks)
;;
;; Orderings the dump exposes are reproduced exactly: the block chain
;; (prev/next), succ vectors (push on creation, unordered_remove on
;; removal), pred vectors likewise, LABEL_DECL_UID in statement order.
;;
;; Not ported: abnormal/EH edges, computed goto, setjmp,
;; generate_range_test in convert_single_case_switch, dominator-based
;; parts (there are no PHIs at this point).
;; ============================================================

(require racket/list
         "../../kernel/data/data.rkt"
         "../../kernel/ir/gimple/types.rkt"
         "state.rkt"
         "tree.rkt"
         "gimple.rkt"
         "../../driver/dominance/dom.rkt"
         (only-in "gimplify.rkt" tree-swap-operands-p))

(provide build-cfg cleanup-tree-cfg find-taken-edge stmt-ends-bb? cfg-next-stamp cfg-clear-messages cfg-add-message
         cfg-set-profile prof-count prof-prob split-edge label-to-block make-edge remove-edge create-bb
         EDGE-FALLTHRU EDGE-TRUE-VALUE EDGE-FALSE-VALUE
         cfg-bbs cfg-block cfg-succs cfg-preds bb-last-stmt find-fallthru-edge extract-true-false-edges
         set-block block-set-phis block-set-stmts ENTRY EXIT
         split-block set-succs set-preds prof-move-edge assign-label-uid register-label link-block unlink-block
         bb-next bb-prev
         redirect-edge-succ find-edge edge-flag? single-succ-p single-pred-p single-succ last-basic-block block-set-preds
         split-edge/st split-edges-for-insertion
         cfg-dom (struct-out Loop) find-loops loop-inside? loop-of replace-uses-by bb-set-last-stmt delete-basic-block replace-uses-folder
         cleanup-dead-labels redirect-edge-and-branch bb-remove-last-stmt)

;; ------------------------------------------------------------
;; Edge flags (subset of EDGE_*; values are our own)
;; ------------------------------------------------------------
(define EDGE-FALLTHRU     1)
(define EDGE-ABNORMAL     2)
(define EDGE-EH           4)
(define EDGE-TRUE-VALUE   8)
(define EDGE-FALSE-VALUE 16)
(define EDGE-COMPLEX (bitwise-ior EDGE-ABNORMAL EDGE-EH))
(define (flag? f bit) (not (zero? (bitwise-and f bit))))

(define ENTRY 0)
(define EXIT 1)

;; ------------------------------------------------------------
;; GCfg field updates
;; ------------------------------------------------------------

(define (cfg-set-blocks c v) (GCfg v (GCfg-edges c) (GCfg-label->bb c) (GCfg-next-label-uid c) (GCfg-messages c) (GCfg-altered c) (GCfg-touched c) (GCfg-profile c) (GCfg-last-bb c)))
(define (cfg-set-edges c v) (GCfg (GCfg-blocks c) v (GCfg-label->bb c) (GCfg-next-label-uid c) (GCfg-messages c) (GCfg-altered c) (GCfg-touched c) (GCfg-profile c) (GCfg-last-bb c)))
(define (cfg-set-label->bb c v) (GCfg (GCfg-blocks c) (GCfg-edges c) v (GCfg-next-label-uid c) (GCfg-messages c) (GCfg-altered c) (GCfg-touched c) (GCfg-profile c) (GCfg-last-bb c)))
(define (cfg-set-next-label-uid c v) (GCfg (GCfg-blocks c) (GCfg-edges c) (GCfg-label->bb c) v (GCfg-messages c) (GCfg-altered c) (GCfg-touched c) (GCfg-profile c) (GCfg-last-bb c)))
(define (cfg-set-altered c v) (GCfg (GCfg-blocks c) (GCfg-edges c) (GCfg-label->bb c) (GCfg-next-label-uid c) (GCfg-messages c) v (GCfg-touched c) (GCfg-profile c) (GCfg-last-bb c)))
(define (cfg-set-touched c v) (GCfg (GCfg-blocks c) (GCfg-edges c) (GCfg-label->bb c) (GCfg-next-label-uid c) (GCfg-messages c) (GCfg-altered c) v (GCfg-profile c) (GCfg-last-bb c)))
(define (dump c fmt . args)
  (GCfg (GCfg-blocks c) (GCfg-edges c) (GCfg-label->bb c) (GCfg-next-label-uid c)
        (pvector-cons-right (GCfg-messages c) (apply format fmt args)) (GCfg-altered c) (GCfg-touched c) (GCfg-profile c) (GCfg-last-bb c)))

(define (altered c i) (cfg-set-altered c (intbits-set (GCfg-altered c) i)))
(define (cfg-add-message c m) (dump c "~a" m))
(define (cfg-set-profile c pf) (GCfg (GCfg-blocks c) (GCfg-edges c) (GCfg-label->bb c) (GCfg-next-label-uid c) (GCfg-messages c) (GCfg-altered c) (GCfg-touched c) pf (GCfg-last-bb c)))
(define (prof-count c bb) (let ([pf (GCfg-profile c)]) (and pf (intmap-ref (GProf-counts pf) bb #f))))
(define (prof-prob c src dest) (let ([pf (GCfg-profile c)]) (and pf (hash-ref (GProf-probs pf) (cons src dest) #f))))
;; edge probabilities follow their edge
(define (prof-remove-edge c src dest)
  (define pf (GCfg-profile c))
  (if pf (cfg-set-profile c (GProf (GProf-counts pf) (hash-remove (GProf-probs pf) (cons src dest)))) c))
(define (prof-move-edge c src old new)
  (define pf (GCfg-profile c))
  (cond
    [(and pf (hash-ref (GProf-probs pf) (cons src old) #f))
     => (lambda (p) (cfg-set-profile c (GProf (GProf-counts pf) (hash-set (hash-remove (GProf-probs pf) (cons src old)) (cons src new) p))))]
    [else c]))
;; the edge SRC -> DEST now leaves NEW (split_block moves the successors)
(define (prof-move-src c src new dest)
  (define pf (GCfg-profile c))
  (cond
    [(and pf (hash-ref (GProf-probs pf) (cons src dest) #f))
     => (lambda (p) (cfg-set-profile c (GProf (GProf-counts pf) (hash-set (hash-remove (GProf-probs pf) (cons src dest)) (cons new dest) p))))]
    [else c]))
;; a pass starts with an empty dump_file: the messages belong to one pass
(define (cfg-clear-messages c)
  (GCfg (GCfg-blocks c) (GCfg-edges c) (GCfg-label->bb c) (GCfg-next-label-uid c) (pvector-empty) (GCfg-altered c) (GCfg-touched c) (GCfg-profile c) (GCfg-last-bb c)))

;; ------------------------------------------------------------
;; Blocks and the chain
;; ------------------------------------------------------------

(define (cfg-block c i) (intmap-ref (GCfg-blocks c) i #f))
(define (set-block c b) (cfg-set-blocks c (intmap-set (GCfg-blocks c) (GBlock-index b) b)))
(define (block-set-stmts b s) (GBlock (GBlock-index b) (GBlock-phis b) s (GBlock-prev b) (GBlock-next b) (GBlock-preds b)))
(define (block-set-prev b p) (GBlock (GBlock-index b) (GBlock-phis b) (GBlock-stmts b) p (GBlock-next b) (GBlock-preds b)))
(define (block-set-next b n) (GBlock (GBlock-index b) (GBlock-phis b) (GBlock-stmts b) (GBlock-prev b) n (GBlock-preds b)))
(define (block-set-phis b p) (GBlock (GBlock-index b) p (GBlock-stmts b) (GBlock-prev b) (GBlock-next b) (GBlock-preds b)))
(define (block-set-preds b p) (GBlock (GBlock-index b) (GBlock-phis b) (GBlock-stmts b) (GBlock-prev b) (GBlock-next b) p))
(define (set-stmts c i s) (set-block c (block-set-stmts (cfg-block c i) s)))
(define (stmts c i) (GBlock-stmts (cfg-block c i)))
(define (bb-next c i) (GBlock-next (cfg-block c i)))
(define (bb-prev c i) (GBlock-prev (cfg-block c i)))
;; last_basic_block: never decreases before compact_blocks; the watermark
;; is kept once a block is created after a deletion (#f: maximum index + 1)
(define (last-basic-block c) (max (add1 (car (intmap-max-entry (GCfg-blocks c)))) (or (GCfg-last-bb c) 0)))
(define (cfg-set-last-bb c v)
  (GCfg (GCfg-blocks c) (GCfg-edges c) (GCfg-label->bb c) (GCfg-next-label-uid c) (GCfg-messages c) (GCfg-altered c) (GCfg-touched c) (GCfg-profile c) v))

;; FOR_EACH_BB_FN: the chain between ENTRY and EXIT, as indices
(define (cfg-bbs c)
  (let loop ([i (bb-next c ENTRY)] [acc (pvector-empty)])
    (if (= i EXIT) acc (loop (bb-next c i) (pvector-cons-right acc i)))))

;; link_block / unlink_block
(define (link-block c i after)
  (define nxt (bb-next c after))
  (let* ([c (set-block c (block-set-next (block-set-prev (cfg-block c i) after) nxt))]
         [c (set-block c (block-set-next (cfg-block c after) i))]
         [c (set-block c (block-set-prev (cfg-block c nxt) i))])
    c))
(define (unlink-block c i)
  (define b (cfg-block c i))
  (let* ([c (set-block c (block-set-prev (cfg-block c (GBlock-next b)) (GBlock-prev b)))]
         [c (set-block c (block-set-next (cfg-block c (GBlock-prev b)) (GBlock-next b)))]
         [c (set-block c (block-set-prev (block-set-next (cfg-block c i) #f) #f))])
    c))

;; create_bb: a new index, linked after AFTER: (values index cfg)
(define (create-bb c stmts after)
  (define i (last-basic-block c))
  (define c1 (cfg-set-last-bb (cfg-set-blocks c (intmap-set (GCfg-blocks c) i (GBlock i (pvector-empty) stmts #f #f (pvector-empty)))) (add1 i)))
  (values i (link-block c1 i after)))

;; expunge_block
(define (expunge-block c i)
  (define c1 (unlink-block c i))
  (cfg-set-edges (cfg-set-blocks c1 (intmap-remove (GCfg-blocks c1) i)) (intmap-remove (GCfg-edges c1) i)))

;; ------------------------------------------------------------
;; Statements in blocks
;; ------------------------------------------------------------

;; gimple_set_bb for a label: LABEL_DECL_UID on first assignment (state),
;; label_to_block_map entry (cfg)
(define (assign-label-uid st c l)
  (cond
    [(decl-flag st l 'label-uid #f) (values st c)]
    [else (values (set-decl-flag st l 'label-uid (GCfg-next-label-uid c))
                  (cfg-set-next-label-uid c (add1 (GCfg-next-label-uid c))))]))
(define (register-label c l i) (cfg-set-label->bb c (intmap-set (GCfg-label->bb c) (decl-uid l) i)))
(define (note-stmt-bb st c g i)
  (cond
    [(glabel? g)
     (define-values (st1 c1) (assign-label-uid st c (glabel-label g)))
     (values st1 (register-label c1 (glabel-label g) i))]
    [else (values st c)]))

(define (bb-append-stmt st c i g)
  (define-values (st1 c1) (note-stmt-bb st c g i))
  (values st1 (set-stmts c1 i (pvector-cons-right (stmts c1 i) g))))
;; insert at position POS: (values st cfg)
(define (bb-insert-stmt-at st c i pos g)
  (define-values (st1 c1) (note-stmt-bb st c g i))
  (define s (stmts c1 i))
  (values st1 (set-stmts c1 i (pvector-append (pvector-cons-right (pvector-take s pos) g) (pvector-drop s pos)))))
(define (bb-remove-stmt-at c i pos)
  (define s (stmts c i))
  (set-stmts c i (pvector-append (pvector-take s pos) (pvector-drop s (add1 pos)))))
(define (bb-remove-last-stmt c i) (set-stmts c i (pvector-drop-right (stmts c i) 1)))
(define (bb-set-last-stmt c i g) (set-stmts c i (seq-replace-last (stmts c i) g)))

(define (bb-last-stmt c i) (seq-last (stmts c i)))
(define (bb-first-stmt c i) (seq-first (stmts c i)))
(define (bb-first-non-label-stmt c i) (for/first ([g (in-pvector (stmts c i))] #:unless (glabel? g)) g))
(define (bb-leading-label-count c i)
  (define s (stmts c i))
  (let loop ([k 0]) (if (and (< k (pvector-length s)) (glabel? (pvector-ref s k))) (loop (add1 k)) k)))

(define (label-to-block c l) (intmap-ref (GCfg-label->bb c) (decl-uid l) #f))

;; ------------------------------------------------------------
;; Edges
;; ------------------------------------------------------------

(define (cfg-succs c i) (intmap-ref (GCfg-edges c) i (pvector-empty)))
(define (set-succs c i v) (cfg-set-edges c (intmap-set (GCfg-edges c) i v)))
(define (cfg-preds c i) (GBlock-preds (cfg-block c i)))
(define (set-preds c i v) (set-block c (block-set-preds (cfg-block c i) v)))

(define (pvector-unordered-remove v k)
  (define n (pvector-length v))
  (if (= k (sub1 n)) (pvector-drop-right v 1) (pvector-drop-right (pvector-set v k (pvector-last v)) 1)))

(define (edge-index c src dest)
  (for/first ([e (in-pvector (cfg-succs c src))] [k (in-naturals)] #:when (= (GEdge-dest e) dest)) k))
(define (find-edge c src dest)
  (define k (edge-index c src dest))
  (and k (pvector-ref (cfg-succs c src) k)))
(define (set-edge c src e)
  (set-succs c src (pvector-set (cfg-succs c src) (edge-index c src (GEdge-dest e)) e)))
(define (edge-set-flags e f) (GEdge (GEdge-dest e) f (GEdge-goto-locus e)))
(define (edge-set-locus e l) (GEdge (GEdge-dest e) (GEdge-flags e) l))
(define (set-edge-locus c src dest l) (set-edge c src (edge-set-locus (find-edge c src dest) l)))
(define (set-edge-flags c src dest f) (set-edge c src (edge-set-flags (find-edge c src dest) f)))
(define (or-edge-flags c src dest f)
  (define e (find-edge c src dest))
  (set-edge c src (edge-set-flags e (bitwise-ior (GEdge-flags e) f))))

;; make_edge: (values new? cfg); an existing edge only gets the flags ORed
(define (make-edge c src dest flags)
  (cond
    [(find-edge c src dest) (values #f (or-edge-flags c src dest flags))]
    [else
     (let* ([c (set-succs c src (pvector-cons-right (cfg-succs c src) (GEdge dest flags #f)))]
            [c (set-preds c dest (pvector-cons-right (cfg-preds c dest) src))])
       (values #t c))]))

;; the next use-list stamp of a function (see gimple.rkt restamp)
(define (cfg-next-stamp c)
  (add1 (for*/fold ([mx 0]) ([i (in-pvector (cfg-bbs c))]
                             [g (in-sequences (in-pvector (GBlock-phis (cfg-block c i))) (in-pvector (GBlock-stmts (cfg-block c i))))])
          (max mx (stmt-max-stamp g)))))
;; restamp a block's PHIs against their previous versions, (values phis next)
(define (restamp-phis old-phis new-phis n0)
  (for/fold ([acc (pvector-empty)] [n n0] #:result (values acc n)) ([old (in-pvector old-phis)] [new (in-pvector new-phis)])
    (define-values (g n1) (restamp old new n))
    (values (pvector-cons-right acc g) n1)))

;; gimple_split_block: the statements from index K on move to a new
;; block placed right after BB, which takes over BB's successor edges
;; (their PHI arguments keep their slots), with a fallthru edge BB -> NEW.
;; The new block starts with BB's count.
(define (split-block c bb k)
  (define b (cfg-block c bb))
  (define all (GBlock-stmts b))
  (define head (for/pvector ([g (in-pvector all)] [i (in-naturals)] #:when (< i k)) g))
  (define tail (for/pvector ([g (in-pvector all)] [i (in-naturals)] #:when (>= i k)) g))
  (define-values (nb c1) (create-bb (set-block c (block-set-stmts b head)) tail bb))
  (define succs (cfg-succs c1 bb))
  ;; move the successor edges
  (define c2
    (for/fold ([c (set-succs (set-succs c1 nb succs) bb (pvector-empty))]) ([e (in-pvector succs)])
      (define d (GEdge-dest e))
      (define db (cfg-block c d))
      (define pk (pvector-index-of (GBlock-preds db) bb))
      (let* ([c (prof-move-src c bb nb d)]
             [c (set-block c (block-set-preds (block-set-phis db (for/pvector ([ph (in-pvector (GBlock-phis db))])
                                                                   (gphi (gphi-loc ph) (gphi-flags ph) (gphi-var ph) (gphi-result ph)
                                                                         (for/pvector ([a (in-pvector (gphi-args ph))])
                                                                           (if (= (car a) bb) (cons nb (cdr a)) a)))))
                                               (pvector-set (GBlock-preds db) pk nb)))])
        c)))
  (define-values (_n c3) (make-edge c2 bb nb EDGE-FALLTHRU))
  (define pf (GCfg-profile c3))
  (define c4
    (if pf
        (cfg-set-profile c3 (GProf (intmap-set (GProf-counts pf) nb (or (intmap-ref (GProf-counts pf) bb #f) 0))
                                   (hash-set (GProf-probs pf) (cons bb nb) 134217728)))
        c3))
  (values nb c4))

;; remove_edge (disconnect_src + disconnect_dest, both unordered_remove);
;; the PHI arguments of DEST for the edge go with it (remove_phi_args,
;; same unordered removal so arguments stay aligned with the preds)
(define (remove-edge c0 src dest)
  (define c (prof-remove-edge c0 src dest))
  (define k (pvector-index-of (cfg-preds c dest) src))
  (define b (cfg-block c dest))
  (define-values (phis* _n)
    (restamp-phis (GBlock-phis b)
                  (for/pvector ([ph (in-pvector (GBlock-phis b))])
                    (gphi (gphi-loc ph) (gphi-flags ph) (gphi-var ph) (gphi-result ph)
                          (pvector-unordered-remove (gphi-args ph) k)))
                  (cfg-next-stamp c)))
  (let* ([c (set-succs c src (pvector-unordered-remove (cfg-succs c src) (edge-index c src dest)))]
         [c (set-block c (block-set-preds (block-set-phis b phis*)
                                          (pvector-unordered-remove (GBlock-preds b) k)))])
    c))

;; redirect_edge_succ: the edge keeps its slot in SRC's succs
;; (disconnect_dest + connect_dest)
(define (redirect-edge-succ c0 src old new)
  (define c (prof-move-edge c0 src old new))
  (define k (edge-index c src old))
  (define e (pvector-ref (cfg-succs c src) k))
  (define pk (pvector-index-of (cfg-preds c old) src))
  (define ob (cfg-block c old))
  (define-values (phis* _n)
    (restamp-phis (GBlock-phis ob)
                  (for/pvector ([ph (in-pvector (GBlock-phis ob))])
                    (gphi (gphi-loc ph) (gphi-flags ph) (gphi-var ph) (gphi-result ph)
                          (pvector-unordered-remove (gphi-args ph) pk)))
                  (cfg-next-stamp c)))
  (let* ([c (set-block c (block-set-preds (block-set-phis ob phis*)
                                           (pvector-unordered-remove (GBlock-preds ob) pk)))]
         [c (set-succs c src (pvector-set (cfg-succs c src) k (GEdge new (GEdge-flags e) (GEdge-goto-locus e))))]
         [c (set-preds c new (pvector-cons-right (cfg-preds c new) src))])
    c))

;; redirect_edge_succ_nodup
(define (redirect-edge-succ-nodup c src old new)
  (define s (find-edge c src new))
  (cond
    [(and s (not (= old new)))
     (define e (find-edge c src old))
     (remove-edge (or-edge-flags c src new (GEdge-flags e)) src old)]
    [else (redirect-edge-succ c src old new)]))

(define (single-succ-p c i) (= 1 (pvector-length (cfg-succs c i))))
(define (single-pred-p c i) (= 1 (pvector-length (cfg-preds c i))))
(define (single-succ-edge c i) (pvector-first (cfg-succs c i)))
(define (single-succ c i) (GEdge-dest (single-succ-edge c i)))
(define (single-pred c i) (pvector-first (cfg-preds c i)))
(define (edge-flag? e f) (flag? (GEdge-flags e) f))

(define (find-fallthru-edge c i)
  (for/first ([e (in-pvector (cfg-succs c i))] #:when (edge-flag? e EDGE-FALLTHRU)) e))

;; extract_true_false_edges_from_block
(define (extract-true-false-edges c i)
  (define e0 (pvector-ref (cfg-succs c i) 0))
  (define e1 (pvector-ref (cfg-succs c i) 1))
  (if (edge-flag? e0 EDGE-TRUE-VALUE) (values e0 e1) (values e1 e0)))

;; ------------------------------------------------------------
;; Statement predicates (tree-cfg.cc)
;; ------------------------------------------------------------

(define (label-nonlocal? st l) (decl-flag st l 'nonlocal #f))
(define (label-forced? st l) (decl-flag st l 'forced #f))
(define (ctrl-altering? g) (and (gcall? g) (stmt-flag g 'ctrl-altering #f)))
;; is_ctrl_stmt / stmt_ends_bb_p
(define (ctrl-stmt? g) (or (gcond? g) (gswitch? g) (ggoto? g) (greturn? g)))
(define (stmt-ends-bb? g) (or (ctrl-stmt? g) (ctrl-altering? g)))

;; stmt_starts_bb_p
(define (stmt-starts-bb? st g prev)
  (cond
    [(glabel? g)
     (define l (glabel-label g))
     (cond
       [(or (label-nonlocal? st l) (label-forced? st l)) #t]
       [(glabel? prev)
        (define p (glabel-label prev))
        (or (label-nonlocal? st p) (not (decl-flag st p 'artificial)))]
       [else #t])]
    [(gcall? g) (flag? (gcall-ecf g) ECF-RETURNS-TWICE)]
    [else #f]))

;; ------------------------------------------------------------
;; make_blocks
;; ------------------------------------------------------------

(define (make-blocks st c body)
  (for/fold ([c c] [st st] [b ENTRY] [start-new #t] [prev #f] #:result (values c st))
            ([g0 (in-pvector body)])
    ;; gimple_call_initialize_ctrl_altering (C subset: noreturn only)
    (define g (if (and (gcall? g0) (gimple-call-noreturn-p g0)) (stmt-set-flag g0 'ctrl-altering #t) g0))
    (define new? (or start-new (stmt-starts-bb? st g prev)))
    (define-values (b1 c1) (if new? (create-bb c (pvector-empty) b) (values b c)))
    (define-values (st2 c2) (bb-append-stmt st c1 b1 g))
    (values c2 st2 b1 (stmt-ends-bb? g) g)))

;; ------------------------------------------------------------
;; cleanup_dead_labels
;; ------------------------------------------------------------

(define (cleanup-dead-labels st c)
  ;; main label per block: the first label, or the first user label after an artificial one
  (define main
    (for/fold ([main intmap-empty]) ([i (in-pvector (cfg-bbs c))])
      (let loop ([k 0] [main main])
        (define s (stmts c i))
        (cond
          [(not (and (< k (pvector-length s)) (glabel? (pvector-ref s k)))) main]
          [else
           (define l (glabel-label (pvector-ref s k)))
           (cond
             [(not (intmap-ref main i #f)) (loop (add1 k) (intmap-set main i l))]
             [(and (not (decl-flag st l 'artificial)) (decl-flag st (intmap-ref main i) 'artificial)) (intmap-set main i l)]
             [else (loop (add1 k) main)])]))))
  ;; main_block_label: (values label main used)
  (define (main-block-label l main used)
    (define i (label-to-block c l))
    (define main1 (if (intmap-ref main i #f) main (intmap-set main i l)))
    (values (intmap-ref main1 i) main1 (intbits-set used i)))
  ;; redirect every reference to the main label of its block
  (define-values (c1 main1 used1)
    (for/fold ([c c] [main main] [used intbits-empty] #:result (values c main used)) ([i (in-pvector (cfg-bbs c))])
      (define g (bb-last-stmt c i))
      (cond
        [(gcond? g)
         (define-values (t main1 used1) (if (gcond-true-label g) (main-block-label (gcond-true-label g) main used) (values #f main used)))
         (define-values (f main2 used2) (if (gcond-false-label g) (main-block-label (gcond-false-label g) main1 used1) (values #f main1 used1)))
         (values (bb-set-last-stmt c i (gcond (gcond-loc g) (gcond-flags g) (gcond-code g) (gcond-lhs g) (gcond-rhs g) t f)) main2 used2)]
        [(gswitch? g)
         (define-values (dc main1 used1)
           (let-values ([(l m u) (main-block-label (case-label (gswitch-default-case g)) main used)])
             (values (tree-set-operand (gswitch-default-case g) 2 l) m u)))
         (define-values (cases main2 used2)
           (for/fold ([acc (pvector-empty)] [main main1] [used used1] #:result (values acc main used)) ([cs (in-pvector (gswitch-cases g))])
             (define-values (l m u) (main-block-label (case-label cs) main used))
             (values (pvector-cons-right acc (tree-set-operand cs 2 l)) m u)))
         (values (bb-set-last-stmt c i (gswitch (gswitch-loc g) (gswitch-flags g) (gswitch-index g) dc cases)) main2 used2)]
        [(ggoto? g)
         (define-values (l main1 used1) (main-block-label (ggoto-dest g) main used))
         (values (bb-set-last-stmt c i (ggoto (ggoto-loc g) (ggoto-flags g) l)) main1 used1)]
        [else (values c main used)])))
  ;; purge the dead leading labels
  (for/fold ([c c1]) ([i (in-pvector (cfg-bbs c1))])
    (define keep (and (intbits-ref used1 i) (intmap-ref main1 i #f)))
    (cond
      [(not (intmap-ref main1 i #f)) c]
      [else
       (define n (bb-leading-label-count c i))
       (define s (stmts c i))
       (define kept
         (for/pvector ([g (in-pvector (pvector-take s n))]
                       #:when (let ([l (glabel-label g)])
                                (or (and keep (decl-eq? l keep)) (not (decl-flag st l 'artificial))
                                    (label-nonlocal? st l) (label-forced? st l))))
           g))
       (set-stmts c i (pvector-append kept (pvector-drop s n)))])))

;; ------------------------------------------------------------
;; group_case_labels
;; ------------------------------------------------------------

;; group_case_labels_stmt; our gswitch keeps the default separately, so
;; GCC's index i is (sub1 i) in `cases`.  Returns the new statement.
(define (group-case-labels-stmt c g)
  (define default-bb (label-to-block c (case-label (gswitch-default-case g))))
  (define old (gswitch-cases g))
  (define old-size (pvector-length old))
  (define new
    (let loop ([i 0] [acc (pvector-empty)])
      (cond
        [(>= i old-size) acc]
        [else
         (define base (pvector-ref old i))
         (define base-bb (label-to-block c (case-label base)))
         (cond
           [(or (not base-bb) (= base-bb default-bb)) (loop (add1 i) acc)]
           [else
            (define-values (next base*)
              (let merge ([j (add1 i)] [high (or (case-high base) (case-low base))] [base base])
                (cond
                  [(>= j old-size) (values j base)]
                  [else
                   (define m (pvector-ref old j))
                   (cond
                     [(and (eqv? (label-to-block c (case-label m)) base-bb)
                           (= (integer-cst-value (case-low m)) (add1 (integer-cst-value high))))
                      (define h (or (case-high m) (case-low m)))
                      (merge (add1 j) h (tree-set-operand base 1 h))]
                     [else (values j base)])])))
            (loop next (pvector-cons-right acc base*))])])))
  (if (< (pvector-length new) old-size)
      (gswitch (gswitch-loc g) (gswitch-flags g) (gswitch-index g) (gswitch-default-case g) new)
      g))

(define (group-case-labels c)
  (for/fold ([c c]) ([i (in-pvector (cfg-bbs c))])
    (define g (bb-last-stmt c i))
    (if (gswitch? g) (bb-set-last-stmt c i (group-case-labels-stmt c g)) c)))

;; ------------------------------------------------------------
;; make_edges
;; ------------------------------------------------------------

(define (make-edges c)
  (define-values (_n c1) (make-edge c ENTRY (bb-next c ENTRY) EDGE-FALLTHRU))
  (for/fold ([c c1]) ([i (in-pvector (cfg-bbs c1))]) (make-edges-bb c i)))

(define (stmt-location g) (and g (stmt-loc g)))

(define (make-edges-bb c i)
  (define last (bb-last-stmt c i))
  (define (with-locus c dest l) (set-edge-locus c i dest l))
  (cond
    [(not last) c]
    [(ggoto? last)                        ; make_goto_expr_edges (simple goto)
     (define dest (label-to-block c (ggoto-dest last)))
     (define-values (_n c1) (make-edge c i dest EDGE-FALLTHRU))
     (bb-remove-last-stmt (with-locus c1 dest (stmt-loc last)) i)]
    [(greturn? last)
     (define-values (_n c1) (make-edge c i EXIT 0))
     (with-locus c1 EXIT (stmt-loc last))]
    [(gcond? last) (make-cond-expr-edges c i last)]
    [(gswitch? last)
     (for/fold ([c c]) ([cs (in-pvector (pvector-cons-left (gswitch-cases last) (gswitch-default-case last)))])
       (define-values (_n c1) (make-edge c i (label-to-block c (case-label cs)) 0))
       c1)]
    [(and (gcall? last) (gimple-call-noreturn-p last)) c]
    [else
     (define-values (_n c1) (make-edge c i (bb-next c i) EDGE-FALLTHRU))
     c1]))

;; make_cond_expr_edges
(define (make-cond-expr-edges c i g)
  (define then-bb (label-to-block c (gcond-true-label g)))
  (define else-bb (label-to-block c (gcond-false-label g)))
  (define-values (_n1 c1) (make-edge c i then-bb EDGE-TRUE-VALUE))
  (define c2 (set-edge-locus c1 i then-bb (stmt-location (bb-first-stmt c1 then-bb))))
  (define-values (new2 c3) (make-edge c2 i else-bb EDGE-FALSE-VALUE))
  (define c4 (if new2 (set-edge-locus c3 i else-bb (stmt-location (bb-first-stmt c3 else-bb))) c3))
  (bb-set-last-stmt c4 i (gcond (gcond-loc g) (gcond-flags g) (gcond-code g) (gcond-lhs g) (gcond-rhs g) #f #f)))

;; ------------------------------------------------------------
;; assign_discriminators (trunk version, 385d9937f0e)
;; ------------------------------------------------------------

;; assign_discriminator: LOCATION_LINE of a location without a locus is 0.
;; MAP: immutable hash line -> (cons bb-id discrim).  (values loc map)
(define (assign-discriminator loc bb-id map)
  (define line (let ([s (gloc-spot loc)]) (if s (cadr s) 0)))
  (define e (hash-ref map line #f))
  (cond
    [(not e) (values loc (hash-set map line (cons bb-id 0)))]
    [else
     (define e* (if (= (car e) bb-id) e (cons bb-id (add1 (cdr e)))))
     (define map* (if (eq? e e*) map (hash-set map line e*)))
     (values (if (zero? (cdr e*)) loc (location-with-discriminator loc (cdr e*))) map*)]))

(define (assign-discriminators c)
  (for/fold ([c c] [map (hash)] [bb-id 0] #:result c) ([i (in-pvector (cfg-bbs c))])
    ;; statements
    (define-values (s* map1 bb-id1)
      (for/fold ([acc (pvector-empty)] [map map] [bb-id bb-id] [prev-loc #f] [prev-rep #f]
                 #:result (values acc map bb-id))
                ([g (in-pvector (stmts c i))])
        (define loc (stmt-loc g))
        (cond
          [(not loc) (values (pvector-cons-right acc g) map bb-id prev-loc prev-rep)]
          [else
           (define-values (rep map1 prev-loc1)
             (if (equal? loc prev-loc)
                 (values prev-rep map loc)
                 (let-values ([(r m) (assign-discriminator loc bb-id map)]) (values r m loc))))
           (define g* (stmt-set-loc g rep))
           ;; break basic blocks after each call
           (if (gcall? g)
               (values (pvector-cons-right acc g*) map1 (add1 bb-id) #f rep)
               (values (pvector-cons-right acc g*) map1 bb-id prev-loc1 rep))])))
    (define c1 (set-stmts c i s*))
    (define bb-id2 (if (single-succ-p c1 i) bb-id1 (add1 bb-id1)))
    ;; successor edges
    (define-values (succs* map2 bb-id3)
      (for/fold ([acc (pvector-empty)] [map map1] [bb-id bb-id2] #:result (values acc map bb-id)) ([e (in-pvector (cfg-succs c1 i))])
        (define-values (e* map*)
          (if (GEdge-goto-locus e)
              (let-values ([(l m) (assign-discriminator (GEdge-goto-locus e) bb-id map)]) (values (edge-set-locus e l) m))
              (values e map)))
        (values (pvector-cons-right acc e*) map* (add1 bb-id))))
    (values (set-succs c1 i succs*) map2 (add1 bb-id3))))

;; ------------------------------------------------------------
;; cleanup_tree_cfg (tree-cfgcleanup.cc, -O0, no loops, no PHIs)
;; ------------------------------------------------------------

;; remove_bb + delete_basic_block
(define (delete-basic-block c i)
  ;; remove_bb: the statements from last to first (release_defs: the VDEF,
  ;; then the LHS), then the PHI results (remove_phi_nodes) are released
  (for* ([g (in-list (reverse (pvector->list (stmts c i))))] [d (in-list (stmt-defs-for-release g))]) (log-ssa-release! (ssa-name-version d)))
  (for ([ph (in-pvector (GBlock-phis (cfg-block c i)))]) (log-ssa-release! (ssa-name-version (gphi-result ph))))
  (let* ([c (dump c "Removing basic block ~a" i)]
         [c (set-stmts c i (pvector-empty))]
         [c (for/fold ([c c]) ([src (in-pvector (cfg-preds c i))]) (remove-edge c src i))]
         [c (for/fold ([c c]) ([e (in-pvector (cfg-succs c i))]) (remove-edge c i (GEdge-dest e)))])
    (expunge-block c i)))

;; ---- cleanup_control_flow_bb ----------------------------------------

;; gimple_simplify on a GIMPLE_COND, constant-operand subset
(define (cond-constant-value st g)
  (define l (gcond-lhs g)) (define r (gcond-rhs g))
  (and (eq? (tree-code l) 'integer_cst) (eq? (tree-code r) 'integer_cst)
       (let ([a (integer-cst-value l)] [b (integer-cst-value r)])
         (define v
           (case (gcond-code g)
             [(eq_expr) (= a b)] [(ne_expr) (not (= a b))]
             [(lt_expr) (< a b)] [(le_expr) (<= a b)] [(gt_expr) (> a b)] [(ge_expr) (>= a b)]
             [else 'unknown]))
         (and (boolean? v) (build-int-cst st (boolean-type st) (if v 1 0))))))

;; find_case_label_for_value: linear scan over the sorted cases, default otherwise
(define (find-case-label-for-value g v)
  (define n (integer-cst-value v))
  (or (for/first ([cs (in-pvector (gswitch-cases g))]
                  #:when (let ([lo (integer-cst-value (case-low cs))]
                               [hi (integer-cst-value (or (case-high cs) (case-low cs)))])
                           (<= lo n hi)))
        cs)
      (gswitch-default-case g)))

;; find_taken_edge: the GEdge or #f
(define (find-taken-edge st c i val)
  (define g (bb-last-stmt c i))
  (cond
    [(gcond? g)
     (define v (or val (cond-constant-value st g)))
     (cond
       [(not (and v (eq? (tree-code v) 'integer_cst))) #f]
       [else (define-values (t f) (extract-true-false-edges c i)) (if (integer-zerop v) f t)])]
    [(gswitch? g)
     (define taken
       (cond
         [(pvector-empty? (gswitch-cases g)) (gswitch-default-case g)]
         [else
          (define v (or val (gswitch-index g)))
          (and (eq? (tree-code v) 'integer_cst) (find-case-label-for-value g v))]))
     (and taken (find-edge c i (label-to-block c (case-label taken))))]
    [else (and (single-succ-p c i) (single-succ-edge c i))]))

;; convert_single_case_switch: (values converted? cfg)
(define (convert-single-case-switch st c i g)
  (cond
    [(not (= 1 (pvector-length (gswitch-cases g)))) (values #f c)]
    [else
     (define index (gswitch-index g))
     (define cs (pvector-first (gswitch-cases g)))
     (when (case-high cs)
       (error 'convert-single-case-switch "range case needs generate_range_test (not ported)"))
     (define default-bb (label-to-block c (case-label (gswitch-default-case g))))
     (define case-bb (label-to-block c (case-label cs)))
     (define cnd (gimple-build-cond 'eq_expr index (fold-convert st (tree-type index) (case-low cs)) #f #f (stmt-loc g)))
     (let* ([c (bb-set-last-stmt c i cnd)]
            [c (or-edge-flags c i case-bb EDGE-TRUE-VALUE)]
            [c (or-edge-flags c i default-bb EDGE-FALSE-VALUE)])
       (values #t c))]))

;; canonicalize_bool_cond: (values swapped? cfg)
(define (canonicalize-bool-cond st c i g cond->string)
  (define t (tree-type (gcond-lhs g)))
  (define code (gcond-code g))
  (cond
    [(not (memq code '(eq_expr ne_expr))) (values #f c)]
    [(not (or (boolean-type-p st t) (and (integral-type-p st t) (eqv? (type-attr st t 'precision) 1)))) (values #f c)]
    [(and (eq? code 'eq_expr) (not (integer-zerop (gcond-rhs g)))) (values #f c)]
    [(and (eq? code 'ne_expr) (not (integer-onep (gcond-rhs g)))) (values #f c)]
    [else
     (define n (gimple-build-cond 'ne_expr (gcond-lhs g) (build-zero-cst st t) #f #f (stmt-loc g)))
     (define succs* (for/pvector ([e (in-pvector (cfg-succs c i))])
                      (edge-set-flags e (bitwise-xor (GEdge-flags e) (bitwise-ior EDGE-TRUE-VALUE EDGE-FALSE-VALUE)))))
     ;; print_gimple_expr (TDF_RHS_ONLY) of a cond: "lhs op rhs"
     (values #t (dump (set-succs (bb-set-last-stmt c i n) i succs*) "  Swapped '~a'" (cond->string st n)))]))

;; cleanup_control_expr_graph: (values changed? cfg)
(define (cleanup-control-expr-graph st c i cond->string)
  (define-values (taken retval c1)
    (cond
      [(not (single-succ-p c i))
       (define-values (_cv c1) (if (gswitch? (bb-last-stmt c i)) (convert-single-case-switch st c i (bb-last-stmt c i)) (values #f c)))
       (define-values (_sw c2) (if (gcond? (bb-last-stmt c1 i)) (canonicalize-bool-cond st c1 i (bb-last-stmt c1 i) cond->string) (values #f c1)))
       (define g (bb-last-stmt c2 i))
       (define val (cond [(gcond? g) (cond-constant-value st g)] [(gswitch? g) (gswitch-index g)] [else #f]))
       (define te (find-taken-edge st c2 i val))
       (cond
         [(not te) (values #f #f c2)]
         [else
          ;; remove all the edges except the one that is always executed
          (define-values (c3 removed)
            (let loop ([c c2] [k 0] [removed #f])
              (define succs (cfg-succs c i))
              (cond
                [(>= k (pvector-length succs)) (values c removed)]
                [(= (GEdge-dest (pvector-ref succs k)) (GEdge-dest te)) (loop c (add1 k) removed)]
                [else
                 ;; taken_edge->probability += e->probability
                 (define d (GEdge-dest (pvector-ref succs k)))
                 (define pf (GCfg-profile c))
                 (define pt (prof-prob c i (GEdge-dest te))) (define pe (prof-prob c i d))
                 (define c* (if (and pf pt pe)
                                (cfg-set-profile c (GProf (GProf-counts pf) (hash-set (GProf-probs pf) (cons i (GEdge-dest te)) (min (+ pt pe) 134217728))))
                                c))
                 (loop (remove-edge c* i d) k #t)])))
          (values te removed c3)])]
      [else (values (single-succ-edge c i) #f c)]))
  (cond
    [(not taken) (values #f c1)]
    [else
     (define dest (GEdge-dest taken))
     (let* ([c (altered c1 i)]
            [c (bb-remove-last-stmt c i)]
            [e (find-edge c i dest)]
            [c (set-edge c i (edge-set-flags e (bitwise-ior EDGE-FALLTHRU
                                                            (bitwise-and (GEdge-flags e) (bitwise-not (bitwise-ior EDGE-TRUE-VALUE EDGE-FALSE-VALUE))))))])
       (values retval c))]))

;; cleanup_control_flow_bb: (values changed? cfg)
(define (cleanup-control-flow-bb st c i cond->string)
  (define g (bb-last-stmt c i))
  (cond
    [(not g) (values #f c)]
    [(or (gcond? g) (gswitch? g)) (cleanup-control-expr-graph st c i cond->string)]
    [(and (gcall? g) (gimple-call-noreturn-p g))
     (define ft (find-fallthru-edge c i))
     (define c1 (if ft (remove-edge c i (GEdge-dest ft)) c))
     (values (and ft #t) (bb-set-last-stmt c1 i (stmt-set-flag g 'ctrl-altering #t)))]
    [else (values #f c)]))

;; ---- cleanup_control_flow_pre -----------------------------------------

(define (cleanup-control-flow-pre st c cond->string)
  ;; pre-order DFS over successor edges, as an explicit stack of (succs . index)
  (define-values (c1 visited retval)
    (let loop ([c c] [stack (list (cons (cfg-succs c ENTRY) 0))] [visited intbits-empty] [retval #f])
      (cond
        [(null? stack) (values c visited retval)]
        [else
         (define top (car stack))
         (define dest (GEdge-dest (pvector-ref (car top) (cdr top))))
         (cond
           [(and (not (= dest EXIT)) (not (intbits-ref visited dest)))
            (define-values (ch c1) (cleanup-control-flow-bb st c dest cond->string))
            (define succs (cfg-succs c1 dest))
            (loop c1 (if (pvector-empty? succs) stack (cons (cons succs 0) stack))
                  (intbits-set visited dest) (or ch retval))]
           [(< (add1 (cdr top)) (pvector-length (car top)))
            (loop c (cons (cons (car top) (add1 (cdr top))) (cdr stack)) visited retval)]
           [else (loop c (cdr stack) visited retval)])])))
  ;; remove all unreachable blocks, by index
  (for/fold ([c c1] [retval retval] #:result (values retval c)) ([i (in-range 2 (last-basic-block c1))])
    (if (and (cfg-block c i) (not (intbits-ref visited i)))
        (values (delete-basic-block c i) #t)
        (values c retval))))

;; ---- merging ------------------------------------------------------------

;; gimple_can_merge_blocks_p (== want_merge_blocks_p here: counts are uninitialized)
(define (can-merge-blocks? st c a b)
  (and (single-succ-p c a)
       (not (edge-flag? (single-succ-edge c a) EDGE-COMPLEX))
       (= (single-succ c a) b)
       (single-pred-p c b)
       (not (= a ENTRY))
       (not (= b EXIT))
       (let ([last (bb-last-stmt c a)]) (not (and last (stmt-ends-bb? last))))
       ;; labels at the beginning of B: user labels block the merge at -O0,
       ;; forced labels always
       (for/and ([g (in-pvector (stmts c b))] #:break (not (glabel? g)))
         (or (decl-flag st (glabel-label g) 'artificial)
             (and (optimize? st) (not (label-forced? st (glabel-label g))))))
       ;; -O0: don't merge if we'd lose goto_locus
       (or (optimize? st)
           (let ([gl (GEdge-goto-locus (single-succ-edge c a))])
             (or (not gl)
                 (let ([prev (bb-last-stmt c a)] [next (bb-first-non-label-stmt c b)])
                   (or (and prev (equal? (stmt-loc prev) gl))
                       (and next (equal? (stmt-loc next) gl)))))))))

;; the argument a PHI receives from predecessor PRED
(define (phi-arg ph pred) (for/first ([a (in-pvector (gphi-args ph))] #:when (= (car a) pred)) (cdr a)))

;; replace_uses_by: every use of NAME (statements and PHI arguments) becomes VAL
(define (commutative-tree-code code)
  (and (memq code '(plus_expr mult_expr mult_highpart_expr min_expr max_expr bit_ior_expr bit_xor_expr bit_and_expr
                    ne_expr eq_expr unordered_expr ordered_expr uneq_expr ltgt_expr truth_and_expr truth_xor_expr truth_or_expr
                    widen_mult_expr)) #t))
(define (comparison-code? code)
  (and (memq code '(lt_expr le_expr gt_expr ge_expr eq_expr ne_expr unordered_expr ordered_expr unlt_expr unle_expr ungt_expr unge_expr uneq_expr ltgt_expr)) #t))
(define (swap-tree-comparison code)
  (case code
    [(lt_expr) 'gt_expr] [(gt_expr) 'lt_expr] [(le_expr) 'ge_expr] [(ge_expr) 'le_expr]
    [(unlt_expr) 'ungt_expr] [(ungt_expr) 'unlt_expr] [(unle_expr) 'unge_expr] [(unge_expr) 'unle_expr]
    [else code]))

(define (replace-uses-by st c name val)
  (define v (ssa-name-version name))
  ;; fold_stmt on each changed statement: the operand-order canonicalization
  ;; of gimple_resimplify2 / fold_stmt_1 (GIMPLE_COND)
  (define (canonicalize g)
    (cond
      [(and (gcond? g) (tree-swap-operands-p st (gcond-lhs g) (gcond-rhs g)))
       (gcond (gcond-loc g) (gcond-flags g) (swap-tree-comparison (gcond-code g)) (gcond-rhs g) (gcond-lhs g)
              (gcond-true-label g) (gcond-false-label g))]
      [(and (gassign? g) (gassign-rhs2 g) (not (gassign-rhs3 g))
            (or (commutative-tree-code (gassign-code g)) (comparison-code? (gassign-code g)))
            (tree-swap-operands-p st (gassign-rhs1 g) (gassign-rhs2 g)))
       (gassign (gassign-loc g) (gassign-flags g) (gassign-lhs g) (swap-tree-comparison (gassign-code g))
                (gassign-rhs2 g) (gassign-rhs1 g) #f)]
      [else g]))
  (define (f t)
    (cond
      [(not t) #f]
      [(eq? (tree-code t) 'ssa_name) (if (eqv? (ssa-name-version t) v) val t)]
      [(pvector-empty? (tree-ops t)) t]
      [else (tree-set-ops t (for/pvector ([o (in-pvector (tree-ops t))]) (f o)))]))
  (define (fs g)
    (cond
      [(gassign? g)
       (define lhs (gassign-lhs g))
       (define rhs1 (f (gassign-rhs1 g)))
       ;; a GIMPLE_SINGLE_RHS keeps gimple_assign_rhs_code = TREE_CODE (rhs1)
       (define code (if (eq? (gassign-code g) (tree-code (gassign-rhs1 g))) (tree-code rhs1) (gassign-code g)))
       (gassign (gassign-loc g) (gassign-flags g) (if (eq? (tree-code lhs) 'ssa_name) lhs (f lhs)) code
                rhs1 (f (gassign-rhs2 g)) (f (gassign-rhs3 g)))]
      [(gcall? g)
       (define lhs (gcall-lhs g))
       (gcall (gcall-loc g) (gcall-flags g) (if (or (not lhs) (eq? (tree-code lhs) 'ssa_name)) lhs (f lhs)) (f (gcall-fn g))
              (for/pvector ([x (in-pvector (gcall-args g))]) (f x)) (gcall-fntype g) (gcall-internal? g) (gcall-ecf g))]
      [(gcond? g) (gcond (gcond-loc g) (gcond-flags g) (gcond-code g) (f (gcond-lhs g)) (f (gcond-rhs g))
                         (gcond-true-label g) (gcond-false-label g))]
      [(gswitch? g) (gswitch (gswitch-loc g) (gswitch-flags g) (f (gswitch-index g)) (gswitch-default-case g) (gswitch-cases g))]
      [(greturn? g) (greturn (greturn-loc g) (greturn-flags g) (f (greturn-retval g)))]
      [else g]))
  ;; SET_USE on each replaced PHI argument, update_stmt on each changed statement
  (for/fold ([c c] [n (cfg-next-stamp c)] #:result c) ([i (in-pvector (cfg-bbs c))])
    (define b (cfg-block c i))
    (define-values (phis* n1)
      (restamp-phis (GBlock-phis b)
                    (for/pvector ([ph (in-pvector (GBlock-phis b))])
                      (gphi (gphi-loc ph) (gphi-flags ph) (gphi-var ph) (gphi-result ph)
                            (for/pvector ([arg (in-pvector (gphi-args ph))])
                              (cons (car arg) (f (cdr arg))))))
                    n))
    ;; the virtual operands live in the flags: a removed virtual PHI's
    ;; result is replaced in VUSEs too
    (define (fv g)
      (let* ([g (if (stmt-flag g 'vuse #f) (stmt-set-flag g 'vuse (f (stmt-flag g 'vuse #f))) g)]
             [g (if (stmt-flag g 'vdef #f) (stmt-set-flag g 'vdef (f (stmt-flag g 'vdef #f))) g)])
        g))
    (define-values (stmts* n2)
      (for/fold ([acc (pvector-empty)] [n n1] #:result (values acc n)) ([g (in-pvector (GBlock-stmts b))])
        (define g1 (fs g))
        (define-values (g* n*) (restamp g (fv (if (equal? (stmt-core g1) (stmt-core g)) g1 (fold-replaced st g1 canonicalize))) n))
        (values (pvector-cons-right acc g*) n*)))
    (values (set-block c (block-set-stmts (block-set-phis b phis*) stmts*)) n2)))

;; fold_stmt (no SSA edges followed) on a statement whose uses were
;; replaced: the folder installed by the pass driver
;; (replace-uses-folder: st g -> g), else the operand-order
;; canonicalization alone
(define replace-uses-folder (make-parameter #f))
(define (fold-replaced st g canonicalize)
  (define f (replace-uses-folder))
  (if (and f (or (gassign? g) (gcond? g))) (f st g) (canonicalize g)))

;; gimple_merge_blocks + cfghooks merge_blocks
(define (merge-blocks st c00 a b)
  ;; gimple_merge_blocks: with the same loop father the larger count is kept
  ;; (profile_count::merge; all counts here have the same quality)
  (define c
    (let ([pf (GCfg-profile c00)])
      (cond
        [(not pf) c00]
        [else
         (define-values (loops _b) (find-loops c00 (cfg-dom c00)))
         (define la (loop-of loops a)) (define lb (loop-of loops b))
         (define ca (prof-count c00 a)) (define cb (prof-count c00 b))
         (if (and (eqv? (and la (Loop-num la)) (and lb (Loop-num lb))) ca cb (> cb ca))
             (cfg-set-profile c00 (GProf (intmap-set (GProf-counts pf) a cb) (GProf-probs pf)))
             c00)])))
  ;; V_i = PHI <V_j> in B: V_j replaces V_i everywhere (A is B's only pred);
  ;; remove_phi_node (.., true) releases V_i
  (define c0
    (for/fold ([c (dump c "Merging blocks ~a and ~a" a b)]) ([ph (in-pvector (GBlock-phis (cfg-block c b)))])
      (log-ssa-release! (ssa-name-version (gphi-result ph)))
      (replace-uses-by st c (gphi-result ph) (cdr (pvector-first (gphi-args ph))))))
  (define c1 (link-block (unlink-block (set-block c0 (block-set-phis (cfg-block c0 b) (pvector-empty))) b) b a))
  ;; B's labels are dropped (a FORCED_LABEL moves to the start of A)
  (define-values (c2 kept)
    (for/fold ([c c1] [acc (pvector-empty)] #:result (values c acc)) ([g (in-pvector (stmts c1 b))])
      (cond
        [(glabel? g)
         (if (label-forced? st (glabel-label g))
             (values (register-label (set-stmts c a (pvector-cons-left (stmts c a) g)) (glabel-label g) a) acc)
             (values c acc))]
        [else (values c (pvector-cons-right acc g))])))
  (let* ([c (set-stmts c2 a (pvector-append (stmts c2 a) kept))]
         [c (set-stmts c b (pvector-empty))]
         [c (altered c a)]
         ;; cfghooks.cc merge_blocks: A takes over B's successor edges (the
         ;; edges keep their probabilities)
         [c (for/fold ([c c]) ([e (in-pvector (cfg-succs c a))]) (remove-edge c a (GEdge-dest e)))]
         [c (for/fold ([c c]) ([e (in-pvector (cfg-succs c b))])
              (define d (GEdge-dest e))
              (define pf (GCfg-profile c))
              (define p (and pf (hash-ref (GProf-probs pf) (cons b d) #f)))
              (if p
                  (cfg-set-profile c (GProf (GProf-counts pf) (hash-set (hash-remove (GProf-probs pf) (cons b d)) (cons a d) p)))
                  c))]
         [c (for/fold ([c c]) ([e (in-pvector (cfg-succs c b))])
              (define d (GEdge-dest e))
              (define db (cfg-block c d))
              ;; the PHI arguments of D keep their slots; only the predecessor index changes
              (set-block c (block-set-preds (block-set-phis db (for/pvector ([ph (in-pvector (GBlock-phis db))])
                                                                 (gphi (gphi-loc ph) (gphi-flags ph) (gphi-var ph) (gphi-result ph)
                                                                       (for/pvector ([x (in-pvector (gphi-args ph))])
                                                                         (if (= (car x) b) (cons a (cdr x)) x)))))
                                            (pvector-set (GBlock-preds db) (pvector-index-of (GBlock-preds db) b) a))))]
         [c (set-succs c a (cfg-succs c b))]
         [c (set-succs c b (pvector-empty))]
         [c (set-preds c b (pvector-empty))])
    (expunge-block c b)))

;; ---- edge redirection ------------------------------------------------------

;; gimple_block_label: (values label cfg st)
(define (gimple-block-label st c i)
  (define n (bb-leading-label-count c i))
  (define found
    (for/first ([k (in-range n)] #:unless (label-nonlocal? st (glabel-label (pvector-ref (stmts c i) k)))) k))
  (cond
    [found
     (define g (pvector-ref (stmts c i) found))
     (define c1 (if (zero? found) c (set-stmts c i (pvector-cons-left (pvector-append (pvector-take (stmts c i) found) (pvector-drop (stmts c i) (add1 found))) g))))
     (values (glabel-label g) c1 st)]
    [else
     (let/st st ([l (create-artificial-label st)])
       (define-values (st1 c1) (bb-insert-stmt-at st c i 0 (gimple-build-label l)))
       (values l c1 st1))]))

;; gimple_try_redirect_by_replacing_jump: (values ok? cfg)
(define (try-redirect-by-replacing-jump c src old target)
  (define succs (cfg-succs c src))
  (cond
    [(not (= 2 (pvector-length succs))) (values #f c)]
    [(not (= target (GEdge-dest (let ([e0 (pvector-ref succs 0)]) (if (= (GEdge-dest e0) old) (pvector-ref succs 1) e0)))))
     (values #f c)]
    [else
     (define g (bb-last-stmt c src))
     (cond
       [(or (gcond? g) (gswitch? g))
        (let* ([c (bb-remove-last-stmt c src)]
               [c (redirect-edge-succ-nodup c src old target)]
               [c (set-edge-flags c src target EDGE-FALLTHRU)])
          (values #t c))]
       [else (values #f c)])]))

;; gimple_redirect_edge_and_branch (normal edges only): (values cfg st)
(define (redirect-edge-and-branch st c src old dest)
  (cond
    [(= old dest) (values c st)]
    [else
     (define-values (ok? c1) (if (= src ENTRY) (values #f c) (try-redirect-by-replacing-jump c src old dest)))
     (cond
       [ok? (values c1 st)]
       [else
        (define g (bb-last-stmt c1 src))
        (define-values (c2 st2)
          (cond
            [(gswitch? g)
             (let-values ([(label c st) (gimple-block-label st c1 dest)])
               (define g* (bb-last-stmt c src))
               (define (fix cs) (if (eqv? (label-to-block c (case-label cs)) old) (tree-set-operand cs 2 label) cs))
               (define g2 (gswitch (gswitch-loc g*) (gswitch-flags g*) (gswitch-index g*) (fix (gswitch-default-case g*))
                                   (for/pvector ([cs (in-pvector (gswitch-cases g*))]) (fix cs))))
               (values (cfg-set-touched (bb-set-last-stmt c src g2) (intbits-set (GCfg-touched c) src)) st))]
            [(ggoto? g) (error 'redirect-edge-and-branch "simple gotos were removed by make_edges")]
            [else (values c1 st)]))
        (values (redirect-edge-succ-nodup c2 src old dest) st2)])]))

;; split_edge (cfghooks + gimple_split_edge): a new empty block on the edge
;; SRC -> DEST, placed after split_edge_bb_loc; DEST's PHI arguments keep
;; their slot (the new edge takes the old one).  (values new-bb cfg)
(define (split-edge c src dest)
  (define dest-prev (bb-prev c dest))
  (define after
    (if (and dest-prev (let ([e (find-edge c dest-prev dest)]) (and e (not (edge-flag? e EDGE-COMPLEX)))))
        src
        (or dest-prev src)))
  (define-values (nb c1) (create-bb c (pvector-empty) after))
  (define-values (_new c2) (make-edge c1 nb dest EDGE-FALLTHRU))
  (define pk (pvector-index-of (cfg-preds c2 dest) src))
  ;; redirect: the pred slot of SRC becomes NB (unordered_remove moves the last, i.e. NB, into it)
  (define e (find-edge c2 src dest))
  (define db (cfg-block c2 dest))
  (define c3
    (let* ([c (prof-move-edge c2 src dest nb)]
           ;; gimple_split_edge hides DEST's PHIs while redirecting: the
           ;; argument keeps its slot and its use-list position (no restamp)
           [c (set-block c (block-set-preds (block-set-phis db (for/pvector ([ph (in-pvector (GBlock-phis db))])
                                                                 (gphi (gphi-loc ph) (gphi-flags ph) (gphi-var ph) (gphi-result ph)
                                                                       (for/pvector ([a (in-pvector (gphi-args ph))])
                                                                         (if (= (car a) src) (cons nb (cdr a)) a)))))
                                             (pvector-unordered-remove (GBlock-preds db) pk)))]
           [c (set-succs c src (pvector-set (cfg-succs c src) (edge-index c src dest) (GEdge nb (GEdge-flags e) (GEdge-goto-locus e))))]
           [c (set-preds c nb (pvector-cons-right (cfg-preds c nb) src))])
      c))
  ;; the new block's count is the edge count (src count scaled by the
  ;; probability, RDIV) and its single successor edge is always taken
  (define pf (GCfg-profile c3))
  (define c4
    (cond
      [pf
       (define src-count (intmap-ref (GProf-counts pf) src #f))
       (define pr (hash-ref (GProf-probs pf) (cons src nb) #f))   ; moved there by prof-move-edge
       (define cnt (and src-count pr (quotient (+ (* src-count pr) (quotient 134217728 2)) 134217728)))
       (cfg-set-profile c3 (GProf (if cnt (intmap-set (GProf-counts pf) nb cnt) (GProf-counts pf))
                                  (hash-set (GProf-probs pf) (cons nb dest) 134217728)))]
      [else c3]))
  (values nb c4))

;; gimple_split_edge for any source: a switch source sends the cases of
;; DEST to a label of the new block (gimple_redirect_edge_and_branch with
;; gimple_block_label); (values nb cfg st)
(define (split-edge/st st c src dest)
  (define g (bb-last-stmt c src))
  (define-values (nb c1) (split-edge c src dest))
  (cond
    [(gswitch? g)
     (define-values (label c2 st2) (gimple-block-label st c1 nb))
     (define (fix cs) (if (eqv? (label-to-block c2 (case-label cs)) dest) (tree-set-operand cs 2 label) cs))
     (define g2 (gswitch (gswitch-loc g) (gswitch-flags g) (gswitch-index g) (fix (gswitch-default-case g))
                         (for/pvector ([cs (in-pvector (gswitch-cases g))]) (fix cs))))
     (values nb (cfg-set-touched (bb-set-last-stmt c2 src g2) (intbits-set (GCfg-touched c2) src)) st2)]
    [else (values nb c1 st)]))

;; split_critical_edges (true): critical edges, and the edges out of a
;; control statement into a block with several predecessors, PHIs or the
;; exit (for_edge_insertion_p); every block of the chain, new ones included
(define (split-edges-for-insertion st c0 #:for-insertion? [for-insertion? #t])
  (let loop ([bb ENTRY] [c c0] [st st])
    (cond
      [(not bb) (values c st)]
      [else
       (define-values (c1 st1)
         (let eloop ([k 0] [c c] [st st])
           (define ss (cfg-succs c bb))
           (cond
             [(>= k (pvector-length ss)) (values c st)]
             [else
              (define e (pvector-ref ss k))
              (define d (GEdge-dest e))
              (define critical (and (>= (pvector-length ss) 2) (>= (pvector-length (cfg-preds c d)) 2)))
              (define last (and (not (= bb ENTRY)) (not (= bb EXIT)) (bb-last-stmt c bb)))
              (define insertion
                (and for-insertion?
                     (or (not (= 1 (pvector-length (cfg-preds c d))))
                         (and (cfg-block c d) (not (pvector-empty? (GBlock-phis (cfg-block c d)))))
                         (= d EXIT))
                     (not (= bb ENTRY))
                     last (stmt-ends-bb? last) (not (greturn? last))))
              (cond
                [(and (not (edge-flag? e EDGE-ABNORMAL)) (or critical insertion))
                 (define-values (_nb c2 st2) (split-edge/st st c bb d))
                 (eloop (add1 k) c2 st2)]
                [else (eloop (add1 k) c st)])])))
       (loop (if (= bb EXIT) #f (or (bb-next c1 bb) EXIT)) c1 st1)])))

;; ---- natural loops ----------------------------------------------------------

;; dominators over block indices (ENTRY first)
(define (cfg-dom c)
  (compute-dom (pvector-cons-left (pvector-cons-left (cfg-bbs c) EXIT) ENTRY) ENTRY
               (lambda (i) (for/pvector ([e (in-pvector (cfg-succs c i))]) (GEdge-dest e)))))

;;   (find-loops cfg dom) -> (values loops back-edges)
;;     loops      : list of Loop in discovery order (num = position + 1,
;;                  loop 0 is the function); Loop (num header latches body
;;                  parent depth) with body an intbits of block indices
;;     back-edges : hash (src . dest) -> #t  (dest dominates src)
;;   (loop-of loops bb) -> the innermost Loop containing BB or #f
(struct Loop (num header latches body parent depth) #:transparent)

(define (dominates? dom a b) (dom-dominates? dom (dom-index dom a) (dom-index dom b)))

;; flow_loops_find: headers in RPO, bodies by the backward walk from the latches
(define (find-loops cfg dom)
  (define back (make-hash))
  (define rpo (for/list ([b (in-pvector (dom-rpo dom))] #:unless (or (= b ENTRY) (= b EXIT))) b))
  (define headers
    (for/list ([bb (in-list rpo)]
               #:when (for/or ([src (in-pvector (cfg-preds cfg bb))])
                        (and (not (= src ENTRY)) (dom-reachable? dom src) (dominates? dom bb src))))
      bb))
  (define loops
    (for/list ([h (in-list headers)] [i (in-naturals 1)])
      (define latches (for/list ([src (in-pvector (cfg-preds cfg h))]
                                 #:when (and (not (= src ENTRY)) (dom-reachable? dom src) (dominates? dom h src)))
                        (hash-set! back (cons src h) #t)
                        src))
      ;; flow_loop_nodes_find: everything that reaches a latch without passing the header
      (define body
        (let loop ([work latches] [seen (intbits-set intbits-empty h)])
          (cond
            [(null? work) seen]
            [(intbits-ref seen (car work)) (loop (cdr work) seen)]
            [else
             (define b (car work))
             (loop (append (cdr work) (for/list ([p (in-pvector (cfg-preds cfg b))] #:unless (= p ENTRY)) p))
                   (intbits-set seen b))])))
      (Loop i h latches body #f 0)))
  ;; nesting: the parent is the innermost other loop whose body contains the header
  (define with-parents
    (for/list ([l (in-list loops)])
      (define candidates
        (for/list ([o (in-list loops)]
                   #:when (and (not (eq? o l)) (intbits-ref (Loop-body o) (Loop-header l))))
          o))
      (define parent
        (and (pair? candidates)
             (argmin (lambda (o) (intbits-count (Loop-body o))) candidates)))
      (Loop (Loop-num l) (Loop-header l) (Loop-latches l) (Loop-body l) (and parent (Loop-num parent)) 0)))
  (define (depth-of l)
    (let loop ([l l] [d 1])
      (if (Loop-parent l) (loop (findf (lambda (o) (= (Loop-num o) (Loop-parent l))) with-parents) (add1 d)) d)))
  (values (for/list ([l (in-list with-parents)])
            (Loop (Loop-num l) (Loop-header l) (Loop-latches l) (Loop-body l) (Loop-parent l) (depth-of l)))
          back))

(define (loop-inside? l bb) (intbits-ref (Loop-body l) bb))

;; the innermost loop containing BB
(define (loop-of loops bb)
  (define cs (filter (lambda (l) (loop-inside? l bb)) loops))
  (and (pair? cs) (argmax Loop-depth cs)))


;; tree_forwarder_block_p's current_loops part: BB (a forwarder to DEST)
;; is kept when it is a loop header, a simple latch or a preheader the
;; loop state asks for ('loops-state of the function: 'simple-latches,
;; 'preheaders), or another edge into a loop header
(define (loop-protected-forwarder? st c bb dest)
  (define state (let ([d (GS-fndecl st)]) (if d (decl-flag st d 'loops-state '()) '())))
  (define-values (loops _b) (find-loops c (cfg-dom c)))
  (define (header-of b) (findf (lambda (l) (= (Loop-header l) b)) loops))
  (define (num l) (and l (Loop-num l)))
  (cond
    [(null? loops) #f]
    [(header-of bb) #t]
    [(header-of dest)
     => (lambda (dl)
          (define bl (loop-of loops bb))
          (cond
            [(eqv? (num bl) (num dl)) (and (memq 'simple-latches state) #t)]
            [(eqv? (num bl) (Loop-parent dl)) (and (memq 'preheaders state) #t)]
            [else #t]))]
    [else #f]))

;; release_defs: the SSA definitions of a statement (VDEF first, then the lhs)
(define (stmt-defs-for-release g)
  (define vd (stmt-flag g 'vdef #f))
  (define lhs (cond [(gassign? g) (gassign-lhs g)] [(gcall? g) (gcall-lhs g)] [else #f]))
  (append (if (and vd (eq? (tree-code vd) 'ssa_name)) (list vd) '())
          (if (and lhs (eq? (tree-code lhs) 'ssa_name)) (list lhs) '())))

;; ---- forwarder blocks -------------------------------------------------------

;; maybe_remove_forwarder_block (no PHIs, no loops, no abnormal edges):
;; (values removed? cfg st)
(define (maybe-remove-forwarder-block st c i)
  (cond
    [(not (single-succ-p c i)) (values #f c st)]
    ;; tree_forwarder_block_p (bb, phi_wanted = false): no PHI nodes
    [(not (pvector-empty? (GBlock-phis (cfg-block c i)))) (values #f c st)]
    [(= (single-succ c i) EXIT) (values #f c st)]
    [(= (single-succ c i) i) (values #f c st)]
    [(edge-flag? (single-succ-edge c i) EDGE-ABNORMAL) (values #f c st)]
    [else
     (define locus (GEdge-goto-locus (single-succ-edge c i)))
     (define dest (single-succ c i))
     (cond
       [(for/or ([src (in-pvector (cfg-preds c i))])
          (define e (find-edge c src i))
          (or (= src ENTRY)
              (edge-flag? e EDGE-EH)
              ;; -O0: a differing goto_locus on any incoming edge keeps the block
              (and (not (optimize? st))
                   (or (loc-has-location? (GEdge-goto-locus e)) (loc-has-location? locus))
                   (not (equal? (GEdge-goto-locus e) locus)))))
        (values #f c st)]
       ;; trunk (GCC 16, 40c037adb36): leave single-pred/single-succ chains to block merging
       [(and (>= (gcc-major-version st) 16) (single-pred-p c i) (single-succ-p c (single-pred c i))) (values #f c st)]
       [(let ([f (bb-first-stmt c dest)]) (and (glabel? f) (label-nonlocal? st (glabel-label f)))) (values #f c st)]
       [(not (for/and ([g (in-pvector (stmts c i))])
               (and (glabel? g)
                    (not (label-nonlocal? st (glabel-label g)))
                    (not (and (not (optimize? st))
                              (or (stmt-has-location? g) (loc-has-location? locus))
                              (not (equal? (stmt-loc g) locus)))))))
        (values #f c st)]
       [(loop-protected-forwarder? st c i dest) (values #f c st)]
       ;; PHIs in DEST: a predecessor that already reaches DEST must agree
       ;; on every PHI argument (phi_alternatives_equal); the split-edge
       ;; fallback is not ported
       [(and (not (pvector-empty? (GBlock-phis (cfg-block c dest))))
             (for/or ([src (in-pvector (cfg-preds c i))])
               (and (find-edge c src dest)
                    (not (for/and ([ph (in-pvector (GBlock-phis (cfg-block c dest)))])
                           (equal? (phi-arg ph src) (phi-arg ph i)))))))
        (values #f c st)]
       [else
        ;; the PHI arguments DEST receives along the edge from this block
        (define bb-args (for/list ([ph (in-pvector (GBlock-phis (cfg-block c dest)))]) (phi-arg ph i)))
        ;; redirect all predecessors to DEST; a new edge gets this block's PHI arguments
        (define-values (c1 st1)
          (let loop ([c c] [st st])
            (cond
              [(pvector-empty? (cfg-preds c i)) (values c st)]
              [else
               (define src (pvector-first (cfg-preds c i)))
               (define had-edge? (and (find-edge c src dest) #t))
               (define-values (c1 st1) (redirect-edge-and-branch st (altered c src) src i dest))
               (define c2
                 (if had-edge?
                     c1
                     (let ([b (cfg-block c1 dest)])
                       (define-values (phis* _n)
                         (restamp-phis (GBlock-phis b)
                                       (for/pvector ([ph (in-pvector (GBlock-phis b))] [arg (in-list bb-args)])
                                         (gphi (gphi-loc ph) (gphi-flags ph) (gphi-var ph) (gphi-result ph)
                                               (pvector-cons-right (gphi-args ph) (cons src arg))))
                                       (cfg-next-stamp c1)))
                       (set-block c1 (block-set-phis b phis*)))))
               (loop c2 st1)])))
        ;; move user / forced / nonlocal labels to the start of DEST
        (define moved
          (for/pvector ([g (in-pvector (stmts c1 i))]
                        #:when (let ([l (glabel-label g)])
                                 (or (label-nonlocal? st1 l) (label-forced? st1 l) (not (decl-flag st1 l 'artificial)))))
            g))
        (let* ([c (for/fold ([c c1]) ([g (in-pvector moved)]) (register-label c (glabel-label g) dest))]
               [c (set-stmts c dest (pvector-append moved (stmts c dest)))]
               [c (set-stmts c i (pvector-filter-not (stmts c i) (lambda (g) (pvector-index-of moved g))))]
               [c (altered c dest)]
               [c (delete-basic-block c i)])
          (values #t c st1))])]))

;; cleanup_tree_cfg_bb: (values changed? cfg st)
(define (cleanup-tree-cfg-bb st c i)
  (define-values (removed? c1 st1) (maybe-remove-forwarder-block st c i))
  (cond
    [removed? (values #t c1 st1)]
    [(and (single-pred-p c1 i) (can-merge-blocks? st1 c1 (single-pred c1 i) i))
     (values #f (altered c1 (single-pred c1 i)) st1)]
    [(and (single-succ-p c1 i) (can-merge-blocks? st1 c1 i (single-succ c1 i)))
     (values #t (merge-blocks st1 c1 i (single-succ c1 i)) st1)]
    [else (values #f c1 st1)]))

;; compact_blocks: renumber along the chain from 2
(define (compact-blocks c)
  (define order (cfg-bbs c))
  (define remap (for/fold ([m (intmap-set (intmap-set intmap-empty ENTRY ENTRY) EXIT EXIT)])
                          ([i (in-pvector order)] [k (in-naturals 2)])
                  (intmap-set m i k)))
  (define (r i) (and i (intmap-ref remap i)))
  (define blocks
    (for/fold ([m intmap-empty]) ([p (in-list (intmap->literal-datum (GCfg-blocks c)))])
      (define b (cdr p))
      (intmap-set m (r (GBlock-index b))
                  (GBlock (r (GBlock-index b))
                          (for/pvector ([ph (in-pvector (GBlock-phis b))])
                            (gphi (gphi-loc ph) (gphi-flags ph) (gphi-var ph) (gphi-result ph)
                                  (for/pvector ([x (in-pvector (gphi-args ph))]) (cons (r (car x)) (cdr x)))))
                          (GBlock-stmts b) (r (GBlock-prev b)) (r (GBlock-next b))
                          (for/pvector ([s (in-pvector (GBlock-preds b))]) (r s))))))
  (define edges
    (for/fold ([m intmap-empty]) ([p (in-list (intmap->literal-datum (GCfg-edges c)))])
      (intmap-set m (r (car p))
                  (for/pvector ([e (in-pvector (cdr p))]) (GEdge (r (GEdge-dest e)) (GEdge-flags e) (GEdge-goto-locus e))))))
  (define l->bb
    (for/fold ([m intmap-empty]) ([p (in-list (intmap->literal-datum (GCfg-label->bb c)))])
      (define k (intmap-ref remap (cdr p) #f))
      (if k (intmap-set m (car p) k) m)))
  (define prof
    (let ([pf (GCfg-profile c)])
      (and pf
           (GProf (for/fold ([m intmap-empty]) ([p (in-list (intmap->literal-datum (GProf-counts pf)))])
                    (define k (intmap-ref remap (car p) #f))
                    (if k (intmap-set m k (cdr p)) m))
                  (for/fold ([h (hash)]) ([(k v) (in-hash (GProf-probs pf))])
                    (define a (intmap-ref remap (car k) #f)) (define b (intmap-ref remap (cdr k) #f))
                    (if (and a b) (hash-set h (cons a b) v) h))))))
  (GCfg blocks edges l->bb (GCfg-next-label-uid c) (GCfg-messages c) intbits-empty intbits-empty prof #f))

;; cleanup_tree_cfg_noloop: (values cfg st).  Not ported: the
;; loop-header forwarder blocks created at its start
;; (make_forwarder_block for headers with several entries) and
;; repair_loop_structures (LOOPS_NEED_FIXUP, then LCSSA repair).
;; AFTER-PRE: the SSA update of the TODO flags, run once the control
;; flow is cleaned (cleanup_tree_cfg_noloop's update_ssa):
;; (st cfg pre-released) -> (values cfg st), PRE-RELEASED the versions
;; the pre phase released (removed blocks), in order
(define (cleanup-tree-cfg st0 c cond->string #:after-pre [after-pre #f])
  (define c0 (cfg-set-altered c intbits-empty))
  (define (logged) (let ([b (ssa-pass-log)]) (if b (hash-ref (unbox b) 'released '()) '())))
  (define logged0 (length (logged)))
  (define-values (_changed c1a) (cleanup-control-flow-pre st0 c0 cond->string))
  ;; the names released by the pre phase, in release order: the hook
  ;; (update_ssa) releases the unregistered ones before its own
  (define pre-released (let ([l (logged)]) (reverse (take l (- (length l) logged0)))))
  (define-values (c1 st) (if after-pre (after-pre st0 c1a pre-released) (values c1a st0)))
  (define-values (c2 st2)
    (for/fold ([c c1] [st st] #:result (values c st)) ([i (in-range 2 (last-basic-block c1))])
      (cond
        [(cfg-block c i) (let-values ([(_ch c st) (cleanup-tree-cfg-bb st c i)]) (values c st))]
        [else (values c st)])))
  ;; the worklist of altered blocks, smallest index first
  (define-values (c3 st3)
    (let loop ([c c2] [st st2])
      (cond
        [(intbits-empty? (GCfg-altered c)) (values c st)]
        [else
         (define i (intbits-first (GCfg-altered c)))
         (define c1 (cfg-set-altered c (intbits-clear (GCfg-altered c) i)))
         (cond
           [(and (>= i 2) (cfg-block c1 i))
            (define-values (_ch c2) (cleanup-control-flow-bb st c1 i cond->string))
            (define-values (_ch2 c3 st3) (cleanup-tree-cfg-bb st c2 i))
            (loop c3 st3)]
           [else (loop c1 st)])])))
  ;; end_recording_case_labels
  (define c4
    (for/fold ([c c3]) ([i (in-list (intbits->list (GCfg-touched c3)))])
      (define g (and (cfg-block c i) (bb-last-stmt c i)))
      (if (gswitch? g) (bb-set-last-stmt c i (group-case-labels-stmt c g)) c)))
  ;; compact_blocks unless the SCEV cache (indexed by block number) is active
  (define scev? (let ([d (GS-fndecl st3)]) (and d (memq 'scev (decl-flag st3 d 'loops-state '())))))
  (values (if scev? c4 (compact-blocks c4)) st3))

;; ------------------------------------------------------------
;; Entry point: execute_build_cfg: (values cfg st)
;; ------------------------------------------------------------

(define (build-cfg st fndecl body #:cond->string [cond->string (lambda (st g) "<cond>")])
  (define st0 (gs-set-fndecl st fndecl))
  (define blocks (intmap-set (intmap-set intmap-empty ENTRY (GBlock ENTRY (pvector-empty) (pvector-empty) #f EXIT (pvector-empty)))
                             EXIT (GBlock EXIT (pvector-empty) (pvector-empty) ENTRY #f (pvector-empty))))
  (define c0 (GCfg blocks intmap-empty intmap-empty 0 (pvector-empty) intbits-empty intbits-empty #f #f))
  (define-values (c1 st1) (make-blocks st0 c0 body))
  (define c2 (if (= (bb-next c1 ENTRY) EXIT) (let-values ([(_i c) (create-bb c1 (pvector-empty) ENTRY)]) c) c1))
  (let* ([c (cleanup-dead-labels st1 c2)]
         [c (group-case-labels c)]
         [c (make-edges c)]
         [c (assign-discriminators c)]
         [c (cleanup-dead-labels st1 c)])
    (cleanup-tree-cfg st1 c cond->string)))
