#lang racket/base

;; ============================================================
;; opt: natural loops (gcc/cfgloop.cc flow_loops_find) and the
;; LOOPS_NORMAL normalization of loop_optimizer_init
;; ============================================================
;;
;;   (find-loops cfg dom) -> (values loops back-edges)
;;     loops      : list of Loop in discovery order (num = position + 1,
;;                  loop 0 is the function); Loop (num header latches body
;;                  parent depth) with body an intbits of block indices
;;     back-edges : hash (src . dest) -> #t  (dest dominates src)
;;   (normalize-loops st cfg) -> cfg
;;     create_preheaders (CP_SIMPLE_PREHEADERS) then
;;     force_single_succ_latches, both through split-edge; a loop with
;;     several latch edges or several entry edges is left alone
;;     (make_forwarder_block is not ported)
;;   (dfs-back-edges cfg) -> hash (src . dest) -> #t   (mark_dfs_back_edges)
;;   (loop-exits cfg loop) -> list of GEdge-like (cons src dest)
;;   (loop-of loops bb) -> the innermost Loop containing BB or #f
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../../../driver/dominance/dom.rkt"
         "../cfg.rkt")

;; Loop, find-loops, loop-inside?, loop-of live in cfg.rkt (CFG cleanup
;; protects loop headers, latches and preheaders)
(provide (struct-out Loop) find-loops normalize-loops dfs-back-edges loop-exits loop-of loop-latch-edge
         loop-preheader-edge loop-inside? loops-innermost-first loops-preorder)

;; loops_list walks (cfgloop.cc walk_loop_tree).  flow_loops_find adds
;; the loops in discovery order and flow_loop_tree_node_add puts each
;; at the head of its father's inner list, so the sons of a loop run in
;; descending number order.
;;   loops-preorder       : loops_list (fn, 0), a loop before its sons
;;   loops-innermost-first: loops_list (fn, LI_FROM_INNERMOST), postorder
(define (loop-sons loops parent)
  (sort (for/list ([l (in-list loops)] #:when (eqv? (Loop-parent l) parent)) l) > #:key Loop-num))
(define (loops-preorder loops)
  (let walk ([p #f])
    (apply append (for/list ([l (in-list (loop-sons loops p))]) (cons l (walk (Loop-num l)))))))
(define (loops-innermost-first loops)
  (let walk ([p #f])
    (apply append (for/list ([l (in-list (loop-sons loops p))]) (append (walk (Loop-num l)) (list l))))))

;; get_loop_exit_edges: (cons src dest) for edges leaving the body, in block/edge order
(define (loop-exits cfg l)
  (for*/list ([src (in-list (intbits->list (Loop-body l)))]
              [e (in-pvector (cfg-succs cfg src))]
              #:unless (loop-inside? l (GEdge-dest e)))
    (cons src (GEdge-dest e))))

(define (loop-latch-edge l) (cons (car (Loop-latches l)) (Loop-header l)))
(define (loop-preheader-edge cfg l)
  (define h (Loop-header l))
  (define ps (for/list ([p (in-pvector (cfg-preds cfg h))] #:unless (memv p (Loop-latches l))) p))
  (and (= 1 (length ps)) (cons (car ps) h)))

;; mark_dfs_back_edges: a DFS from ENTRY in successor order
(define (dfs-back-edges cfg)
  (define back (make-hash))
  (define state (make-hasheqv))        ; bb -> 'open | 'done
  (let dfs ([bb ENTRY])
    (hash-set! state bb 'open)
    (for ([e (in-pvector (cfg-succs cfg bb))])
      (define d (GEdge-dest e))
      (case (hash-ref state d #f)
        [(open) (hash-set! back (cons bb d) #t)]
        [(done) (void)]
        [else (dfs d)]))
    (hash-set! state bb 'done))
  back)

;; ------------------------------------------------------------
;; loop_optimizer_init (LOOPS_NORMAL): preheaders, then simple latches
;; ------------------------------------------------------------

(define (normalize-loops st cfg0)
  (define (dom-of cfg)
    (compute-dom (pvector-cons-left (pvector-cons-left (cfg-bbs cfg) EXIT) ENTRY) ENTRY
                 (lambda (i) (for/pvector ([e (in-pvector (cfg-succs cfg i))]) (GEdge-dest e)))))
  (define-values (loops _back) (find-loops cfg0 (dom-of cfg0)))
  ;; create_preheaders (CP_SIMPLE_PREHEADERS), loops_list (cfun, 0)
  (define cfg1
    (for/fold ([cfg cfg0]) ([l (in-list (loops-preorder loops))])
      (define h (Loop-header l))
      (define entries (for/list ([p (in-pvector (cfg-preds cfg h))] #:unless (memv p (Loop-latches l))) p))
      (cond
        [(not (= 1 (length entries))) cfg]           ; make_forwarder_block not ported
        [else
         (define src (car entries))
         (define need?
           (or (= src ENTRY)
               (not (= 1 (pvector-length (cfg-succs cfg src))))
               (let ([last (bb-last-stmt cfg src)]) (and last (stmt-ends-bb? last)))))
         ;; create_preheader: "Created preheader block for loop N" whenever dump_file
         (if need?
             (let-values ([(_nb c) (split-edge cfg src h)]) (cfg-add-message c (format "Created preheader block for loop ~a" (Loop-num l))))
             cfg)])))
  ;; force_single_succ_latches, loops_list (cfun, 0)
  (define-values (loops1 _b1) (find-loops cfg1 (dom-of cfg1)))
  (for/fold ([cfg cfg1]) ([l (in-list (loops-preorder loops1))])
    (cond
      [(not (= 1 (length (Loop-latches l)))) cfg]
      [else
       (define latch (car (Loop-latches l)))
       (if (and (not (= latch (Loop-header l))) (= 1 (pvector-length (cfg-succs cfg latch))))
           cfg
           (let-values ([(_nb c) (split-edge cfg latch (Loop-header l))]) c))])))
