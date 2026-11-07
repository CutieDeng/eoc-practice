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

(provide (struct-out Loop) find-loops normalize-loops dfs-back-edges loop-exits loop-of loop-latch-edge
         loop-preheader-edge loop-inside? loops-innermost-first)

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

;; loops_list (LI_FROM_INNERMOST): deeper loops first, then by number
(define (loops-innermost-first loops)
  (sort loops (lambda (a b) (or (> (Loop-depth a) (Loop-depth b))
                                (and (= (Loop-depth a) (Loop-depth b)) (< (Loop-num a) (Loop-num b)))))))

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
  ;; create_preheaders (CP_SIMPLE_PREHEADERS)
  (define cfg1
    (for/fold ([cfg cfg0]) ([l (in-list loops)])
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
  ;; force_single_succ_latches
  (define-values (loops1 _b1) (find-loops cfg1 (dom-of cfg1)))
  (for/fold ([cfg cfg1]) ([l (in-list loops1)])
    (cond
      [(not (= 1 (length (Loop-latches l)))) cfg]
      [else
       (define latch (car (Loop-latches l)))
       (if (and (not (= latch (Loop-header l))) (= 1 (pvector-length (cfg-succs cfg latch))))
           cfg
           (let-values ([(_nb c) (split-edge cfg latch (Loop-header l))]) c))])))
