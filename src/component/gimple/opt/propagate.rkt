#lang racket/base

;; ============================================================
;; opt: the SSA propagation engine (gcc/tree-ssa-propagate.cc,
;; ssa_propagation_engine) over a GCfg
;; ============================================================
;;
;; Port of ssa_prop_init / add_control_edge / add_ssa_edge /
;; simulate_stmt / simulate_block / ssa_propagate.  The client (a pass
;; such as CCP) supplies the three callbacks of the C++ class:
;;
;;   init!      : eng -> void                      (sets simulate-again flags, seeds its lattice)
;;   visit-stmt : eng uid stmt -> (values result taken-dest output-name)
;;   visit-phi  : eng uid phi  -> (values result output-name)
;;
;; result is one of 'varying 'interesting 'not-interesting.
;;
;; Statements are identified by their uid: the position in the RPO
;; walk of the blocks, PHIs first (gimple_set_uid in ssa_prop_init).
;; The CFG is not modified during propagation, so a uid maps to a fixed
;; (block, phi-or-stmt, index).  The engine keeps its worklists and
;; flags in local mutable tables; the result handed back is the engine
;; itself, queried for EDGE_EXECUTABLE and the simulate-again flags.
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../../../driver/dominance/dom.rkt"
         "../cfg.rkt"
         "../ssa.rkt"
         "ssa-ops.rkt")

(provide ssa-propagate
         eng-cfg eng-dom eng-stmt eng-uid-bb eng-phi-uid? eng-def-uid eng-def-stmt
         eng-simulate-again? eng-set-simulate-again! eng-edge-executable? eng-block-visited?
         eng-uids-of-block)

;; order->bb   : vector of block indices in RPO (ENTRY/EXIT excluded)
;; bb->order   : hasheqv block -> order
;; uid->stmt   : vector (gphi or statement)
;; uid->bb     : vector
;; uid->phi?   : vector of booleans
;; block-uids  : hasheqv bb -> (cons phi-uids stmt-uids) (lists, in order)
;; def->uid    : hasheqv version -> uid
;; uses        : hasheqv version -> list of (cons uid pred-or-#f)   (real uses)
;; sim-again   : vector of booleans
;; exec        : mutable hash (cons src dest) -> #t
;; visited     : hasheqv bb -> #t
;; cfg-blocks / ssa-wl : boxes holding intbits (orders / uids)
;; back?       : GCC <= 12 (before r13-5077, PR108353): back-edge
;;               destinations and uses in earlier blocks go to the
;;               *-back worklists, processed once the regular ones are
;;               empty; curr-order is the order of the block being simulated
;; cfg-blocks-back / ssa-wl-back / curr-order : boxes
(struct eng (cfg dom order->bb bb->order uid->stmt uid->bb uid->phi? block-uids def->uid uses sim-again exec visited
                 cfg-blocks ssa-wl back? cfg-blocks-back ssa-wl-back curr-order))

(define (eng-stmt e uid) (vector-ref (eng-uid->stmt e) uid))
(define (eng-uid-bb e uid) (vector-ref (eng-uid->bb e) uid))
(define (eng-phi-uid? e uid) (vector-ref (eng-uid->phi? e) uid))
(define (eng-def-uid e name) (hash-ref (eng-def->uid e) (ssa-name-version name) #f))
(define (eng-def-stmt e name) (let ([u (eng-def-uid e name)]) (and u (eng-stmt e u))))
(define (eng-simulate-again? e uid) (vector-ref (eng-sim-again e) uid))
(define (eng-set-simulate-again! e uid v) (vector-set! (eng-sim-again e) uid v))
(define (eng-edge-executable? e src dest) (hash-ref (eng-exec e) (cons src dest) #f))
(define (eng-block-visited? e bb) (hash-ref (eng-visited e) bb #f))
(define (eng-uids-of-block e bb) (hash-ref (eng-block-uids e) bb (cons '() '())))

;; ssa_prop_init
(define (make-engine cfg dom back?)
  (define order->bb
    (for/vector ([b (in-pvector (dom-rpo dom))] #:unless (or (= b ENTRY) (= b EXIT))) b))
  (define bb->order (for/hasheqv ([b (in-vector order->bb)] [i (in-naturals)]) (values b i)))
  (define stmts '()) (define bbs '()) (define phis? '()) (define n 0)
  (define block-uids (make-hasheqv))
  (define def->uid (make-hasheqv))
  (define uses (make-hasheqv))
  (define (add-use! name uid pred)
    (hash-update! uses (ssa-name-version name) (lambda (l) (cons (cons uid pred) l)) '()))
  (for ([b (in-vector order->bb)])
    (define blk (cfg-block cfg b))
    (define phi-uids
      (for/list ([ph (in-pvector (GBlock-phis blk))])
        (define uid n)
        (set! n (add1 n))
        (set! stmts (cons ph stmts)) (set! bbs (cons b bbs)) (set! phis? (cons #t phis?))
        (when (gphi-result ph) (hash-set! def->uid (ssa-name-version (gphi-result ph)) uid))
        (for ([a (in-list (phi-ssa-args ph))]) (add-use! (cdr a) uid (car a)))
        uid))
    (define stmt-uids
      (for/list ([g (in-pvector (GBlock-stmts blk))])
        (define uid n)
        (set! n (add1 n))
        (set! stmts (cons g stmts)) (set! bbs (cons b bbs)) (set! phis? (cons #f phis?))
        (for ([d (in-list (stmt-all-defs g))]) (hash-set! def->uid (ssa-name-version d) uid))
        (for ([u (in-list (stmt-real-uses g))]) (add-use! u uid #f))
        uid))
    (hash-set! block-uids b (cons phi-uids stmt-uids)))
  (eng cfg dom order->bb bb->order
       (list->vector (reverse stmts)) (list->vector (reverse bbs)) (list->vector (reverse phis?))
       block-uids def->uid (for/hasheqv ([(k v) (in-hash uses)]) (values k (reverse v)))
       (make-vector n #t) (make-hash) (make-hasheqv)
       (box intbits-empty) (box intbits-empty) back? (box intbits-empty) (box intbits-empty) (box 0)))

(define (succ-dests cfg b) (for/list ([e (in-pvector (cfg-succs cfg b))]) (GEdge-dest e)))

;; add_control_edge
(define (add-control-edge! e src dest)
  (unless (or (= dest EXIT) (hash-ref (eng-exec e) (cons src dest) #f))
    (hash-set! (eng-exec e) (cons src dest) #t)
    (define order (hash-ref (eng-bb->order e) dest))
    (define bx (if (and (eng-back? e) (< order (unbox (eng-curr-order e)))) (eng-cfg-blocks-back e) (eng-cfg-blocks e)))
    (set-box! bx (intbits-set (unbox bx) order))))

;; add_ssa_edge
(define (add-ssa-edge! e name)
  (for ([u (in-list (hash-ref (eng-uses e) (ssa-name-version name) '()))])
    (define uid (car u)) (define pred (cdr u))
    (when (and (eng-simulate-again? e uid)
               (eng-block-visited? e (eng-uid-bb e uid))
               (or (not pred) (eng-edge-executable? e pred (eng-uid-bb e uid))))
      (define bx (if (and (eng-back? e) (< (hash-ref (eng-bb->order e) (eng-uid-bb e uid)) (unbox (eng-curr-order e))))
                     (eng-ssa-wl-back e)
                     (eng-ssa-wl e)))
      (set-box! bx (intbits-set (unbox bx) uid)))))

;; simulate_stmt
(define (simulate-stmt! e uid visit-stmt visit-phi)
  (define bx (eng-ssa-wl e))
  (set-box! bx (intbits-clear (unbox bx) uid))
  (when (eng-simulate-again? e uid)
    (define g (eng-stmt e uid))
    (define bb (eng-uid-bb e uid))
    (define phi? (eng-phi-uid? e uid))
    (define-values (val taken output)
      (if phi?
          (let-values ([(r out) (visit-phi e uid g)]) (values r #f out))
          (visit-stmt e uid g)))
    (cond
      [(eq? val 'varying)
       (eng-set-simulate-again! e uid #f)
       (when output (add-ssa-edge! e output))
       (when (stmt-ends-bb? g)
         (for ([d (in-list (succ-dests (eng-cfg e) bb))]) (add-control-edge! e bb d)))]
      [else
       (when (eq? val 'interesting)
         (when output (add-ssa-edge! e output))
         (when taken (add-control-edge! e bb taken)))
       ;; no use whose definition is simulated again: never visited again
       (define (sim-def? arg)
         (and (ssa-name? arg) (not (ssa-default-def? arg))
              (let ([d (eng-def-uid e arg)]) (and d (eng-simulate-again? e d)))))
       (define has-simulate-again-uses
         (cond
           [phi?
            (for/or ([pred (in-pvector (cfg-preds (eng-cfg e) bb))])
              (or (not (eng-edge-executable? e pred bb))
                  (sim-def? (phi-arg-for-pred g pred))))]
           [else (for/or ([u (in-list (stmt-real-uses g))]) (sim-def? u))]))
       (unless has-simulate-again-uses (eng-set-simulate-again! e uid #f))])))

;; simulate_block
(define (simulate-block! e bb visit-stmt visit-phi)
  (unless (= bb EXIT)
    (define uids (eng-uids-of-block e bb))
    (for ([uid (in-list (car uids))]) (simulate-stmt! e uid visit-stmt visit-phi))
    (unless (eng-block-visited? e bb)
      (for ([uid (in-list (cdr uids))]) (simulate-stmt! e uid visit-stmt visit-phi))
      (hash-set! (eng-visited e) bb #t)
      ;; a single normal successor is executable (abnormal/EH edges: none here)
      (define dests (succ-dests (eng-cfg e) bb))
      (when (= 1 (length dests)) (add-control-edge! e bb (car dests))))))

;; ssa_propagate: returns the engine
(define (ssa-propagate cfg dom #:init init! #:visit-stmt visit-stmt #:visit-phi visit-phi
                       #:backedge-worklists? [back? #f])
  (define e (make-engine cfg dom back?))
  (init! e)
  (for ([d (in-list (succ-dests cfg ENTRY))])
    (hash-remove! (eng-exec e) (cons ENTRY d))
    (add-control-edge! e ENTRY d))
  (let loop ()
    (define blocks (unbox (eng-cfg-blocks e)))
    (define wl (unbox (eng-ssa-wl e)))
    (define next-block-order (if (intbits-empty? blocks) -1 (intbits-first blocks)))
    (define next-uid (if (intbits-empty? wl) -1 (intbits-first wl)))
    (cond
      [(and (= next-block-order -1) (= next-uid -1))
       ;; GCC <= 12: the regular worklists are empty, continue with the back-edge ones
       (when (and back? (not (and (intbits-empty? (unbox (eng-cfg-blocks-back e))) (intbits-empty? (unbox (eng-ssa-wl-back e))))))
         (set-box! (eng-cfg-blocks e) (unbox (eng-cfg-blocks-back e)))
         (set-box! (eng-ssa-wl e) (unbox (eng-ssa-wl-back e)))
         (set-box! (eng-cfg-blocks-back e) intbits-empty)
         (set-box! (eng-ssa-wl-back e) intbits-empty)
         (loop))]
      [else
       (define next-stmt-bb-order
         (if (= next-uid -1) -1 (hash-ref (eng-bb->order e) (eng-uid-bb e next-uid))))
       (cond
         [(and (not (= next-block-order -1))
               (or (= next-stmt-bb-order -1) (<= next-block-order next-stmt-bb-order)))
          (set-box! (eng-curr-order e) next-block-order)
          (set-box! (eng-cfg-blocks e) (intbits-clear blocks next-block-order))
          (simulate-block! e (vector-ref (eng-order->bb e) next-block-order) visit-stmt visit-phi)]
         [else
          (set-box! (eng-curr-order e) next-stmt-bb-order)
          (simulate-stmt! e next-uid visit-stmt visit-phi)])
       (loop)]))
  e)
