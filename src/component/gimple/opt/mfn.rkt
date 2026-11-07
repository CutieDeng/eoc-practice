#lang racket/base

;; ============================================================
;; opt: a mutable working copy of one function in SSA form
;; ============================================================
;;
;; GCC's optimizers walk a function with gimple_stmt_iterators,
;; immediate-use lists and SSA_NAME_DEF_STMT, mutating in place.  The
;; persistent GCfg is the inter-pass format; inside a pass that needs
;; this style, `gcfg->mfn` makes a working copy with
;;
;;   - one integer id per statement cell (PHIs included), block chains
;;     as doubly linked cells (O(1) insert/remove while iterating);
;;   - defs: SSA version -> id (SSA_NAME_DEF_STMT); a default
;;     definition has no entry;
;;   - uses: SSA version -> (id -> count), real and virtual operands
;;     (immediate uses with multiplicity, so has_single_use is exact);
;;
;; and `mfn->gcfg` writes the statements back into the GCfg skeleton.
;; Nothing mutable escapes a pass; the CFG structure (blocks, edges)
;; is not changed through an mfn.
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../gimple.rkt"
         "../cfg.rkt"
         "../ssa.rkt"
         "ssa-ops.rkt")

(provide gcfg->mfn mfn->gcfg mfn-cfg
         mfn-stmt mfn-bb mfn-phi? mfn-phis mfn-stmts mfn-first mfn-last mfn-next mfn-prev
         mfn-def mfn-def-of-version mfn-def-stmt mfn-use-ids mfn-imm-uses mfn-num-uses mfn-single-use? mfn-zero-uses? mfn-released?
         mfn-replace! mfn-relink! mfn-remove! mfn-insert-before! mfn-insert-after! mfn-release-name! mfn-unlink-vdef!
         stmt-use-names stmt-def-names)

(struct cell (stmt prev next bb) #:mutable)
(struct mfn (cfg cells [next-id #:mutable] heads tails phi-lists defs uses released [next-stamp #:mutable]))

;; SSA names used / defined by a statement or PHI, virtual operands included
(define (stmt-use-names g)
  (cond
    [(gphi? g) (map cdr (phi-ssa-args g))]
    [else (append (stmt-real-uses g) (let ([v (stmt-vuse g)]) (if v (list v) '())))]))
(define (stmt-def-names g)
  (cond
    [(gphi? g) (if (gphi-result g) (list (gphi-result g)) '())]
    [else (stmt-all-defs g)]))

(define (ver t) (ssa-name-version t))

(define (add-tables! m id g)
  (for ([d (in-list (stmt-def-names g))]) (hash-set! (mfn-defs m) (ver d) id))
  (for ([u (in-list (stmt-use-names g))])
    (define h (hash-ref! (mfn-uses m) (ver u) make-hasheqv))
    (hash-update! h id add1 0)))

(define (remove-tables! m id g)
  (for ([d (in-list (stmt-def-names g))])
    (when (eqv? (hash-ref (mfn-defs m) (ver d) #f) id) (hash-remove! (mfn-defs m) (ver d))))
  (for ([u (in-list (stmt-use-names g))])
    (define h (hash-ref (mfn-uses m) (ver u) #f))
    (when h
      (define n (sub1 (hash-ref h id 0)))
      (if (<= n 0) (hash-remove! h id) (hash-set! h id n)))))

(define (fresh-id! m)
  (define id (mfn-next-id m))
  (set-mfn-next-id! m (add1 id))
  id)

(define (gcfg->mfn cfg)
  (define m (mfn cfg (make-hasheqv) 0 (make-hasheqv) (make-hasheqv) (make-hasheqv) (make-hasheqv) (make-hasheqv) (make-hasheqv)
                 (add1 (for*/fold ([mx 0]) ([bb (in-pvector (cfg-bbs cfg))]
                                           [g (in-sequences (in-pvector (GBlock-phis (cfg-block cfg bb))) (in-pvector (GBlock-stmts (cfg-block cfg bb))))])
                         (max mx (stmt-max-stamp g))))))
  (for ([bb (in-pvector (cfg-bbs cfg))])
    (define b (cfg-block cfg bb))
    (hash-set! (mfn-phi-lists m) bb
               (for/list ([ph (in-pvector (GBlock-phis b))])
                 (define id (fresh-id! m))
                 (hash-set! (mfn-cells m) id (cell ph #f #f bb))
                 (add-tables! m id ph)
                 id))
    (hash-set! (mfn-heads m) bb #f)
    (hash-set! (mfn-tails m) bb #f)
    (for ([g (in-pvector (GBlock-stmts b))])
      (define id (fresh-id! m))
      (define prev (hash-ref (mfn-tails m) bb))
      (hash-set! (mfn-cells m) id (cell g prev #f bb))
      (if prev (set-cell-next! (hash-ref (mfn-cells m) prev) id) (hash-set! (mfn-heads m) bb id))
      (hash-set! (mfn-tails m) bb id)
      (add-tables! m id g)))
  m)

(define (mfn->gcfg m)
  (define cfg (mfn-cfg m))
  (for/fold ([c cfg]) ([bb (in-pvector (cfg-bbs cfg))])
    (define b (cfg-block c bb))
    (set-block c (block-set-stmts (block-set-phis b (for/pvector ([id (in-list (mfn-phis m bb))]) (mfn-stmt m id)))
                                  (for/pvector ([id (in-list (mfn-stmts m bb))]) (mfn-stmt m id))))))

;; ---- queries ----

(define (cell-of m id) (hash-ref (mfn-cells m) id))
(define (mfn-stmt m id) (cell-stmt (cell-of m id)))
(define (mfn-bb m id) (cell-bb (cell-of m id)))
(define (mfn-phi? m id) (gphi? (mfn-stmt m id)))
(define (mfn-phis m bb) (hash-ref (mfn-phi-lists m) bb '()))
(define (mfn-first m bb) (hash-ref (mfn-heads m) bb #f))
(define (mfn-last m bb) (hash-ref (mfn-tails m) bb #f))
(define (mfn-next m id) (cell-next (cell-of m id)))
(define (mfn-prev m id) (cell-prev (cell-of m id)))
(define (mfn-stmts m bb)
  (let loop ([id (mfn-first m bb)] [acc '()])
    (if id (loop (mfn-next m id) (cons id acc)) (reverse acc))))

(define (mfn-def m name) (hash-ref (mfn-defs m) (ver name) #f))
(define (mfn-def-of-version m v) (hash-ref (mfn-defs m) v #f))
(define (mfn-def-stmt m name) (let ([id (mfn-def m name)]) (and id (mfn-stmt m id))))
(define (mfn-use-ids m name) (hash-keys (hash-ref (mfn-uses m) (ver name) (hasheqv))))
;; FOR_EACH_IMM_USE_STMT order: the most recently (re)linked use first
(define (use-stamp m id name)
  (define g (mfn-stmt m id))
  (cond
    [(gphi? g)
     (define st (or (phi-arg-stamps g) (pvector-empty)))
     (for/fold ([mx -1]) ([a (in-pvector (gphi-args g))] [i (in-naturals)])
       (define x (and (< i (pvector-length st)) (pvector-ref st i)))
       (if (and x (ssa-name? (cdr a)) (ssa-name=? (cdr a) name) (> x mx)) x mx))]
    [else (or (stmt-stamp g) -1)]))
(define (mfn-imm-uses m name)
  (sort (mfn-use-ids m name) > #:key (lambda (id) (use-stamp m id name))))
(define (mfn-num-uses m name) (for/sum ([(id n) (in-hash (hash-ref (mfn-uses m) (ver name) (hasheqv)))]) n))
(define (mfn-single-use? m name) (= 1 (mfn-num-uses m name)))
(define (mfn-zero-uses? m name) (= 0 (mfn-num-uses m name)))
(define (mfn-released? m name) (hash-ref (mfn-released m) (ver name) #f))

;; ---- mutation ----

(define (mfn-replace! m id g0)
  (define c (cell-of m id))
  (define-values (g n) (restamp (cell-stmt c) g0 (mfn-next-stamp m)))
  (set-mfn-next-stamp! m n)
  (remove-tables! m id (cell-stmt c))
  (set-cell-stmt! c g)
  (add-tables! m id g))

;; gsi_remove + release_defs / remove_phi_node (release)
(define (mfn-remove! m id)
  (define c (cell-of m id))
  (define g (cell-stmt c))
  (remove-tables! m id g)
  (for ([d (in-list (stmt-def-names g))]) (hash-set! (mfn-released m) (ver d) #t))
  (cond
    [(gphi? g) (hash-set! (mfn-phi-lists m) (cell-bb c) (remq id (hash-ref (mfn-phi-lists m) (cell-bb c))))]
    [else
     (define bb (cell-bb c))
     (if (cell-prev c) (set-cell-next! (cell-of m (cell-prev c)) (cell-next c)) (hash-set! (mfn-heads m) bb (cell-next c)))
     (if (cell-next c) (set-cell-prev! (cell-of m (cell-next c)) (cell-prev c)) (hash-set! (mfn-tails m) bb (cell-prev c)))])
  (hash-remove! (mfn-cells m) id))

(define (mfn-release-name! m name) (hash-set! (mfn-released m) (ver name) #t))

(define (link-new! m g0 bb prev next)
  (define id (fresh-id! m))
  (define-values (g n) (restamp #f g0 (mfn-next-stamp m)))
  (set-mfn-next-stamp! m n)
  (hash-set! (mfn-cells m) id (cell g prev next bb))
  (if prev (set-cell-next! (cell-of m prev) id) (hash-set! (mfn-heads m) bb id))
  (if next (set-cell-prev! (cell-of m next) id) (hash-set! (mfn-tails m) bb id))
  (add-tables! m id g)
  id)


;; update_stmt without an operand change still relinks the uses
(define (mfn-relink! m id)
  (define c (cell-of m id))
  (define g (cell-stmt c))
  (unless (gphi? g)
    (set-cell-stmt! c (stmt-set-flag g 'stamp (mfn-next-stamp m)))
    (set-mfn-next-stamp! m (add1 (mfn-next-stamp m)))))

(define (mfn-insert-before! m id g)
  (define c (cell-of m id))
  (link-new! m g (cell-bb c) (cell-prev c) id))

(define (mfn-insert-after! m id g)
  (define c (cell-of m id))
  (link-new! m g (cell-bb c) id (cell-next c)))

;; unlink_stmt_vdef: the uses of the statement's vdef become uses of its vuse
(define (mfn-unlink-vdef! m id)
  (define g (mfn-stmt m id))
  (define vdef (and (not (gphi? g)) (stmt-vdef g)))
  (define vuse (and (not (gphi? g)) (stmt-vuse g)))
  (when (and vdef vuse)
    (define (f t) (if (and (ssa-name? t) (ssa-name=? t vdef)) vuse t))
    (for ([uid (in-list (mfn-use-ids m vdef))])
      (define ug (mfn-stmt m uid))
      (mfn-replace! m uid
                    (cond
                      [(gphi? ug) (phi-set-args ug (for/pvector ([a (in-pvector (gphi-args ug))]) (cons (car a) (f (cdr a)))))]
                      [(stmt-vuse ug) (stmt-set-flag ug 'vuse (f (stmt-vuse ug)))]
                      [else ug])))))
