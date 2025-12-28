#lang racket/base

;; ============================================================
;; Dominance Analysis
;; ============================================================
;;
;; Computes dominance relationships in a CFG.
;;
;; A block D dominates block N if every path from entry to N
;; must pass through D.
;;
;; Provides multiple algorithms:
;;   1. Simple iterative (Cooper, Harvey, Kennedy)
;;   2. Lengauer-Tarjan (efficient for large CFGs)
;;   3. Semi-NCA (simple yet fast)
;;
;; References:
;;   - Cooper et al. "A Simple, Fast Dominance Algorithm"
;;   - Lengauer & Tarjan "A Fast Algorithm for Finding Dominators"
;;   - Georgiadis et al. "Finding Dominators in Practice"
;; ============================================================

(require racket/match racket/list racket/set racket/hash)
(require "../framework.rkt")
(require "../../ir/cfg/main.rkt")
(require "../../lib/main.rkt")

;; ============================================================
;; Dominance Result Structure
;; ============================================================

;; Complete dominance information
(struct DominanceInfo (
  dominators      ; Hash[BlockId -> Set[BlockId]] - all dominators
  idom            ; Hash[BlockId -> BlockId] - immediate dominator
  dom-tree        ; Hash[BlockId -> (Listof BlockId)] - dominator tree children
  dom-frontier    ; Hash[BlockId -> Set[BlockId]] - dominance frontier
  post-order      ; (Listof BlockId) - reverse post-order
) #:transparent)

(provide (struct-out DominanceInfo))

;; ============================================================
;; Algorithm 1: Simple Iterative (Cooper-Harvey-Kennedy)
;; ============================================================
;;
;; Simple but effective for most CFGs.
;; Time: O(n²) in worst case, O(n × depth) typical
;; Space: O(n)

(define (compute-dominance-simple cfg ctx)
  (define entry (cfg-get-entry cfg))
  (define block-ids (cfg-all-block-ids cfg))

  ;; Build predecessor map
  (define pred-map (build-predecessor-map cfg))

  ;; Compute reverse post-order
  (define rpo (compute-reverse-post-order cfg entry))
  (define rpo-index (for/hash ([bid rpo] [i (in-naturals)])
                      (values bid i)))

  ;; Initialize: entry dominates itself, others have all blocks
  (define dom (make-hash))
  (hash-set! dom entry entry)  ; entry's idom is itself

  ;; Iterative dataflow
  (define changed? #t)
  (define iterations 0)

  (let loop ()
    (when changed?
      (set! changed? #f)
      (set! iterations (+ iterations 1))

      (for ([bid rpo] #:unless (equal? bid entry))
        (define preds (hash-ref pred-map bid '()))

        ;; Find first processed predecessor
        (define new-idom
          (for/fold ([idom #f])
                    ([p preds]
                     #:when (hash-has-key? dom p))
            (if idom
                (intersect-idom dom idom p rpo-index)
                p)))

        (when (and new-idom
                   (not (equal? new-idom (hash-ref dom bid #f))))
          (hash-set! dom bid new-idom)
          (set! changed? #t)))

      (loop)))

  ;; Build complete dominance info
  (define dom-tree (build-dom-tree dom entry))
  (define dom-frontier (compute-dom-frontier cfg dom pred-map))
  (define dominators (compute-all-dominators dom entry))

  (AnalysisResult
   (DominanceInfo dominators dom dom-tree dom-frontier rpo)
   'simple-iterative
   (hash 'iterations iterations)
   #t))

;; Find intersection of two idoms in the dominator tree
(define (intersect-idom dom b1 b2 rpo-index)
  (let loop ([f1 b1] [f2 b2])
    (cond
      [(equal? f1 f2) f1]
      [(< (hash-ref rpo-index f1 +inf.0)
          (hash-ref rpo-index f2 +inf.0))
       (loop f1 (hash-ref dom f2 f2))]
      [else
       (loop (hash-ref dom f1 f1) f2)])))

;; ============================================================
;; Algorithm 2: Lengauer-Tarjan
;; ============================================================
;;
;; More complex but faster for large CFGs.
;; Time: O(n × α(n)) nearly linear
;; Space: O(n)

(define (compute-dominance-lengauer-tarjan cfg ctx)
  (define entry (cfg-get-entry cfg))
  (define block-ids (cfg-all-block-ids cfg))
  (define n (length block-ids))

  ;; DFS numbering
  (define dfs-num (make-hash))
  (define vertex (make-vector n #f))  ; vertex[i] = block with dfs number i
  (define parent (make-hash))
  (define semi (make-hash))
  (define idom (make-hash))
  (define ancestor (make-hash))
  (define best (make-hash))
  (define bucket (make-hash))

  ;; Build predecessor and successor maps
  (define pred-map (build-predecessor-map cfg))
  (define succ-map (build-successor-map cfg))

  ;; Step 1: DFS numbering
  (define counter 0)
  (define (dfs v p)
    (hash-set! dfs-num v counter)
    (vector-set! vertex counter v)
    (hash-set! parent v p)
    (hash-set! semi v counter)
    (hash-set! ancestor v #f)
    (hash-set! best v v)
    (hash-set! bucket v '())
    (set! counter (+ counter 1))

    (for ([w (hash-ref succ-map v '())])
      (unless (hash-has-key? dfs-num w)
        (dfs w v))))

  (dfs entry #f)

  ;; Link-Eval with path compression
  (define (compress v)
    (define a (hash-ref ancestor v))
    (when (and a (hash-ref ancestor a #f))
      (compress a)
      (when (< (hash-ref semi (hash-ref best a))
               (hash-ref semi (hash-ref best v)))
        (hash-set! best v (hash-ref best a)))
      (hash-set! ancestor v (hash-ref ancestor a))))

  (define (eval v)
    (if (hash-ref ancestor v #f)
        (begin
          (compress v)
          (hash-ref best v))
        v))

  (define (link v w)
    (hash-set! ancestor w v))

  ;; Steps 2-3: Compute semi-dominators
  (for ([i (in-range (- counter 1) 0 -1)])
    (define w (vector-ref vertex i))

    ;; Step 2: Compute semi-dominator
    (for ([v (hash-ref pred-map w '())])
      (when (hash-has-key? dfs-num v)
        (define u (eval v))
        (when (< (hash-ref semi u) (hash-ref semi w))
          (hash-set! semi w (hash-ref semi u)))))

    ;; Add w to bucket of semi(w)
    (define s (vector-ref vertex (hash-ref semi w)))
    (hash-set! bucket s (cons w (hash-ref bucket s '())))

    ;; Link w to parent
    (define p (hash-ref parent w))
    (when p (link p w))

    ;; Step 3: Compute idom from bucket
    (when p
      (for ([v (hash-ref bucket p '())])
        (define u (eval v))
        (hash-set! idom v
                   (if (< (hash-ref semi u) (hash-ref semi v))
                       u
                       p)))
      (hash-set! bucket p '())))

  ;; Step 4: Finalize idom
  (for ([i (in-range 1 counter)])
    (define w (vector-ref vertex i))
    (unless (equal? (hash-ref idom w #f)
                    (vector-ref vertex (hash-ref semi w)))
      (hash-set! idom w (hash-ref idom (hash-ref idom w)))))

  ;; Entry dominates itself
  (hash-set! idom entry entry)

  ;; Build complete dominance info
  (define rpo (for/list ([i (in-range counter)])
                (vector-ref vertex i)))
  (define dom-tree (build-dom-tree idom entry))
  (define dom-frontier (compute-dom-frontier cfg idom pred-map))
  (define dominators (compute-all-dominators idom entry))

  (AnalysisResult
   (DominanceInfo dominators idom dom-tree dom-frontier rpo)
   'lengauer-tarjan
   (hash 'dfs-nodes counter)
   #t))

;; ============================================================
;; Algorithm 3: Semi-NCA (Simple Nearly-Linear)
;; ============================================================
;;
;; Good balance between simplicity and efficiency.
;; Based on semidominators but simpler than L-T.
;; Time: O(n × log n) typical
;; Space: O(n)

(define (compute-dominance-semi-nca cfg ctx)
  ;; For now, delegate to simple algorithm
  ;; TODO: Implement Semi-NCA properly
  (compute-dominance-simple cfg ctx))

;; ============================================================
;; Helper Functions
;; ============================================================

;; Build predecessor map
(define (build-predecessor-map cfg)
  (define pred-map (make-hash))
  (for ([bid (cfg-all-block-ids cfg)])
    (hash-set! pred-map bid '()))

  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (for ([succ (terminator-successors (CfgBlock-terminator block))])
        (hash-set! pred-map succ
                   (cons bid (hash-ref pred-map succ '()))))))
  pred-map)

;; Build successor map
(define (build-successor-map cfg)
  (define succ-map (make-hash))
  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if block
        (hash-set! succ-map bid
                   (terminator-successors (CfgBlock-terminator block)))
        (hash-set! succ-map bid '())))
  succ-map)

;; Compute reverse post-order via DFS
(define (compute-reverse-post-order cfg entry)
  (define visited (mutable-set))
  (define post-order '())

  (define succ-map (build-successor-map cfg))

  (define (dfs bid)
    (unless (set-member? visited bid)
      (set-add! visited bid)
      (for ([succ (hash-ref succ-map bid '())])
        (dfs succ))
      (set! post-order (cons bid post-order))))

  (dfs entry)
  post-order)  ; This is already reverse post-order

;; Build dominator tree from idom map
(define (build-dom-tree idom entry)
  (define tree (make-hash))
  (for ([(bid parent) (in-hash idom)])
    (hash-set! tree bid '()))

  (for ([(bid parent) (in-hash idom)])
    (unless (equal? bid entry)
      (hash-set! tree parent
                 (cons bid (hash-ref tree parent '())))))
  tree)

;; Compute all dominators from idom
(define (compute-all-dominators idom entry)
  (define doms (make-hash))

  (for ([bid (hash-keys idom)])
    (define dom-set (mutable-set bid))
    (let loop ([current bid])
      (define parent (hash-ref idom current #f))
      (when (and parent (not (equal? parent current)))
        (set-add! dom-set parent)
        (loop parent)))
    (hash-set! doms bid (set->list dom-set)))

  doms)

;; Compute dominance frontier
(define (compute-dom-frontier cfg idom pred-map)
  (define df (make-hash))
  (for ([bid (cfg-all-block-ids cfg)])
    (hash-set! df bid '()))

  (for ([bid (cfg-all-block-ids cfg)])
    (define preds (hash-ref pred-map bid '()))
    (when (> (length preds) 1)
      (for ([p preds])
        (let loop ([runner p])
          (when (and runner
                     (not (equal? runner (hash-ref idom bid #f))))
            (hash-set! df runner
                       (set->list
                        (set-add (list->set (hash-ref df runner '())) bid)))
            (loop (hash-ref idom runner #f)))))))

  df)

;; ============================================================
;; Analysis Registration
;; ============================================================

(define dominance-analysis
  (Analysis
   'dominance
   "Compute dominance relationships in CFG"
   (hash
    'simple compute-dominance-simple
    'lengauer-tarjan compute-dominance-lengauer-tarjan
    'semi-nca compute-dominance-semi-nca)
   'simple  ; Default algorithm
   '()      ; No dependencies
   '(cfg-structure)))  ; Invalidated by CFG changes

(register-analysis! dominance-analysis)

(provide dominance-analysis)

;; ============================================================
;; Convenience Functions
;; ============================================================

;; Compute dominance with default algorithm
(define (compute-dominance cfg #:algorithm [algo #f])
  (define ctx (make-analysis-context))
  (define result (run-analysis 'dominance cfg ctx #:algorithm algo))
  (AnalysisResult-data result))

;; Check if block a dominates block b
(define (dominates? dom-info a b)
  (if (member a (hash-ref (DominanceInfo-dominators dom-info) b '()))
      #t
      #f))

;; Get immediate dominator
(define (get-idom dom-info bid)
  (hash-ref (DominanceInfo-idom dom-info) bid #f))

;; Get dominator tree children
(define (get-dom-children dom-info bid)
  (hash-ref (DominanceInfo-dom-tree dom-info) bid '()))

;; Get dominance frontier
(define (get-dom-frontier dom-info bid)
  (hash-ref (DominanceInfo-dom-frontier dom-info) bid '()))

(provide compute-dominance dominates?
         get-idom get-dom-children get-dom-frontier)
