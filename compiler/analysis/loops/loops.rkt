#lang racket/base

;; ============================================================
;; Loop Analysis
;; ============================================================
;;
;; Identifies loops in a CFG using back edges and dominance.
;;
;; A natural loop is identified by a back edge (N -> H) where:
;;   - H dominates N (H is the loop header)
;;   - The loop body is the set of nodes from which N can be
;;     reached without going through H
;;
;; Provides multiple algorithms:
;;   1. Dominance-based (classic, using back edges)
;;   2. Tarjan's SCC (strongly connected components)
;;
;; References:
;;   - Aho et al. "Compilers: Principles, Techniques, and Tools"
;;   - Tarjan "Depth-First Search and Linear Graph Algorithms"
;; ============================================================

(require racket/match racket/list racket/set racket/hash)
(require "../framework.rkt")
(require "../dominance/main.rkt")
(require "../../ir/cfg/main.rkt")
(require "../../lib/main.rkt")

;; ============================================================
;; Loop Result Structure
;; ============================================================

;; A natural loop
(struct Loop (
  header        ; BlockId - loop header (dominates all loop blocks)
  back-edges    ; (Listof (Pairof BlockId BlockId)) - back edges (source . header)
  body          ; Set[BlockId] - all blocks in the loop
  exits         ; (Listof BlockId) - blocks with edges leaving the loop
  depth         ; Integer - nesting depth (1 = outermost)
  parent        ; Loop or #f - containing loop (for nested loops)
) #:transparent)

;; Complete loop information
(struct LoopInfo (
  loops         ; (Listof Loop) - all loops, sorted by nesting depth
  block->loop   ; Hash[BlockId -> Loop] - innermost loop containing block
  headers       ; Set[BlockId] - all loop headers
  back-edges    ; (Listof (Pairof BlockId BlockId)) - all back edges
) #:transparent)

(provide (struct-out Loop) (struct-out LoopInfo))

;; ============================================================
;; Algorithm 1: Dominance-Based Loop Detection
;; ============================================================
;;
;; Standard algorithm using dominance and back edges.
;; Time: O(n + e)
;; Space: O(n)

(define (compute-loops-dominance cfg ctx)
  ;; First compute dominance
  (define dom-result (run-analysis 'dominance cfg ctx))
  (define dom-info (AnalysisResult-data dom-result))

  ;; Build successor map
  (define succ-map (build-successor-map cfg))

  ;; Find all back edges: edge (N -> H) where H dominates N
  (define back-edges '())
  (for ([bid (cfg-all-block-ids cfg)])
    (for ([succ (hash-ref succ-map bid '())])
      (when (dominates? dom-info succ bid)
        (set! back-edges (cons (cons bid succ) back-edges)))))

  ;; Group back edges by header
  (define header->back-edges (make-hash))
  (for ([edge back-edges])
    (define header (cdr edge))
    (hash-set! header->back-edges header
               (cons edge (hash-ref header->back-edges header '()))))

  ;; Build predecessor map for finding loop body
  (define pred-map (build-predecessor-map cfg))

  ;; For each header, find the loop body
  (define loops '())
  (for ([(header edges) (in-hash header->back-edges)])
    (define body (find-loop-body header edges pred-map))
    (define exits (find-loop-exits body succ-map))
    (define loop (Loop header edges body exits 0 #f))
    (set! loops (cons loop loops)))

  ;; Compute nesting depth and parent relationships
  (define loops-with-depth (compute-loop-nesting loops))

  ;; Build block -> innermost loop mapping
  (define block->loop (make-hash))
  (for ([loop (sort loops-with-depth > #:key Loop-depth)])
    (for ([bid (in-set (Loop-body loop))])
      (hash-set! block->loop bid loop)))

  (AnalysisResult
   (LoopInfo
    loops-with-depth
    block->loop
    (list->set (map Loop-header loops-with-depth))
    back-edges)
   'dominance-based
   (hash 'loops (length loops-with-depth)
         'back-edges (length back-edges))
   #t))

;; Find the loop body given the header and back edges
(define (find-loop-body header back-edges pred-map)
  (define body (mutable-set header))

  ;; Start from back edge sources and walk back to header
  (for ([edge back-edges])
    (define source (car edge))
    (define worklist (list source))

    (let loop ()
      (unless (null? worklist)
        (define bid (car worklist))
        (set! worklist (cdr worklist))

        (unless (set-member? body bid)
          (set-add! body bid)
          ;; Add predecessors to worklist
          (for ([pred (hash-ref pred-map bid '())])
            (set! worklist (cons pred worklist))))

        (loop))))

  (list->set (set->list body)))

;; Find loop exit blocks (blocks with edges leaving the loop)
(define (find-loop-exits body succ-map)
  (for/list ([bid (in-set body)]
             #:when (for/or ([succ (hash-ref succ-map bid '())])
                      (not (set-member? body succ))))
    bid))

;; Compute loop nesting (depth and parent relationships)
(define (compute-loop-nesting loops)
  (cond
    [(null? loops) '()]
    [else
     ;; Sort by body size (larger loops contain smaller ones)
     (define sorted (sort loops > #:key (lambda (l) (set-count (Loop-body l)))))

     ;; Compute depth and parent for each loop
     (define result '())
     (for ([loop sorted])
       (define parent
         (for/first ([other result]
                     #:when (and (not (eq? other loop))
                                 (set-member? (Loop-body other)
                                              (Loop-header loop))))
           other))

       (define depth
         (if parent
             (+ 1 (Loop-depth parent))
             1))

       (set! result
             (cons (struct-copy Loop loop
                                [depth depth]
                                [parent parent])
                   result)))

     (reverse result)]))

;; ============================================================
;; Algorithm 2: Tarjan's SCC-Based Loop Detection
;; ============================================================
;;
;; Uses strongly connected components to find loops.
;; Can find irreducible loops that dominance-based misses.
;; Time: O(n + e)
;; Space: O(n)

(define (compute-loops-scc cfg ctx)
  (define block-ids (cfg-all-block-ids cfg))
  (define succ-map (build-successor-map cfg))

  ;; Tarjan's SCC algorithm
  (define index 0)
  (define indices (make-hash))
  (define lowlinks (make-hash))
  (define on-stack (make-hash))
  (define stack '())
  (define sccs '())

  (define (strongconnect v)
    (hash-set! indices v index)
    (hash-set! lowlinks v index)
    (set! index (+ index 1))
    (set! stack (cons v stack))
    (hash-set! on-stack v #t)

    (for ([w (hash-ref succ-map v '())])
      (cond
        [(not (hash-has-key? indices w))
         (strongconnect w)
         (hash-set! lowlinks v
                    (min (hash-ref lowlinks v)
                         (hash-ref lowlinks w)))]
        [(hash-ref on-stack w #f)
         (hash-set! lowlinks v
                    (min (hash-ref lowlinks v)
                         (hash-ref indices w)))]))

    ;; If v is a root node, pop the SCC
    (when (= (hash-ref lowlinks v) (hash-ref indices v))
      (define scc '())
      (let loop ()
        (define w (car stack))
        (set! stack (cdr stack))
        (hash-set! on-stack w #f)
        (set! scc (cons w scc))
        (unless (equal? w v)
          (loop)))
      (when (> (length scc) 1)  ; Only care about non-trivial SCCs
        (set! sccs (cons scc sccs)))))

  ;; Run on all blocks
  (for ([v block-ids])
    (unless (hash-has-key? indices v)
      (strongconnect v)))

  ;; Convert SCCs to loops
  ;; For each SCC, find the entry point (header)
  (define pred-map (build-predecessor-map cfg))
  (define loops
    (for/list ([scc sccs])
      (define scc-set (list->set scc))
      ;; Header is the block with a predecessor outside the SCC
      (define header
        (for/first ([bid scc]
                    #:when (for/or ([pred (hash-ref pred-map bid '())])
                             (not (set-member? scc-set pred))))
          bid))
      (define actual-header (or header (car scc)))  ; Fallback to first block

      ;; Find back edges within the SCC
      (define back-edges
        (for*/list ([bid scc]
                    [succ (hash-ref succ-map bid '())]
                    #:when (and (set-member? scc-set succ)
                                (equal? succ actual-header)))
          (cons bid succ)))

      (define exits (find-loop-exits scc-set succ-map))
      (Loop actual-header back-edges scc-set exits 0 #f)))

  ;; Compute nesting
  (define loops-with-depth (compute-loop-nesting loops))

  ;; Build block -> innermost loop mapping
  (define block->loop (make-hash))
  (for ([loop (sort loops-with-depth > #:key Loop-depth)])
    (for ([bid (in-set (Loop-body loop))])
      (hash-set! block->loop bid loop)))

  (AnalysisResult
   (LoopInfo
    loops-with-depth
    block->loop
    (list->set (map Loop-header loops-with-depth))
    (apply append (map Loop-back-edges loops-with-depth)))
   'scc-based
   (hash 'loops (length loops-with-depth)
         'sccs (length sccs))
   #t))

;; ============================================================
;; Helper Functions
;; ============================================================

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

;; ============================================================
;; Analysis Registration
;; ============================================================

(define loop-analysis
  (Analysis
   'loops
   "Identify loops in CFG"
   (hash
    'dominance compute-loops-dominance
    'scc compute-loops-scc)
   'dominance  ; Default algorithm
   '(dominance)  ; Depends on dominance
   '(cfg-structure)))  ; Invalidated by CFG changes

(register-analysis! loop-analysis)

(provide loop-analysis)

;; ============================================================
;; Convenience Functions
;; ============================================================

;; Compute loops with default algorithm
(define (compute-loops cfg #:algorithm [algo #f])
  (define ctx (make-analysis-context))
  (define result (run-analysis 'loops cfg ctx #:algorithm algo))
  (AnalysisResult-data result))

;; Check if block is in a loop
(define (in-loop? loop-info bid)
  (hash-has-key? (LoopInfo-block->loop loop-info) bid))

;; Get innermost loop containing block
(define (get-containing-loop loop-info bid)
  (hash-ref (LoopInfo-block->loop loop-info) bid #f))

;; Check if block is a loop header
(define (loop-header? loop-info bid)
  (set-member? (LoopInfo-headers loop-info) bid))

;; Get all loops at a given nesting depth
(define (get-loops-at-depth loop-info depth)
  (filter (lambda (l) (= (Loop-depth l) depth))
          (LoopInfo-loops loop-info)))

;; Get the loop depth of a block (0 if not in a loop)
(define (get-loop-depth loop-info bid)
  (define loop (hash-ref (LoopInfo-block->loop loop-info) bid #f))
  (if loop (Loop-depth loop) 0))

;; Get all nested loops within a loop
(define (get-nested-loops loop-info loop)
  (filter (lambda (l)
            (and (Loop-parent l)
                 (equal? (Loop-header (Loop-parent l))
                         (Loop-header loop))))
          (LoopInfo-loops loop-info)))

(provide compute-loops in-loop? get-containing-loop loop-header?
         get-loops-at-depth get-loop-depth get-nested-loops)
