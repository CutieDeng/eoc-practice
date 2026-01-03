#lang racket/base

;; ============================================================
;; Driver: Loop Analysis
;; ============================================================
;;
;; Parameterized loop detection and analysis algorithms.
;; These do NOT depend on any specific graph representation.
;;
;; ============================================================

(require racket/list)
(require racket/set)
(require "../../kernel/data/main.rkt")
(require "../graph/traversal.rkt")
(require "../dominance/dominator.rkt")

(provide
  ;; Loop detection
  find-natural-loops
  find-back-edges
  find-loop-headers

  ;; Loop structure
  make-loop-info
  loop-info-header
  loop-info-body
  loop-info-exits
  loop-info-back-edges
  loop-info-depth

  ;; Loop nesting
  build-loop-forest
  loop-contains?
  get-innermost-loop)

;; ============================================================
;; Loop Info Structure
;; ============================================================

(struct LoopInfo
  (header      ; node - loop header
   body        ; (setof node) - all nodes in loop
   exits       ; (listof node) - exit nodes (have successors outside)
   back-edges  ; (listof (cons node node)) - (tail . header) pairs
   depth)      ; integer - nesting depth (0 = outermost)
  #:prefab)

(define (make-loop-info header body exits back-edges depth)
  (LoopInfo header body exits back-edges depth))

(define (loop-info-header loop) (LoopInfo-header loop))
(define (loop-info-body loop) (LoopInfo-body loop))
(define (loop-info-exits loop) (LoopInfo-exits loop))
(define (loop-info-back-edges loop) (LoopInfo-back-edges loop))
(define (loop-info-depth loop) (LoopInfo-depth loop))

;; ============================================================
;; Back Edge Detection
;; ============================================================

;; Find all back edges in the graph
;; A back edge is an edge (n -> h) where h dominates n
;; Parameters:
;;   nodes            : (listof node)
;;   entry            : node
;;   get-successors   : node -> (listof node)
;;   get-predecessors : node -> (listof node)
;; Returns: (listof (cons node node)) - (tail . header) pairs
;;
(define (find-back-edges nodes entry get-successors get-predecessors)
  (define dom (compute-dominators nodes entry get-predecessors))

  (for*/list ([node nodes]
              [succ (get-successors node)]
              #:when (dominates? dom succ node))
    (cons node succ)))

;; ============================================================
;; Natural Loop Detection
;; ============================================================

;; Find all natural loops
;; Parameters:
;;   nodes            : (listof node)
;;   entry            : node
;;   get-successors   : node -> (listof node)
;;   get-predecessors : node -> (listof node)
;; Returns: (listof LoopInfo)
;;
(define (find-natural-loops nodes entry get-successors get-predecessors)
  (define back-edges
    (find-back-edges nodes entry get-successors get-predecessors))

  ;; Group back edges by header
  (define header->back-edges (make-hash))
  (for ([edge back-edges])
    (define header (cdr edge))
    (hash-set! header->back-edges header
      (cons edge (hash-ref header->back-edges header '()))))

  ;; For each header, compute the loop body
  (for/list ([header (hash-keys header->back-edges)])
    (define edges (hash-ref header->back-edges header))
    (define tails (map car edges))

    ;; Find all nodes in the loop using backward traversal
    (define body (mutable-set))
    (set-add! body header)

    (define (add-to-loop node)
      (unless (set-member? body node)
        (set-add! body node)
        (for ([pred (get-predecessors node)])
          (add-to-loop pred))))

    (for ([tail tails])
      (add-to-loop tail))

    ;; Find exit nodes
    (define exits
      (for/list ([node body]
                 #:when (for/or ([succ (get-successors node)])
                          (not (set-member? body succ))))
        node))

    (make-loop-info
      header
      (for/set ([n body]) n)
      exits
      edges
      0)))  ; Depth computed later in build-loop-forest

;; Find all loop headers
;; Parameters:
;;   nodes            : (listof node)
;;   entry            : node
;;   get-successors   : node -> (listof node)
;;   get-predecessors : node -> (listof node)
;; Returns: (setof node)
;;
(define (find-loop-headers nodes entry get-successors get-predecessors)
  (define back-edges
    (find-back-edges nodes entry get-successors get-predecessors))
  (for/set ([edge back-edges])
    (cdr edge)))

;; ============================================================
;; Loop Nesting Forest
;; ============================================================

;; Build loop nesting forest with proper depth
;; Parameters:
;;   loops : (listof LoopInfo) - from find-natural-loops
;; Returns: (values
;;            updated-loops   : (listof LoopInfo) with correct depths
;;            parent-map      : hash loop-header -> parent-loop-header or #f
;;            children-map    : hash loop-header -> (listof loop-header))
;;
(define (build-loop-forest loops)
  (define loop-by-header (make-hash))
  (for ([loop loops])
    (hash-set! loop-by-header (loop-info-header loop) loop))

  (define parent-map (make-hash))
  (define children-map (make-hash))

  ;; Initialize children map
  (for ([loop loops])
    (hash-set! children-map (loop-info-header loop) '()))

  ;; Determine parent for each loop
  ;; Parent is the smallest loop that strictly contains this loop
  (for ([loop loops])
    (define header (loop-info-header loop))
    (define body (loop-info-body loop))

    (define candidates
      (for/list ([other loops]
                 #:when (and (not (equal? header (loop-info-header other)))
                            (set-member? (loop-info-body other) header)))
        other))

    (define parent
      (if (null? candidates)
          #f
          (argmin (lambda (l) (set-count (loop-info-body l)))
                  candidates)))

    (when parent
      (define parent-header (loop-info-header parent))
      (hash-set! parent-map header parent-header)
      (hash-set! children-map parent-header
        (cons header (hash-ref children-map parent-header '())))))

  ;; Compute depths
  (define (compute-depth header)
    (define parent (hash-ref parent-map header #f))
    (if parent
        (+ 1 (compute-depth parent))
        0))

  (define updated-loops
    (for/list ([loop loops])
      (define header (loop-info-header loop))
      (make-loop-info
        header
        (loop-info-body loop)
        (loop-info-exits loop)
        (loop-info-back-edges loop)
        (compute-depth header))))

  (values updated-loops parent-map children-map))

;; ============================================================
;; Loop Utilities
;; ============================================================

;; Check if loop1 contains loop2
;; (loop1's body contains loop2's header)
;;
(define (loop-contains? loop1 loop2)
  (set-member? (loop-info-body loop1)
               (loop-info-header loop2)))

;; Get innermost loop containing a node
;; Parameters:
;;   node  : node
;;   loops : (listof LoopInfo)
;; Returns: LoopInfo or #f
;;
(define (get-innermost-loop node loops)
  (define containing
    (filter (lambda (loop)
              (set-member? (loop-info-body loop) node))
            loops))
  (if (null? containing)
      #f
      (argmax loop-info-depth containing)))
