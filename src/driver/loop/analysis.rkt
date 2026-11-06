#lang racket/base

;; ============================================================
;; Driver: Loop Analysis
;; ============================================================
;;
;; Parameterized loop detection and analysis algorithms.
;; These do NOT depend on any specific graph representation.
;;
;; Data structures:
;;   - sets of nodes (loop bodies, headers): immutable hash node -> #t
;;     (prefab-serializable, unlike an opaque map)
;;   - node -> X maps: immutable hash
;;   - ordered collections: pvector, ordered by position in `nodes`
;;
;; ============================================================

(require "../../kernel/data/data.rkt")
(require "../dominance/dom.rkt")

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
;; Set operations (immutable hash as set)
;; ============================================================

(define (set-singleton x) (hash x #t))
(define (set-contains? s x) (hash-has-key? s x))
(define (set-add s x) (hash-set s x #t))
(define (set-count s) (hash-count s))

;; ============================================================
;; Loop Info Structure
;; ============================================================

(struct LoopInfo
  (header      ; node - loop header
   body        ; hash (as set) - all nodes in loop
   exits       ; pvector of nodes - exit nodes (have successors outside)
   back-edges  ; pvector of (cons node node) - (tail . header) pairs
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
;;   nodes            : pvector of nodes
;;   entry            : node
;;   get-successors   : node -> pvector of nodes
;;   get-predecessors : node -> pvector of nodes
;; Returns: pvector of (cons node node) - (tail . header) pairs
;;
(define (find-back-edges nodes entry get-successors get-predecessors)
  (define d (compute-dom nodes entry get-successors))
  (for*/fold ([result (pvector-empty)])
             ([node (in-pvector nodes)]
              [succ (in-pvector (get-successors node))])
    (define i (dom-index d node))
    (define j (dom-index d succ))
    (if (and i j (dom-dominates? d j i))
        (pvector-cons-right result (cons node succ))
        result)))

;; ============================================================
;; Natural Loop Detection
;; ============================================================

;; Find all natural loops
;; Parameters:
;;   nodes            : pvector of nodes
;;   entry            : node
;;   get-successors   : node -> pvector of nodes
;;   get-predecessors : node -> pvector of nodes
;; Returns: pvector of LoopInfo, one per header, headers in `nodes` order
;;
(define (find-natural-loops nodes entry get-successors get-predecessors)
  (define back-edges
    (find-back-edges nodes entry get-successors get-predecessors))

  ;; Group back edges by header
  (define header->back-edges
    (for/fold ([m (hash)])
              ([edge (in-pvector back-edges)])
      (define header (cdr edge))
      (hash-set m header
                (pvector-cons-right (hash-ref m header (pvector-empty)) edge))))

  ;; For each header (in `nodes` order), compute the loop body
  (for/fold ([result (pvector-empty)])
            ([header (in-pvector nodes)]
             #:when (hash-has-key? header->back-edges header))
    (define edges (hash-ref header->back-edges header))
    (define tails
      (for/pvector ([e (in-pvector edges)])
        (car e)))

    ;; Find all nodes in the loop using backward traversal
    (define (add-to-loop node current-body)
      (if (set-contains? current-body node)
          current-body
          (let ([new-body (set-add current-body node)])
            (for/fold ([b new-body])
                      ([pred (in-pvector (get-predecessors node))])
              (add-to-loop pred b)))))

    (define final-body
      (for/fold ([body (set-singleton header)])
                ([tail (in-pvector tails)])
        (add-to-loop tail body)))

    ;; Find exit nodes (in `nodes` order)
    (define exits
      (for/fold ([ex (pvector-empty)])
                ([node (in-pvector nodes)]
                 #:when (set-contains? final-body node))
        (if (for/or ([succ (in-pvector (get-successors node))])
              (not (set-contains? final-body succ)))
            (pvector-cons-right ex node)
            ex)))

    (pvector-cons-right result
      (make-loop-info
        header
        final-body
        exits
        edges
        0))))  ; Depth computed later in build-loop-forest

;; Find all loop headers
;; Parameters:
;;   nodes            : pvector of nodes
;;   entry            : node
;;   get-successors   : node -> pvector of nodes
;;   get-predecessors : node -> pvector of nodes
;; Returns: hash (as set)
;;
(define (find-loop-headers nodes entry get-successors get-predecessors)
  (define back-edges
    (find-back-edges nodes entry get-successors get-predecessors))
  (for/fold ([s (hash)])
            ([edge (in-pvector back-edges)])
    (set-add s (cdr edge))))

;; ============================================================
;; Loop Nesting Forest
;; ============================================================

;; Build loop nesting forest with proper depth
;; Parameters:
;;   loops : pvector of LoopInfo - from find-natural-loops
;; Returns: (values
;;            updated-loops : pvector of LoopInfo with correct depths
;;            parent-map    : hash loop-header -> parent-loop-header or #f
;;            children-map  : hash loop-header -> pvector of loop-header)
;;
(define (build-loop-forest loops)
  ;; Initialize children map
  (define initial-children-map
    (for/fold ([m (hash)])
              ([loop (in-pvector loops)])
      (hash-set m (loop-info-header loop) (pvector-empty))))

  ;; Determine parent for each loop
  ;; Parent is the smallest loop that strictly contains this loop
  (define-values (parent-map children-map)
    (for/fold ([parents (hash)]
               [children initial-children-map])
              ([loop (in-pvector loops)])
      (define header (loop-info-header loop))
      ;; Find all loops that contain this loop's header (excluding self)
      (define candidates
        (for/fold ([cs (pvector-empty)])
                  ([other (in-pvector loops)])
          (if (and (not (equal? header (loop-info-header other)))
                   (set-contains? (loop-info-body other) header))
              (pvector-cons-right cs other)
              cs)))
      (if (pvector-empty? candidates)
          (values parents children)
          ;; Find the smallest containing loop
          (let ()
            (define parent
              (for/fold ([best #f] [best-size +inf.0])
                        ([c (in-pvector candidates)])
                (define c-size (set-count (loop-info-body c)))
                (if (< c-size best-size)
                    (values c c-size)
                    (values best best-size))))
            (define parent-header (loop-info-header parent))
            (values
              (hash-set parents header parent-header)
              (hash-set children parent-header
                        (pvector-cons-right
                          (hash-ref children parent-header (pvector-empty))
                          header)))))))

  ;; Compute depths
  (define (compute-depth header)
    (define parent (hash-ref parent-map header #f))
    (if parent
        (+ 1 (compute-depth parent))
        0))

  (define updated-loops
    (for/pvector ([loop (in-pvector loops)])
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
  (set-contains? (loop-info-body loop1)
                 (loop-info-header loop2)))

;; Get innermost loop containing a node
;; Parameters:
;;   node  : node
;;   loops : pvector of LoopInfo
;; Returns: LoopInfo or #f
;;
(define (get-innermost-loop node loops)
  (for/fold ([best #f] [best-depth -1])
            ([loop (in-pvector loops)])
    (if (and (set-contains? (loop-info-body loop) node)
             (> (loop-info-depth loop) best-depth))
        (values loop (loop-info-depth loop))
        (values best best-depth))))
