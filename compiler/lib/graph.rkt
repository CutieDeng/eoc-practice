#lang racket/base

;; ============================================================
;; Foundation: Graph Data Structure
;; ============================================================
;;
;; Immutable directed graph implementation based on ordered-map.
;; Supports both directed and undirected edge operations.
;;
;; Note: This is a simple adjacency-list graph for the compiler.
;; For more advanced features (ID management, edge pairing, multi-edge),
;; see cutie-ftree/graph.rkt
;; ============================================================

(require racket/match racket/dict)
(require "ftree.rkt")

;; Graph structure: comparison function + outgoing edges + incoming edges
(struct Graph (cmp-fn out in))

;; === Constructors ===

(define (graph-make-empty)
  (Graph integer-compare
         (ordered-map-empty integer-compare)
         (ordered-map-empty integer-compare)))

(define (graph-make-empty-raw cmp-fn)
  (Graph cmp-fn
         (ordered-map-empty cmp-fn)
         (ordered-map-empty cmp-fn)))

(define (graph-make-empty-normal)
  (define graph (Graph integer-compare
                       (ordered-map-empty integer-compare)
                       (ordered-map-empty integer-compare)))
  (add-vertex (add-vertex graph 0) 1))

;; === Queries ===

(define (has-vertex? graph vertex)
  (if (ordered-map-query (Graph-out graph) vertex) #t #f))

(define (has-edge? graph u v)
  (define u-out (ordered-map-query (Graph-out graph) u))
  (cond
    [u-out
     (define u-out^ (cdr u-out))
     (if (ordered-map-query u-out^ v) #t #f)]
    [else #f]))

(define (vertex=? graph u v) (equal? u v))

;; === Vertex Operations ===

(define (add-vertex graph u)
  (match-define (Graph cmp-fn out in) graph)
  (cond
    [(ordered-map-query out u) graph]
    [else
     (define out^ (ordered-map-set out u (ordered-map-empty cmp-fn)))
     (define in^ (ordered-map-set in u (ordered-map-empty cmp-fn)))
     (Graph cmp-fn out^ in^)]))

(define (remove-vertex graph u)
  (match-define (Graph _ out in) graph)
  (define u-out (ordered-map-query out u))
  (define u-in (ordered-map-query in u))
  (cond
    [u-out
     (define graph^
       (for/fold ([g graph]) ([to (in-dict-keys (cdr u-out))])
         (remove-directed-edge g u to)))
     (define graph^^
       (for/fold ([g graph^]) ([from (in-dict-keys (cdr u-in))])
         (remove-directed-edge g from u)))
     graph^^]
    [else graph]))

;; === Edge Operations ===

(define (add-edge graph u v)
  (define graph^ (add-directed-edge graph u v))
  (add-directed-edge graph^ v u))

(define (add-directed-edge graph u v)
  (match-define (Graph cmp-fn out in) graph)
  (define u-out (ordered-map-query out u))
  (define v-in (ordered-map-query in v))
  (cond
    [(and u-out v-in)
     (define u-out-new (ordered-map-set (cdr u-out) v #f))
     (cond
       [(eq? (cdr u-out) u-out-new) graph]  ; already exists
       [else
        (define v-in-new (ordered-map-set (cdr v-in) u #f))
        (define out^ (ordered-map-set out u u-out-new))
        (define in^ (ordered-map-set in v v-in-new))
        (Graph cmp-fn out^ in^)])]
    [else graph]))

(define (remove-directed-edge graph u v)
  (match-define (Graph cmp-fn out in) graph)
  (define u-out (ordered-map-query out u))
  (define v-in (ordered-map-query in v))
  (cond
    [(and u-out v-in)
     (define-values (u-out-new exists) (ordered-map-delete (cdr u-out) v))
     (cond
       [exists
        (define-values (v-in-new _) (ordered-map-delete (cdr v-in) u))
        (define out^ (ordered-map-set out u u-out-new))
        (define in^ (ordered-map-set in v v-in-new))
        (Graph cmp-fn out^ in^)]
       [else graph])]
    [else graph]))

(define (remove-edge graph u v)
  (remove-directed-edge (remove-directed-edge graph u v) v u))

;; === Traversal ===

(define (get-vertices graph)
  (for/fold ([vs pvector-empty]) ([v (in-dict-keys (Graph-out graph))])
    (pvector-cons-left vs v)))

(define (in-vertices graph)
  (in-dict-keys (Graph-out graph)))

(define (get-neighbors graph u)
  (define u-out (ordered-map-query (Graph-out graph) u))
  (define u-out^ (cdr u-out))
  (for/fold ([vs pvector-empty]) ([v (in-dict-keys u-out^)])
    (pvector-cons-left vs v)))

(define (in-neighbors graph u)
  (define u-out (ordered-map-query (Graph-out graph) u))
  (define u-out^ (cdr u-out))
  (in-dict-keys u-out^))

(define (get-edges graph)
  (for/fold ([es pvector-empty]) ([(u tos) (in-dict graph)])
    (for/fold ([ess es]) ([to (in-dict-keys tos)])
      (pvector-cons-left ess (cons u to)))))

(define (in-edges graph)
  (in-pvector (get-edges graph)))

(define (transpose graph)
  (match-define (Graph cmp-fn out in) graph)
  (Graph cmp-fn in out))

;; === Exports ===

(provide graph-make-empty graph-make-empty-raw graph-make-empty-normal)
(provide has-vertex? has-edge?)
(provide vertex=?)
(provide add-vertex add-edge add-directed-edge)
(provide remove-vertex remove-directed-edge remove-edge)
(provide get-neighbors get-vertices in-vertices in-neighbors get-edges in-edges)
(provide transpose)
(provide (struct-out Graph))
