#lang racket/base

;; ============================================================
;; Foundation: Graph Data Structure
;; ============================================================
;;
;; Immutable directed graph implementation based on finger trees.
;; Supports both directed and undirected edge operations.
;; ============================================================

(require racket/match racket/dict)
(require "ftree.rkt")

;; Graph structure: comparison function + outgoing edges + incoming edges
(struct Graph (cmp-fn out in))

;; === Constructors ===

(define (graph-make-empty)
  (Graph integer-compare
         (ordl-make-empty integer-compare)
         (ordl-make-empty integer-compare)))

(define (graph-make-empty-raw cmp-fn)
  (Graph cmp-fn
         (ordl-make-empty cmp-fn)
         (ordl-make-empty cmp-fn)))

(define (graph-make-empty-normal)
  (define graph (Graph integer-compare
                       (ordl-make-empty integer-compare)
                       (ordl-make-empty integer-compare)))
  (add-vertex (add-vertex graph 0) 1))

;; === Queries ===

(define (has-vertex? graph vertex)
  (if (ordl-query (Graph-out graph) vertex) #t #f))

(define (has-edge? graph u v)
  (define u-out (ordl-query (Graph-out graph) u))
  (cond
    [u-out
     (define u-out^ (cdr u-out))
     (if (ordl-query u-out^ v) #t #f)]
    [else #f]))

(define (vertex=? graph u v) (equal? u v))

;; === Vertex Operations ===

(define (add-vertex graph u)
  (match-define (Graph cmp-fn out in) graph)
  (cond
    [(ordl-query out u) graph]
    [else
     (define out^ (ordl-insert out u (ordl-make-empty cmp-fn) #f))
     (define in^ (ordl-insert in u (ordl-make-empty cmp-fn) #f))
     (Graph cmp-fn out^ in^)]))

(define (remove-vertex graph u)
  (match-define (Graph _ out in) graph)
  (define u-out (ordl-query out u))
  (define u-in (ordl-query in u))
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
  (define u-out (ordl-query out u))
  (define v-in (ordl-query in v))
  (cond
    [(and u-out v-in)
     (define u-out^ (ordl-insert (cdr u-out) v #f #f))
     (cond
       [(eq? u-out u-out^) graph]
       [else
        (define v-in^ (ordl-insert (cdr v-in) u #f #f))
        (define out^ (ordl-insert out u u-out^ #t))
        (define in^ (ordl-insert in v v-in^ #t))
        (Graph cmp-fn out^ in^)])]
    [else graph]))

(define (remove-directed-edge graph u v)
  (match-define (Graph cmp-fn out in) graph)
  (define u-out (ordl-query out u))
  (define v-in (ordl-query in v))
  (cond
    [(and u-out v-in)
     (match-define-values (u-out^ exists) (ordl-delete (cdr u-out) v))
     (cond
       [exists
        (match-define-values (v-in^ _) (ordl-delete (cdr v-in) u))
        (define out^ (ordl-insert out u u-out^ #t))
        (define in^ (ordl-insert in v v-in^ #t))
        (Graph cmp-fn out^ in^)]
       [else graph])]
    [else graph]))

(define (remove-edge graph u v)
  (remove-directed-edge (remove-directed-edge graph u v) v u))

;; === Traversal ===

(define (get-vertices graph)
  (for/fold ([vs (ral-empty)]) ([v (in-dict-keys (Graph-out graph))])
    (ral-consl vs v)))

(define (in-vertices graph)
  (in-dict-keys (Graph-out graph)))

(define (get-neighbors graph u)
  (define u-out (ordl-query (Graph-out graph) u))
  (define u-out^ (cdr u-out))
  (for/fold ([vs (ral-empty)]) ([v (in-dict-keys u-out^)])
    (ral-consl vs v)))

(define (in-neighbors graph u)
  (define u-out (ordl-query (Graph-out graph) u))
  (define u-out^ (cdr u-out))
  (in-dict-keys u-out^))

(define (get-edges graph)
  (for/fold ([es (ral-empty)]) ([(u tos) (in-dict graph)])
    (for/fold ([ess es]) ([to (in-dict-keys tos)])
      (ral-consl ess (cons u to)))))

(define (in-edges graph)
  (in-ral0 (get-edges graph)))

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
