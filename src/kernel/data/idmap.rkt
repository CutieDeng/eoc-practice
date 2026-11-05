#lang racket/base

;; ============================================================
;; idmap: persistent map keyed by typed ids, stored as an intmap
;; ============================================================
;;
;; IR tables are keyed by single-field prefab ids (BlockId, VarId,
;; NodeId, WireId, InputId, OutputId, InsnId ...).  An idmap stores
;; them in a racket/intmap keyed by the id's integer payload, and
;; keeps the typed key next to the value so iteration hands back
;; the typed id without needing the id's constructor:
;;
;;     intmap[ id-int -> (cons typed-id value) ]
;;
;; Properties:
;;   - representation is a plain intmap: write/read round-trips,
;;     prefab-compatible, equal?-comparable
;;   - iteration order is ascending id
;;   - no comparator stored, no closure dispatch: the key payload is
;;     read with one unsafe-struct-ref
;;
;; Keys may also be plain exact integers (payload = the integer) or #f.
;; ============================================================

(require racket/intmap
         racket/unsafe/ops)

(provide idmap-empty
         idmap?
         idmap-empty?
         idmap-count
         idmap-ref
         idmap-set
         idmap-has-key?
         idmap-remove
         idmap-query
         idmap-min
         idmap-keys
         idmap-values
         idmap->pairs
         in-idmap
         in-idmap-keys
         in-idmap-values
         id-val)

(define idmap-empty intmap-empty)
(define (idmap? v) (intmap? v))
(define (idmap-empty? m) (intmap-empty? m))
(define (idmap-count m) (intmap-count m))

;; Integer payload of a typed id (or the integer itself).  #f is
;; accepted as a key and sorts before every id (payload -1): some
;; tables use #f for "no block" (e.g. method-end try windows).
(define (id-val k)
  (cond
    [(exact-integer? k) k]
    [(not k) -1]
    [else (unsafe-struct-ref k 0)]))

(define (idmap-ref m k [default (lambda () (error 'idmap-ref "key not found: ~e" k))])
  (define e (intmap-ref m (id-val k) #f))
  (cond
    [e (cdr e)]
    [(procedure? default) (default)]
    [else default]))

(define (idmap-set m k v)
  (intmap-set m (id-val k) (cons k v)))

(define (idmap-has-key? m k)
  (intmap-has-key? m (id-val k)))

(define (idmap-remove m k)
  (intmap-remove m (id-val k)))

;; -> (cons typed-key value) or #f
(define (idmap-query m k)
  (intmap-ref m (id-val k) #f))

;; -> (cons typed-key value) or #f, for the smallest id
(define (idmap-min m)
  (define e (intmap-min-entry m))
  (and e (cdr e)))

(define (idmap->pairs m)
  (for/list ([e (in-intmap-values m)]) e))
(define (idmap-keys m)
  (for/list ([e (in-intmap-values m)]) (car e)))
(define (idmap-values m)
  (for/list ([e (in-intmap-values m)]) (cdr e)))

;; Sequences.  in-idmap yields (cons typed-key value) as ONE value,
;; matching how call sites destructure with car/cdr.
(define (in-idmap m) (in-intmap-values m))
(define (in-idmap-keys m) (in-list (idmap-keys m)))
(define (in-idmap-values m) (in-list (idmap-values m)))

(module+ test
  (require rackunit racket/port)
  (struct K (id) #:prefab)
  (define m (idmap-set (idmap-set (idmap-set idmap-empty (K 5) 'five) (K 2) 'two) 9 'nine))
  (check-equal? (idmap-count m) 3)
  (check-equal? (idmap-ref m (K 2)) 'two)
  (check-equal? (idmap-ref m (K 7) 'none) 'none)
  (check-equal? (idmap-ref m (K 7) (lambda () 'thunk)) 'thunk)
  (check-true (idmap-has-key? m 9))
  (check-true (idmap-has-key? m (K 9)) "K 9 and 9 share the payload: keys collide by design")
  (check-equal? (idmap-keys m) (list (K 2) (K 5) 9))
  (check-equal? (idmap-values m) '(two five nine))
  (check-equal? (idmap-min m) (cons (K 2) 'two))
  (check-equal? (idmap-query m (K 5)) (cons (K 5) 'five))
  (check-equal? (for/list ([kv (in-idmap m)]) (car kv)) (list (K 2) (K 5) 9))
  (check-equal? (idmap-count (idmap-remove m (K 5))) 2)
  (check-equal? (idmap-remove m (K 77)) m)
  ;; serialization round trip
  (define s (with-output-to-string (lambda () (write m))))
  (check-equal? (read (open-input-string s)) m))
