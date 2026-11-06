#lang racket/base

;; ============================================================
;; Kernel IR: gimple -- datum (de)serialization
;; ============================================================
;;
;; prefab structs and immutable hasheqs survive write/read unchanged,
;; pvectors and intmaps do not (`#pvector(...)` has no reader syntax,
;; verified on Racket 9.3.13).  value->datum rewrites them into two
;; prefab carriers, datum->value undoes it.  Everything else (symbols,
;; numbers, strings, booleans, pairs) passes through.
;;
;;   #s(PV (elem ...))          pvector
;;   #s(IM ((key . val) ...))   intmap, ascending keys
;; ============================================================

(require racket/intmap
         "../../data/data.rkt")

(provide value->datum datum->value (struct-out PV) (struct-out IM))

(struct PV (elems) #:prefab)
(struct IM (entries) #:prefab)

(define (value->datum v)
  (cond
    [(pvector? v) (PV (for/list ([x (in-pvector v)]) (value->datum x)))]
    [(intmap? v) (IM (for/list ([p (in-list (intmap->literal-datum v))])
                       (cons (car p) (value->datum (cdr p)))))]
    [(hash? v) (for/hasheq ([(k x) (in-hash v)]) (values k (value->datum x)))]
    [(prefab-struct-key v)
     => (lambda (key)
          (apply make-prefab-struct key
                 (for/list ([x (in-vector (struct->vector v) 1)]) (value->datum x))))]
    [(pair? v) (cons (value->datum (car v)) (value->datum (cdr v)))]
    [else v]))

(define (datum->value d)
  (cond
    [(PV? d) (list->pvector (map datum->value (PV-elems d)))]
    [(IM? d) (literal-datum->intmap (for/list ([p (in-list (IM-entries d))])
                                      (cons (car p) (datum->value (cdr p)))))]
    [(hash? d) (for/hasheq ([(k x) (in-hash d)]) (values k (datum->value x)))]
    [(prefab-struct-key d)
     => (lambda (key)
          (apply make-prefab-struct key
                 (for/list ([x (in-vector (struct->vector d) 1)]) (datum->value x))))]
    [(pair? d) (cons (datum->value (car d)) (datum->value (cdr d)))]
    [else d]))
