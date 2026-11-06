#lang racket/base

;; ============================================================
;; Kernel IR: gccast -- GCC GENERIC dump as produced by gccast-plugin
;; ============================================================
;;
;; Pure data.  Prefab names must match gccast-plugin/src/gccast.cc:
;;
;;   #s(TU gcc-version main-file fns)        fns : list of Fn
;;   #s(Fn name decl body nodes)             decl/body : TreeId; nodes : vector of Node
;;   #s(Node code type loc fields)           code : symbol (GCC tree code name, e.g. 'plus_expr)
;;                                           type : TreeId or #f
;;                                           loc  : (list file line col) or #f
;;                                           fields : alist
;;   #s(TreeId n)                            index into the enclosing Fn's node vector
;;   #s(Wide precision limbs)                INTEGER_CST wider than a host word
;;
;; Field conventions (see gccast.cc):
;;   expressions          : ((ops ref ...) [side-effects] [volatile] [readonly]
;;                          [constant] [internal flags: CALL_EXPR] [vars: BIND_EXPR])
;;   declarations         : uid name public static external artificial
;;                          readonly volatile addressable used ignored register
;;                          [not-gimple-reg] [context] size size-unit
;;                          [initial | arguments result end-locus | offset bit-offset
;;                          bit-field | arg-type | forced]
;;   types                : uid name size align quals addressable mode
;;                          [canonical-uid] [string-flag] [prototype] size-unit
;;                          [main-variant precision
;;                          unsigned min max domain fields arg-types values]
;;   constants            : value | hex | bytes | real imag
;;   exceptional          : id | purpose value chain | elts | vars subblocks start-loc end-loc | stmts
;; ============================================================

(provide (struct-out TU)
         (struct-out Fn)
         (struct-out Node)
         (struct-out TreeId)
         (struct-out Wide)
         node-field
         node-ops
         wide->integer)

(struct TU (gcc-version main-file fns) #:prefab)
(struct Fn (name decl body nodes) #:prefab)
(struct Node (code type loc fields) #:prefab)
(struct TreeId (id) #:prefab)
(struct Wide (precision limbs) #:prefab)

;; Alist lookup on a node's fields.
(define (node-field n key [default #f])
  (define p (assq key (Node-fields n)))
  (if p (cdr p) default))

;; Operand refs of an expression node (empty for non-expressions).
(define (node-ops n)
  (node-field n 'ops '()))

;; Reconstruct the two's-complement value of a Wide (limbs are
;; little-endian signed 64-bit host words, sign-extended to precision).
(define (wide->integer w)
  (define limbs (Wide-limbs w))
  (define n (length limbs))
  (define raw
    (for/fold ([acc 0]) ([l (in-list (reverse limbs))])
      (+ (* acc (expt 2 64)) (bitwise-and l (sub1 (expt 2 64))))))
  ;; top limb is signed: if negative, subtract 2^(64n)
  (if (negative? (list-ref limbs (sub1 n)))
      (- raw (expt 2 (* 64 n)))
      raw))
