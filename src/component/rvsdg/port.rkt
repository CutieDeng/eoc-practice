#lang racket/base

;; ============================================================
;; Component: the CFG -> RVSDG structurizer's port (component pattern)
;; ============================================================
;;
;; The structurizer (structurize.rkt) reads a control-flow graph only
;; through this port: a record of closures over an opaque adapter value
;; `a`, bundled with it as a "cfg view" `cv` = (cons port a).
;;
;;   entry  : a -> bid
;;   blocks : a -> pvector bid
;;   block  : a bid -> CfgBlock or #f
;;   info   : a key default -> value          extension queries
;;
;; Exchange format: the port answers with the kernel prefabs
;; CfgBlock (phis insns terminator), PhiInsn, VfInsn and Term:*
;; (kernel/ir/cfg/types.rkt).  They are small persistent values; an
;; adapter for another IR builds them on demand (the GIMPLE adapter
;; turns statements into VfInsn views), it does not store them.
;; Successors come from the terminators; back edges from the
;; dominance driver.
;; ============================================================

(require "../../kernel/data/data.rkt"
         "../../kernel/ir/cfg/types.rkt"
         "../../component/cfg/utils/graph-ops.rkt"
         "../../driver/loop/analysis.rkt")

(provide (struct-out rvsdg-port) make-cv
         cv-entry cv-blocks cv-block cv-info cv-succs cv-back-edges
         kernel-cfg-port)

(struct rvsdg-port (entry blocks block info))

(define (make-cv port a) (cons port a))
(define (cv-entry cv) ((rvsdg-port-entry (car cv)) (cdr cv)))
(define (cv-blocks cv) ((rvsdg-port-blocks (car cv)) (cdr cv)))
(define (cv-block cv bid) ((rvsdg-port-block (car cv)) (cdr cv) bid))
(define (cv-info cv key [default #f]) ((rvsdg-port-info (car cv)) (cdr cv) key default))

(define (cv-succs cv bid)
  (define blk (cv-block cv bid))
  (if blk (terminator-successors (CfgBlock-terminator blk)) (pvector-empty)))

;; (tail . header) pairs, over the terminator graph
(define (cv-back-edges cv)
  (define nodes (cv-blocks cv))
  (define (succs b) (cv-succs cv b))
  (define preds
    (let ([m (for*/fold ([m (hash)]) ([b (in-pvector nodes)] [s (in-pvector (succs b))])
               (hash-set m s (pvector-cons-right (hash-ref m s (pvector-empty)) b)))])
      (lambda (b) (hash-ref m b (pvector-empty)))))
  (find-back-edges nodes (cv-entry cv) succs preds))

;; the kernel Cfg is its own adapter
(define kernel-cfg-port
  (rvsdg-port Cfg-entry
              (lambda (c) (list->pvector (cfg-all-block-ids c)))
              cfg-get-block
              cfg-get-info))
