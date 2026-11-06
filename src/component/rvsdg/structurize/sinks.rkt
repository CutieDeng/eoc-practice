#lang racket/base

;; ============================================================
;; CFG -> RVSDG structurizer: return / throw sink nodes
;; ============================================================
;; Split out of structurize.rkt (roadmap Phase 3.1); see that file's
;; header for the algorithm.  The CFG is read through port.rkt.
;; ============================================================

(require racket/match
         racket/list
         "../../../kernel/ir/cfg/types.rkt"
         "../../../kernel/ir/rvsdg/rvsdg.rkt"
         "../utils/builder.rkt"
         "../port.rkt"
         "../../../kernel/data/data.rkt"
         "ctx.rkt")

(provide install-return install-throw)


;; ============================================================
;; Terminator materialisation
;; ============================================================

(define (install-return region var->out ret-values)
  (define n (pvector-length ret-values))
  (define-values (region1 _nid in-ids _outs)
    (region-add-node region (Simple 'return) n 0))
  (for/fold ([r region1])
            ([v (in-pvector ret-values)]
             [iid (in-pvector in-ids)])
    (define oid (or (idmap-ref var->out v #f)
                    (error 'install-return "undef ~a" v)))
    (define-values (r* _w) (region-add-wire r oid iid))
    r*))


(define (install-throw region var->out exn)
  (define-values (region1 _nid in-ids _outs)
    (region-add-node region (Throw #f) 1 0))
  (define oid (or (idmap-ref var->out exn #f)
                  (error 'install-throw "undef ~a" exn)))
  (define-values (r* _w) (region-add-wire region1 oid (pvector-ref in-ids 0)))
  r*)
