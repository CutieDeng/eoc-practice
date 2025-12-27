#lang racket/base

;; ============================================================
;; Abstract Optimization Pass Framework
;; ============================================================
;;
;; Defines the structure for optimization passes that work with
;; any instruction set through the semantics abstraction.
;;
;; Key Concepts:
;;   - OptPass: A transformation on CFG using semantics
;;   - PassContext: Runtime context for a pass execution
;;   - PassResult: Result of running a pass
;; ============================================================

(require racket/match racket/list racket/string)
(require "semantics.rkt")
(require "../../ir/cfg/main.rkt")

;; ============================================================
;; Optimization Pass Structure
;; ============================================================

;; An optimization pass
(struct OptPass (
  name           ; Symbol - pass name for debugging/stats
  description    ; String - what this pass does
  transform      ; (Cfg × InsnSemantics × PassContext) -> PassResult
  ;; Optional properties
  preserves      ; (Listof Symbol) - analysis results this pass preserves
  requires       ; (Listof Symbol) - analysis results this pass needs
  invalidates    ; (Listof Symbol) - analysis results this pass invalidates
) #:transparent)

(provide (struct-out OptPass))

;; ============================================================
;; Pass Context
;; ============================================================

;; Runtime context available to passes
(struct PassContext (
  semantics      ; InsnSemantics - instruction semantics
  analyses       ; Hash[Symbol -> Any] - cached analysis results
  options        ; Hash[Symbol -> Any] - pass configuration options
  stats          ; (Box Hash) - mutable statistics accumulator
) #:transparent)

;; Create a new context
(define (make-context semantics
                       #:analyses [analyses (hash)]
                       #:options [options (hash)])
  (PassContext semantics analyses options (box (hash))))

;; Get semantics from context
(define (ctx-semantics ctx)
  (PassContext-semantics ctx))

;; Get cached analysis
(define (ctx-get-analysis ctx key)
  (hash-ref (PassContext-analyses ctx) key #f))

;; Update analysis cache
(define (ctx-set-analysis ctx key value)
  (struct-copy PassContext ctx
               [analyses (hash-set (PassContext-analyses ctx) key value)]))

;; Get option value
(define (ctx-option ctx key [default #f])
  (hash-ref (PassContext-options ctx) key default))

;; Record a statistic
(define (ctx-record-stat! ctx key value)
  (define stats-box (PassContext-stats ctx))
  (set-box! stats-box (hash-set (unbox stats-box) key value)))

;; Increment a counter statistic
(define (ctx-incr-stat! ctx key [delta 1])
  (define stats-box (PassContext-stats ctx))
  (define current (hash-ref (unbox stats-box) key 0))
  (set-box! stats-box (hash-set (unbox stats-box) key (+ current delta))))

;; Get all statistics
(define (ctx-get-stats ctx)
  (unbox (PassContext-stats ctx)))

(provide (struct-out PassContext)
         make-context
         ctx-semantics ctx-get-analysis ctx-set-analysis
         ctx-option ctx-record-stat! ctx-incr-stat! ctx-get-stats)

;; ============================================================
;; Pass Result
;; ============================================================

;; Result of running a pass
(struct PassResult (
  cfg            ; Cfg - transformed CFG
  changed?       ; Boolean - whether anything changed
  context        ; PassContext - possibly updated context
  messages       ; (Listof String) - debug/info messages
) #:transparent)

;; Create a result indicating no change
(define (pass-unchanged cfg ctx)
  (PassResult cfg #f ctx '()))

;; Create a result indicating change
(define (pass-changed cfg ctx)
  (PassResult cfg #t ctx '()))

;; Create a result with messages
(define (pass-result cfg changed? ctx #:messages [msgs '()])
  (PassResult cfg changed? ctx msgs))

(provide (struct-out PassResult)
         pass-unchanged pass-changed pass-result)

;; ============================================================
;; Pass Execution
;; ============================================================

;; Run a single pass
(define (run-pass pass cfg ctx)
  (ctx-record-stat! ctx 'pass-name (OptPass-name pass))
  ((OptPass-transform pass) cfg (ctx-semantics ctx) ctx))

;; Run a pass and get stats
(define (run-pass-with-stats pass cfg ctx)
  (define result (run-pass pass cfg ctx))
  (values (PassResult-cfg result)
          (PassResult-changed? result)
          (ctx-get-stats (PassResult-context result))))

(provide run-pass run-pass-with-stats)

;; ============================================================
;; Pass Composition
;; ============================================================

;; Compose multiple passes into one
(define (compose-passes . passes)
  (define names (map OptPass-name passes))
  (OptPass
   (string->symbol (string-join (map symbol->string names) "+"))
   (format "Composed: ~a" names)
   (lambda (cfg sem ctx)
     (define-values (final-cfg changed? final-ctx)
       (for/fold ([cfg cfg]
                  [any-changed? #f]
                  [ctx ctx])
                 ([pass passes])
         (define result (run-pass pass cfg ctx))
         (values (PassResult-cfg result)
                 (or any-changed? (PassResult-changed? result))
                 (PassResult-context result))))
     (PassResult final-cfg changed? final-ctx '()))
   '() '() '()))

;; Run passes until fixpoint
(define (run-until-fixpoint passes cfg ctx #:max-iters [max-iters 10])
  (let loop ([cfg cfg]
             [ctx ctx]
             [iter 0])
    (if (>= iter max-iters)
        (PassResult cfg #f ctx
                    (list (format "Fixpoint not reached after ~a iterations" max-iters)))
        (let ([result ((OptPass-transform (apply compose-passes passes))
                       cfg (ctx-semantics ctx) ctx)])
          (if (PassResult-changed? result)
              (loop (PassResult-cfg result)
                    (PassResult-context result)
                    (+ iter 1))
              (PassResult (PassResult-cfg result) #t
                          (PassResult-context result)
                          (list (format "Fixpoint reached after ~a iterations" (+ iter 1)))))))))

(provide compose-passes run-until-fixpoint)

;; ============================================================
;; Common Pass Patterns
;; ============================================================

;; Create a pass that transforms each block independently
(define (make-local-pass name desc block-transform)
  (OptPass
   name desc
   (lambda (cfg sem ctx)
     (define changed? #f)
     (define cfg^
       (for/fold ([cfg cfg])
                 ([bid (cfg-all-block-ids cfg)])
         (define block (cfg-get-block cfg bid))
         (if block
             (let ([new-block (block-transform block sem ctx)])
               (when (not (equal? block new-block))
                 (set! changed? #t))
               (cfg-set-block cfg new-block))
             cfg)))
     (if changed?
         (pass-changed cfg^ ctx)
         (pass-unchanged cfg ctx)))
   '() '() '()))

;; Create a pass that transforms instructions within blocks
(define (make-insn-pass name desc insn-transform)
  (make-local-pass
   name desc
   (lambda (block sem ctx)
     (define new-insns
       (for/list ([insn (CfgBlock-insns block)])
         (if (VfInsn? insn)
             (insn-transform insn sem ctx)
             insn)))
     (if (equal? new-insns (CfgBlock-insns block))
         block
         (struct-copy CfgBlock block [insns new-insns])))))

;; Create a pass that filters out instructions
(define (make-filter-pass name desc keep?)
  (make-local-pass
   name desc
   (lambda (block sem ctx)
     (define new-insns
       (filter (lambda (insn)
                 (or (not (VfInsn? insn))
                     (keep? insn sem ctx)))
               (CfgBlock-insns block)))
     (if (= (length new-insns) (length (CfgBlock-insns block)))
         block
         (begin
           (ctx-incr-stat! ctx 'removed
                           (- (length (CfgBlock-insns block))
                              (length new-insns)))
           (struct-copy CfgBlock block [insns new-insns]))))))

(provide make-local-pass make-insn-pass make-filter-pass)

;; ============================================================
;; Pass Registry
;; ============================================================

;; Global registry of available passes
(define pass-registry (make-hash))

(define (register-pass! pass)
  (hash-set! pass-registry (OptPass-name pass) pass))

(define (get-pass name)
  (hash-ref pass-registry name #f))

(define (list-passes)
  (hash-keys pass-registry))

(provide register-pass! get-pass list-passes)
