#lang racket/base

;; ============================================================
;; Pipeline: Pass Definition and Composition
;; ============================================================
;;
;; Provides utilities for defining and composing compiler passes
;; into complete compilation pipelines.
;;
;; Design:
;;   - A Pass is a named transformation with metadata
;;   - A Pipeline is a sequence of passes
;;   - Passes can be composed, filtered, and profiled
;;
;; ============================================================

(require racket/match racket/list)

(provide
  ;; Pass definition
  (struct-out Pass)
  make-pass

  ;; Pipeline definition
  (struct-out Pipeline)
  make-pipeline

  ;; Execution
  run-pass
  run-pipeline
  run-pipeline-with-stats

  ;; Composition
  pipeline-append
  pipeline-prepend
  pipeline-filter

  ;; Utilities
  pass-enabled?
  pipeline-pass-names)

;; ============================================================
;; Pass Structure
;; ============================================================

;; A compiler pass
;;
;; Fields:
;;   name        : Symbol - pass name
;;   description : String - what this pass does
;;   transform   : (IR -> IR) - the transformation function
;;   enabled?    : Boolean - whether pass is active
;;   requires    : (Listof Symbol) - passes that must run before
;;   invalidates : (Listof Symbol) - analyses invalidated by this pass
;;
(struct Pass (
  name
  description
  transform
  enabled?
  requires
  invalidates
) #:prefab)

;; Create a pass with defaults
(define (make-pass name transform
                   #:description [desc ""]
                   #:enabled? [enabled? #t]
                   #:requires [requires '()]
                   #:invalidates [invalidates '()])
  (Pass name desc transform enabled? requires invalidates))

;; ============================================================
;; Pipeline Structure
;; ============================================================

;; A compilation pipeline
;;
;; Fields:
;;   name   : Symbol - pipeline name
;;   passes : (Listof Pass) - ordered list of passes
;;   config : Hash - configuration options
;;
(struct Pipeline (
  name
  passes
  config
) #:prefab)

;; Create a pipeline
(define (make-pipeline name passes #:config [config (hash)])
  (Pipeline name passes config))

;; ============================================================
;; Pass Execution
;; ============================================================

;; Run a single pass
;; Returns: (values result stats)
;;
(define (run-pass pass ir)
  (if (Pass-enabled? pass)
      (let ()
        (define start-time (current-inexact-milliseconds))
        (define result ((Pass-transform pass) ir))
        (define end-time (current-inexact-milliseconds))
        (values result
                (hash 'name (Pass-name pass)
                      'time-ms (- end-time start-time)
                      'ran #t)))
      (values ir
              (hash 'name (Pass-name pass)
                    'time-ms 0
                    'ran #f))))

;; ============================================================
;; Pipeline Execution
;; ============================================================

;; Run a pipeline on IR
;; Returns: final IR
;;
(define (run-pipeline pipeline ir)
  (define passes (Pipeline-passes pipeline))
  (for/fold ([current-ir ir])
            ([pass passes])
    (define-values (result _stats) (run-pass pass current-ir))
    result))

;; Run a pipeline with statistics
;; Returns: (values final-ir stats-list)
;;
(define (run-pipeline-with-stats pipeline ir)
  (define passes (Pipeline-passes pipeline))
  (define stats-list '())

  (define final-ir
    (for/fold ([current-ir ir])
              ([pass passes])
      (define-values (result stats) (run-pass pass current-ir))
      (set! stats-list (cons stats stats-list))
      result))

  (values final-ir (reverse stats-list)))

;; ============================================================
;; Pipeline Composition
;; ============================================================

;; Append passes to a pipeline
(define (pipeline-append pipeline . passes)
  (struct-copy Pipeline pipeline
    [passes (append (Pipeline-passes pipeline) passes)]))

;; Prepend passes to a pipeline
(define (pipeline-prepend pipeline . passes)
  (struct-copy Pipeline pipeline
    [passes (append passes (Pipeline-passes pipeline))]))

;; Filter passes in a pipeline
(define (pipeline-filter pipeline pred)
  (struct-copy Pipeline pipeline
    [passes (filter pred (Pipeline-passes pipeline))]))

;; ============================================================
;; Utilities
;; ============================================================

;; Check if a pass is enabled
(define (pass-enabled? pass)
  (Pass-enabled? pass))

;; Get names of all passes in a pipeline
(define (pipeline-pass-names pipeline)
  (map Pass-name (Pipeline-passes pipeline)))
