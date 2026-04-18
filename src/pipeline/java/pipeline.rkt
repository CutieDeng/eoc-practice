#lang racket/base

;; ============================================================
;; Pipeline: Java Compilation
;; ============================================================
;;
;; Complete compilation pipeline for Java bytecode:
;;   .class file → JVM IR → CFG → SSA CFG → RVSDG
;;
;; Current scope:
;;   - Reads a class file via the ASM-based reader.
;;   - Runs each method through jvm-method->cfg, jvm-cfg->ssa, and
;;     cfg->rvsdg.  Methods whose control flow exceeds the initial
;;     RVSDG lowering (Term:cond / switch / loops) currently error
;;     from cfg->rvsdg -- we propagate that to the caller rather
;;     than silently skipping.
;;
;; ============================================================

(require "ir/types.rkt"
         "../../frontend/java/reader.rkt"
         "../../component/java/transform/jvm-to-cfg.rkt"
         "../../component/java/transform/ssa-construct.rkt"
         "../../component/java/transform/cfg-to-rvsdg.rkt")

(provide
  java-pipeline-info
  java-compile-class
  java-compile-method
  java-method->cfg
  java-method->ssa-cfg
  java-method->rvsdg)

;; ============================================================
;; Per-method passes
;; ============================================================

(define (java-method->cfg mth)
  (jvm-method->cfg mth))

(define (java-method->ssa-cfg mth)
  (jvm-cfg->ssa (jvm-method->cfg mth)))

(define (java-method->rvsdg mth)
  (cfg->rvsdg (jvm-cfg->ssa (jvm-method->cfg mth))))

;; ============================================================
;; Class-level driver
;; ============================================================

;; Compile every method in a class file path.  Returns a list of
;; (cons method-name lambda-or-error).  Any method that cannot yet
;; be lowered surfaces as (cons name (cons 'error exn-message)) so a
;; caller can report progress without aborting on the first branch-
;; heavy method.
(define (java-compile-class path)
  (define klass (read-jvm-class-file path))
  (for/list ([mth (in-list (JvmClass-methods klass))])
    (with-handlers ([exn:fail? (lambda (e)
                                  (cons (JvmMethod-name mth)
                                        (cons 'error (exn-message e))))])
      (cons (JvmMethod-name mth)
            (java-method->rvsdg mth)))))

;; Single-method convenience: raises on failure.
(define (java-compile-method mth)
  (java-method->rvsdg mth))

;; ============================================================
;; Pipeline metadata
;; ============================================================

(define (java-pipeline-info)
  (hash
    'name 'java
    'description "Java bytecode compilation pipeline"
    'status 'partial
    'supported '(jvm-to-cfg ssa-construct linear-rvsdg)
    'deferred  '(gamma-recovery theta-recovery kappa-recovery)))
