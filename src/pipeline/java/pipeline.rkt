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
;;     cfg->rvsdg.  cfg->rvsdg handles linear jump chains, if-else
;;     diamonds with multi-block linear arms and nested inner
;;     diamonds (→ Gamma), and single-loop while-patterns whose body
;;     may span multiple blocks — mixing Term:jump steps with inner
;;     convergent diamonds — before reaching the latch (→ Theta).
;;     Nested loops and loops inside Gamma arms are also handled:
;;     every back-edge becomes an entry in a per-compilation header→
;;     Theta-Ctx map, and translate-segment consults that map on each
;;     block so headers encountered deep inside a sub-region still
;;     lower into a Theta living in the surrounding region.  A
;;     Term:cond whose two arms are each a single block ending in
;;     Term:ret / Term:throw lowers to a terminal Gamma whose two
;;     sub-regions install their own return / throw sinks.  When only
;;     ONE of the two arms is such a terminal block, the cond lowers
;;     to an asymmetric early-exit Gamma: the exit sub-region owns
;;     the return / throw sink while the continue sub-region is a no-
;;     op; translate-segment then resumes from the continuing
;;     branch's block in the outer region.
;;   - Methods whose control flow still exceeds lowering capacity
;;     (switch, try/catch, multi-block early-exit arms, early-exit
;;     nested inside another Gamma / Theta, or a loop header with
;;     multiple back-edges) error from cfg->rvsdg;
;;     `java-compile-class` catches each exception per method rather
;;     than aborting the whole class.
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
    'supported '(jvm-to-cfg ssa-construct linear-rvsdg
                 gamma-recovery gamma-multi-block-arms
                 gamma-nested-diamonds theta-recovery
                 theta-multi-block-body theta-gamma-inside-body
                 nested-thetas theta-inside-gamma
                 gamma-early-exit-both-arms
                 gamma-early-exit-asymmetric)
    'deferred  '(switch-recovery kappa-recovery
                 gamma-early-exit-multi-block-arm
                 gamma-early-exit-inside-region
                 multi-latch-loops)))
