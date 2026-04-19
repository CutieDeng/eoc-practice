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
;;     branch's block in the outer region.  If instead both arms are
;;     multi-block yet each arm's reach-set (walked over Term:jump /
;;     Term:cond) contains only ret / throw leaves AND the two
;;     reach-sets are disjoint, the cond lowers as a multi-block
;;     terminal Gamma: each sub-region is built by recursing into
;;     translate-segment and installing the returned ret / throw
;;     payload as the sub-region's sink.  An early-exit sitting
;;     INSIDE a standard diamond's arm — single-block or multi-
;;     block — is also tolerated: translate-gamma precomputes each
;;     outer arm's full forward-reach, derives the shared set (their
;;     intersection), and passes it via `current-shared-set`, which
;;     lets `arm-advance` recognise any exit subtree whose reach is
;;     disjoint from the shared set and skip past it to find the
;;     outer join.  Inside the outer arm's sub-region, the inner
;;     cond lowers via the asymmetric early-exit path: the dispatch
;;     checks whether the active stop-bid lies in exactly one arm's
;;     reach-set; if so the other arm's reach-set becomes the exit
;;     sub-region (possibly spanning many blocks).
;;     An early-exit sitting inside a Theta loop body is handled
;;     analogously: `arm-reach-set` respects `current-arm-scope`
;;     (set to the loop's body-blocks by translate-theta) so a
;;     Term:cond arm that jumps back to the header shows up as a
;;     boundary terminator, and the new `reach-set-reaches-stop?`
;;     helper classifies the arm that targets the header as the
;;     continue arm.  The other arm — single-block or multi-block
;;     ending in ret / throw — lowers to the exit sub-region of an
;;     asymmetric Gamma installed inside the Theta body.
;;   - Methods whose control flow still exceeds lowering capacity
;;     (switch, try/catch, or a loop header with multiple back-
;;     edges) error from cfg->rvsdg;
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
                 gamma-early-exit-asymmetric
                 gamma-early-exit-multi-block-arm
                 gamma-early-exit-inside-gamma-single-block
                 gamma-early-exit-inside-gamma-multi-block
                 gamma-early-exit-inside-theta-single-block
                 gamma-early-exit-inside-theta-multi-block)
    'deferred  '(switch-recovery kappa-recovery
                 multi-latch-loops)))
