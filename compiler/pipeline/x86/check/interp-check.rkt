#lang racket/base

;; ============================================================
;; Interp Checker
;; ============================================================
;;
;; Verifies semantic preservation across compilation passes.
;; Runs interpreters on input and output of each pass,
;; ensuring they produce the same result.
;;
;; ============================================================

(require racket/match
         racket/format
         "../interp/interp-ast.rkt"
         "../interp/interp-cvar.rkt"
         "../interp/interp-x86.rkt"
         "../passes/uniquify.rkt"
         "../passes/remove-complex.rkt"
         "../passes/explicate-control.rkt"
         "../passes/select-instructions.rkt"
         "../passes/assign-homes.rkt"
         "../passes/patch-instructions.rkt"
         "../passes/prelude-conclusion.rkt"
         "../../../kernel/ir/ast/types.rkt")

(provide check-pass
         check-pipeline
         check-all-passes
         (struct-out interp-check-result))

;; ============================================================
;; Result Type
;; ============================================================

(struct interp-check-result (pass? pass-name before-result after-result message)
  #:transparent)

;; ============================================================
;; Pass Checkers
;; ============================================================

;; Check a single pass preserves semantics
;; pass-fn: function that transforms IR
;; interp-before: interpreter for input IR
;; interp-after: interpreter for output IR
;; pass-name: name for error messages
(define (check-pass pass-fn interp-before interp-after pass-name prog)
  (define before-result
    (with-handlers ([exn:fail? (lambda (e) (cons 'error (exn-message e)))])
      (interp-before prog)))
  (define output-prog
    (with-handlers ([exn:fail? (lambda (e) (cons 'error (exn-message e)))])
      (pass-fn prog)))
  (cond
    [(and (pair? before-result) (eq? (car before-result) 'error))
     (interp-check-result #f pass-name before-result #f
                          (~a "interp-before failed: " (cdr before-result)))]
    [(and (pair? output-prog) (eq? (car output-prog) 'error))
     (interp-check-result #f pass-name before-result #f
                          (~a "pass failed: " (cdr output-prog)))]
    [else
     (define after-result
       (with-handlers ([exn:fail? (lambda (e) (cons 'error (exn-message e)))])
         (interp-after output-prog)))
     (cond
       [(and (pair? after-result) (eq? (car after-result) 'error))
        (interp-check-result #f pass-name before-result after-result
                             (~a "interp-after failed: " (cdr after-result)))]
       [(equal? before-result after-result)
        (interp-check-result #t pass-name before-result after-result
                             "semantics preserved")]
       [else
        (interp-check-result #f pass-name before-result after-result
                             (~a "results differ: " before-result " vs " after-result))])]))

;; ============================================================
;; Pipeline Checker
;; ============================================================

;; Check all passes in the pipeline
(define (check-all-passes prog)
  (define results '())

  ;; uniquify: AST -> AST
  (define r1 (check-pass uniquify interp-ast interp-ast "uniquify" prog))
  (set! results (cons r1 results))

  (when (interp-check-result-pass? r1)
    (define prog1 (uniquify prog))

    ;; remove-complex: AST -> AST
    (define r2 (check-pass remove-complex-opera* interp-ast interp-ast
                           "remove-complex" prog1))
    (set! results (cons r2 results))

    (when (interp-check-result-pass? r2)
      (define prog2 (remove-complex-opera* prog1))

      ;; explicate-control: AST -> CProgram
      (define r3 (check-pass explicate-control interp-ast interp-cvar
                             "explicate-control" prog2))
      (set! results (cons r3 results))

      (when (interp-check-result-pass? r3)
        (define prog3 (explicate-control prog2))

        ;; select-instructions: CProgram -> X86Program
        (define r4 (check-pass select-instructions interp-cvar interp-x86
                               "select-instructions" prog3))
        (set! results (cons r4 results))

        (when (interp-check-result-pass? r4)
          (define prog4 (select-instructions prog3))

          ;; assign-homes: X86Program -> X86Program
          (define r5 (check-pass assign-homes interp-x86 interp-x86
                                 "assign-homes" prog4))
          (set! results (cons r5 results))

          (when (interp-check-result-pass? r5)
            (define prog5 (assign-homes prog4))

            ;; patch-instructions: X86Program -> X86Program
            (define r6 (check-pass patch-instructions interp-x86 interp-x86
                                   "patch-instructions" prog5))
            (set! results (cons r6 results))

            (when (interp-check-result-pass? r6)
              (define prog6 (patch-instructions prog5))

              ;; prelude-and-conclusion: X86Program -> X86Program
              (define r7 (check-pass prelude-and-conclusion interp-x86 interp-x86
                                     "prelude-and-conclusion" prog6))
              (set! results (cons r7 results))))))))

  (reverse results))

;; Convenience function to check entire pipeline
(define (check-pipeline prog)
  (define results (check-all-passes prog))
  (define failed (filter (lambda (r) (not (interp-check-result-pass? r))) results))
  (if (null? failed)
      (values #t results)
      (values #f results)))
