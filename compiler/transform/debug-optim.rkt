#lang racket/base

;; ============================================================
;; 调试优化管道问题
;; ============================================================

(require racket/match racket/format racket/dict racket/list)
(require "../ftree.rkt")
(require "../core/cfg.rkt")
(require "../core/p-types.rkt")
(require "../cfg/raw.rkt")
(require "../optim/cfg/pipeline.rkt")
(require "../optim/cfg/const-fold.rkt")
(require "../optim/cfg/copy-prop.rkt")
(require "../optim/cfg/dce.rkt")
(require "../optim/cfg/gvn.rkt")
(require "../optim/cfg/sccp.rkt")
(require "l-to-cfg.rkt")
(require "test-e2e.rkt")

;; ============================================================
;; 打印 CFG 详细信息
;; ============================================================

(define (print-cfg cfg label)
  (printf "\n=== ~a ===\n" label)
  (printf "Entry: ~a\n" (cfg-get-entry cfg))
  (for ([bid (sort (cfg-all-block-ids cfg)
                   (lambda (a b) (< (BlockId-id a) (BlockId-id b))))])
    (define block (cfg-get-block cfg bid))
    (when block
      (printf "\nBlock ~a:\n" (BlockId-id bid))
      (for ([phi (CfgBlock-phis block)])
        (printf "  PHI: ~a <- ~a\n"
                (PhiInsn-output phi)
                (PhiInsn-sources phi)))
      (for ([insn (CfgBlock-insns block)])
        (printf "  ~a ~a -> ~a\n"
                (VfInsn-op insn)
                (VfInsn-inputs insn)
                (VfInsn-outputs insn)))
      (printf "  Term: ~a\n" (CfgBlock-terminator block)))))

;; ============================================================
;; 单步优化调试
;; ============================================================

(define (debug-optimize-step cfg pass-name pass-fn)
  (printf "\n>>> Applying ~a...\n" pass-name)
  (define cfg^ (pass-fn cfg))
  (define before-insns (count-cfg-insns cfg))
  (define after-insns (count-cfg-insns cfg^))
  (printf "    Instructions: ~a -> ~a\n" before-insns after-insns)
  cfg^)

(define (count-cfg-insns cfg)
  (for/sum ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if block (length (CfgBlock-insns block)) 0)))

;; 逐步优化
(define (debug-optimize cfg)
  (printf "\n======== Step-by-Step Optimization ========\n")

  ;; 跳过 inline，因为可能有问题
  ;; (define cfg0 (debug-optimize-step cfg "inline" cfg-inline))
  (define cfg0 cfg)

  (define cfg1 (debug-optimize-step cfg0 "gvn" cfg-gvn))
  (define cfg2 (debug-optimize-step cfg1 "copy-prop" cfg-copy-prop))
  (define cfg3 (debug-optimize-step cfg2 "sccp" cfg-sccp))
  (define cfg4 (debug-optimize-step cfg3 "const-fold" cfg-const-fold))
  (define cfg5 (debug-optimize-step cfg4 "dce" cfg-dce))

  cfg5)

;; ============================================================
;; 测试用例 1: nested-let
;; ============================================================

(define (test-nested-let)
  (displayln "\n")
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║           DEBUG: nested-let                              ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")

  ;; (let ([x 10])
  ;;   (let ([y (+ x 5)])    ; y = 15
  ;;     (* y 2)))           ; result = 30
  (define prog
    (Program (ordl-make-empty symbol-compare)
      (Let 0 (Int 10)
        (Let 1 (Prim '+ (list (Var 0) (Int 5)))
          (Prim '* (list (Var 1) (Int 2)))))))

  (define cfg (l-program->cfg prog))
  (print-cfg cfg "Original CFG")

  (printf "\nInterpreting original CFG...\n")
  (define result-before (interp-cfg cfg))
  (printf "Result: ~a\n" result-before)

  ;; 逐步优化
  (define cfg-opt (debug-optimize cfg))
  (print-cfg cfg-opt "Optimized CFG")

  (printf "\nInterpreting optimized CFG...\n")
  (define result-after (interp-cfg cfg-opt))
  (printf "Result: ~a\n" result-after)

  (printf "\nExpected: 30, Before: ~a, After: ~a\n" result-before result-after))

;; ============================================================
;; 测试用例 2: if-true
;; ============================================================

(define (test-if-true)
  (displayln "\n")
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║           DEBUG: if-true                                 ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")

  ;; (if (< 5 10) 1 2)
  (define prog
    (Program (ordl-make-empty symbol-compare)
      (If (Prim '< (list (Int 5) (Int 10)))
          (Int 1)
          (Int 2))))

  (define cfg (l-program->cfg prog))
  (print-cfg cfg "Original CFG")

  (printf "\nInterpreting original CFG...\n")
  (define result-before (interp-cfg cfg))
  (printf "Result: ~a\n" result-before)

  ;; 逐步优化
  (define cfg-opt (debug-optimize cfg))
  (print-cfg cfg-opt "Optimized CFG")

  (printf "\nInterpreting optimized CFG...\n")
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "Error: ~a\n" (exn-message e)))])
    (define result-after (interp-cfg cfg-opt))
    (printf "Result: ~a\n" result-after))

  (printf "\nExpected: 1, Before: ~a\n" result-before))

;; ============================================================
;; 测试用例 3: constant-folding
;; ============================================================

(define (test-constant-folding)
  (displayln "\n")
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║           DEBUG: constant-folding                        ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")

  ;; (let ([a 5])
  ;;   (let ([b 3])
  ;;     (let ([c (+ a b)])      ; c = 8
  ;;       (let ([d (* c 2)])    ; d = 16
  ;;         (+ d 4)))))         ; result = 20
  (define prog
    (Program (ordl-make-empty symbol-compare)
      (Let 0 (Int 5)
        (Let 1 (Int 3)
          (Let 2 (Prim '+ (list (Var 0) (Var 1)))
            (Let 3 (Prim '* (list (Var 2) (Int 2)))
              (Prim '+ (list (Var 3) (Int 4)))))))))

  (define cfg (l-program->cfg prog))
  (print-cfg cfg "Original CFG")

  (printf "\nInterpreting original CFG...\n")
  (define result-before (interp-cfg cfg))
  (printf "Result: ~a\n" result-before)

  ;; 逐步优化
  (define cfg-opt (debug-optimize cfg))
  (print-cfg cfg-opt "Optimized CFG")

  (printf "\nInterpreting optimized CFG...\n")
  (define result-after (interp-cfg cfg-opt))
  (printf "Result: ~a\n" result-after)

  (printf "\nExpected: 20, Before: ~a, After: ~a\n" result-before result-after))

;; ============================================================
;; 测试用例 4: if-false
;; ============================================================

(define (test-if-false)
  (displayln "\n")
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║           DEBUG: if-false                                ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")

  ;; (if (> 5 10) 1 2)  ; 5 > 10 = false, so result should be 2
  (define prog
    (Program (ordl-make-empty symbol-compare)
      (If (Prim '> (list (Int 5) (Int 10)))
          (Int 1)
          (Int 2))))

  (define cfg (l-program->cfg prog))
  (print-cfg cfg "Original CFG")

  (printf "\nInterpreting original CFG...\n")
  (define result-before (interp-cfg cfg))
  (printf "Result: ~a\n" result-before)

  ;; 逐步优化
  (define cfg-opt (debug-optimize cfg))
  (print-cfg cfg-opt "Optimized CFG")

  (printf "\nInterpreting optimized CFG...\n")
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "Error: ~a\n" (exn-message e)))])
    (define result-after (interp-cfg cfg-opt))
    (printf "Result: ~a\n" result-after))

  (printf "\nExpected: 2, Before: ~a\n" result-before))

;; ============================================================
;; 主入口
;; ============================================================

(module+ main
  (test-nested-let)
  (test-if-true)
  (test-if-false)
  (test-constant-folding))
