#lang racket/base

;; ============================================================
;; CFG Optimization Pipeline
;; ============================================================
;;
;; 组合多个 CFG 优化 pass，按最优顺序执行
;; ============================================================

(require "const-fold.rkt")
(require "copy-prop.rkt")
(require "dce.rkt")
(require "../../cfg/raw.rkt")
(require "../../core/cfg.rkt")

;; ============================================================
;; 优化组合
;; ============================================================

;; 标准优化流水线：复写传播 + 常量折叠 + 死代码消除
(define (cfg-optimize cfg)
  (define cfg1 (cfg-copy-prop cfg))
  (define cfg2 (cfg-const-fold cfg1))
  (define cfg3 (cfg-dce cfg2))
  cfg3)

;; 迭代优化直到不动点
(define (cfg-optimize-fixpoint cfg [max-iterations 10])
  (let loop ([cfg cfg] [i 0])
    (when (>= i max-iterations)
      (error 'cfg-optimize-fixpoint "Did not converge in ~a iterations" max-iterations))
    (define cfg^ (cfg-optimize cfg))
    (if (cfg-equal? cfg cfg^)
        cfg^
        (loop cfg^ (+ i 1)))))

;; 简单的 CFG 相等性检查（基于指令数量）
(define (cfg-equal? cfg1 cfg2)
  (equal? (cfg-insn-counts cfg1) (cfg-insn-counts cfg2)))

(define (cfg-insn-counts cfg)
  (for/list ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if block
        (length (CfgBlock-insns block))
        0)))

(provide cfg-optimize cfg-optimize-fixpoint)

;; ============================================================
;; 带统计的优化
;; ============================================================

(define (cfg-optimize-with-stats cfg)
  (define (count-insns c)
    (for/sum ([bid (cfg-all-block-ids c)])
      (define block (cfg-get-block c bid))
      (if block
          (length (CfgBlock-insns block))
          0)))

  (define before (count-insns cfg))
  (define cfg0 (cfg-copy-prop cfg))
  (define after-copy-prop (count-insns cfg0))
  (define cfg1 (cfg-const-fold cfg0))
  (define after-fold (count-insns cfg1))
  (define cfg2 (cfg-dce cfg1))
  (define after-dce (count-insns cfg2))

  (values cfg2
          `((before . ,before)
            (after-copy-prop . ,after-copy-prop)
            (after-const-fold . ,after-fold)
            (after-dce . ,after-dce)
            (total-removed . ,(- before after-dce)))))

(provide cfg-optimize-with-stats)
