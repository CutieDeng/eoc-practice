#lang racket/base

;; ============================================================
;; CFG Optimization Pipeline
;; ============================================================
;;
;; 组合多个 CFG 优化 pass，按最优顺序执行
;; ============================================================

(require "const-fold.rkt")
(require "copy-prop.rkt")
(require "sccp.rkt")
(require "gvn.rkt")
(require "licm.rkt")
(require "dce.rkt")
(require "../../cfg/raw.rkt")
(require "../../core/cfg.rkt")

;; ============================================================
;; 优化组合
;; ============================================================

;; 标准优化流水线：GVN + 复写传播 + SCCP + 常量折叠 + LICM + 死代码消除
(define (cfg-optimize cfg)
  (define cfg1 (cfg-gvn cfg))          ; 消除冗余计算
  (define cfg2 (cfg-copy-prop cfg1))   ; 传播复写
  (define cfg3 (cfg-sccp cfg2))        ; 稀疏条件常量传播
  (define cfg4 (cfg-const-fold cfg3))  ; 处理遗漏的简单情况
  (define cfg5 (cfg-licm cfg4))        ; 循环不变代码外提
  (define cfg6 (cfg-dce cfg5))         ; 删除死代码
  cfg6)

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
  (define cfg0 (cfg-gvn cfg))
  (define after-gvn (count-insns cfg0))
  (define cfg1 (cfg-copy-prop cfg0))
  (define after-copy-prop (count-insns cfg1))
  (define cfg2 (cfg-sccp cfg1))
  (define after-sccp (count-insns cfg2))
  (define cfg3 (cfg-const-fold cfg2))
  (define after-fold (count-insns cfg3))
  (define cfg4 (cfg-licm cfg3))
  (define after-licm (count-insns cfg4))
  (define cfg5 (cfg-dce cfg4))
  (define after-dce (count-insns cfg5))

  (values cfg5
          `((before . ,before)
            (after-gvn . ,after-gvn)
            (after-copy-prop . ,after-copy-prop)
            (after-sccp . ,after-sccp)
            (after-const-fold . ,after-fold)
            (after-licm . ,after-licm)
            (after-dce . ,after-dce)
            (total-removed . ,(- before after-dce)))))

(provide cfg-optimize-with-stats)
