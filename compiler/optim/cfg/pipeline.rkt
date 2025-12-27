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
(require "strength-reduce.rkt")
(require "phi-opt.rkt")
(require "forward-prop.rkt")
(require "reassoc.rkt")
(require "loop-normalize.rkt")
(require "ivopts.rkt")
(require "loop-unroll.rkt")
(require "jump-thread.rkt")
(require "if-combine.rkt")
(require "tail-merge.rkt")
(require "dom-opt.rkt")
(require "dce.rkt")
(require "../../cfg/raw.rkt")
(require "../../core/cfg.rkt")

;; ============================================================
;; 优化组合
;; ============================================================

;; 标准优化流水线
(define (cfg-optimize cfg)
  (define cfg1 (cfg-gvn cfg))              ; 消除冗余计算
  (define cfg2 (cfg-copy-prop cfg1))       ; 传播复写
  (define cfg3 (cfg-phi-opt cfg2))         ; 简化 PHI 节点
  (define cfg4 (cfg-sccp cfg3))            ; 稀疏条件常量传播
  (define cfg5 (cfg-const-fold cfg4))      ; 处理遗漏的简单情况
  (define cfg6 (cfg-forward-prop cfg5))    ; 前向传播 (合并操作)
  (define cfg7 (cfg-reassoc cfg6))         ; 表达式重结合
  (define cfg8 (cfg-strength-reduce cfg7)) ; 强度削减
  (define cfg9 (cfg-algebraic-simplify cfg8)) ; 代数简化
  (define cfg10 (cfg-loop-normalize cfg9)) ; 循环规范化
  (define cfg11 (cfg-ivopts cfg10))        ; 归纳变量优化
  (define cfg12 (cfg-loop-unroll cfg11))   ; 循环展开（完全展开小循环）
  (define cfg13 (cfg-licm cfg12))          ; 循环不变代码外提
  (define cfg14 (cfg-jump-thread cfg13))   ; 跳转线程化
  (define cfg15 (cfg-if-combine cfg14))    ; If 表达式合并
  (define cfg16 (cfg-tail-merge cfg15))    ; 尾部合并
  (define cfg17 (cfg-dom-opt cfg16))       ; 支配树优化
  (define cfg18 (cfg-phi-opt cfg17))       ; 再次简化 PHI (LICM 可能创建新的)
  (define cfg19 (cfg-dce cfg18))           ; 删除死代码
  cfg19)

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

  (define (count-phis c)
    (for*/sum ([bid (cfg-all-block-ids c)]
               [block (in-value (cfg-get-block c bid))]
               #:when block)
      (length (CfgBlock-phis block))))

  (define before (count-insns cfg))
  (define before-phis (count-phis cfg))
  (define cfg0 (cfg-gvn cfg))
  (define after-gvn (count-insns cfg0))
  (define cfg1 (cfg-copy-prop cfg0))
  (define after-copy-prop (count-insns cfg1))
  (define cfg2 (cfg-phi-opt cfg1))
  (define after-phi-opt-1 (count-phis cfg2))
  (define cfg3 (cfg-sccp cfg2))
  (define after-sccp (count-insns cfg3))
  (define cfg4 (cfg-const-fold cfg3))
  (define after-fold (count-insns cfg4))
  (define cfg5 (cfg-forward-prop cfg4))
  (define after-forward-prop (count-insns cfg5))
  (define cfg6 (cfg-reassoc cfg5))
  (define after-reassoc (count-insns cfg6))
  (define cfg7 (cfg-strength-reduce cfg6))
  (define after-strength (count-insns cfg7))
  (define cfg8 (cfg-algebraic-simplify cfg7))
  (define after-algebra (count-insns cfg8))
  (define cfg9 (cfg-loop-normalize cfg8))
  (define after-loop-normalize (count-insns cfg9))
  (define cfg10 (cfg-ivopts cfg9))
  (define after-ivopts (count-insns cfg10))
  (define cfg11 (cfg-loop-unroll cfg10))
  (define after-loop-unroll (count-insns cfg11))
  (define cfg12 (cfg-licm cfg11))
  (define after-licm (count-insns cfg12))
  (define cfg13 (cfg-jump-thread cfg12))
  (define after-jump-thread (count-insns cfg13))
  (define cfg14 (cfg-if-combine cfg13))
  (define after-if-combine (count-insns cfg14))
  (define cfg15 (cfg-tail-merge cfg14))
  (define after-tail-merge (count-insns cfg15))
  (define cfg16 (cfg-dom-opt cfg15))
  (define after-dom-opt (count-insns cfg16))
  (define cfg17 (cfg-phi-opt cfg16))
  (define after-phi-opt-2 (count-phis cfg17))
  (define cfg18 (cfg-dce cfg17))
  (define after-dce (count-insns cfg18))

  (values cfg18
          `((before . ,before)
            (after-gvn . ,after-gvn)
            (after-copy-prop . ,after-copy-prop)
            (phis-before . ,before-phis)
            (phis-after-phi-opt-1 . ,after-phi-opt-1)
            (after-sccp . ,after-sccp)
            (after-const-fold . ,after-fold)
            (after-forward-prop . ,after-forward-prop)
            (after-reassoc . ,after-reassoc)
            (after-strength-reduce . ,after-strength)
            (after-algebraic-simplify . ,after-algebra)
            (after-loop-normalize . ,after-loop-normalize)
            (after-ivopts . ,after-ivopts)
            (after-loop-unroll . ,after-loop-unroll)
            (after-licm . ,after-licm)
            (after-jump-thread . ,after-jump-thread)
            (after-if-combine . ,after-if-combine)
            (after-tail-merge . ,after-tail-merge)
            (after-dom-opt . ,after-dom-opt)
            (phis-after-phi-opt-2 . ,after-phi-opt-2)
            (after-dce . ,after-dce)
            (total-removed . ,(- before after-dce)))))

(provide cfg-optimize-with-stats)
