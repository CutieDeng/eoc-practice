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
(require "dse.rkt")
(require "phi-prop.rkt")
(require "pre.rkt")
(require "loop-distrib.rkt")
(require "loop-interchange.rkt")
(require "inline.rkt")
(require "../../cfg/raw.rkt")
(require "../../core/cfg.rkt")

;; ============================================================
;; 优化组合
;; ============================================================

;; 标准优化流水线
(define (cfg-optimize cfg)
  (define cfg0 (cfg-inline cfg))           ; 函数内联（启用更多优化机会）
  (define cfg1 (cfg-gvn cfg0))             ; 消除冗余计算
  (define cfg2 (cfg-pre cfg1))             ; 部分冗余消除
  (define cfg3 (cfg-copy-prop cfg2))       ; 传播复写
  (define cfg4 (cfg-phi-opt cfg3))         ; 简化 PHI 节点
  (define cfg5 (cfg-phi-prop cfg4))        ; PHI 传播（间接加载）
  (define cfg6 (cfg-sccp cfg5))            ; 稀疏条件常量传播
  (define cfg7 (cfg-const-fold cfg6))      ; 处理遗漏的简单情况
  (define cfg8 (cfg-forward-prop cfg7))    ; 前向传播 (合并操作)
  (define cfg9 (cfg-reassoc cfg8))         ; 表达式重结合
  (define cfg10 (cfg-strength-reduce cfg9)) ; 强度削减
  (define cfg11 (cfg-algebraic-simplify cfg10)) ; 代数简化
  (define cfg12 (cfg-loop-normalize cfg11)) ; 循环规范化
  (define cfg13 (cfg-loop-distrib cfg12))  ; 循环分布
  (define cfg14 (cfg-loop-interchange cfg13)) ; 循环交换
  (define cfg15 (cfg-ivopts cfg14))        ; 归纳变量优化
  (define cfg16 (cfg-loop-unroll cfg15))   ; 循环展开（完全展开小循环）
  (define cfg17 (cfg-licm cfg16))          ; 循环不变代码外提
  (define cfg18 (cfg-jump-thread cfg17))   ; 跳转线程化
  (define cfg19 (cfg-if-combine cfg18))    ; If 表达式合并
  (define cfg20 (cfg-tail-merge cfg19))    ; 尾部合并
  (define cfg21 (cfg-dom-opt cfg20))       ; 支配树优化
  (define cfg22 (cfg-phi-opt cfg21))       ; 再次简化 PHI (LICM 可能创建新的)
  (define cfg23 (cfg-dse cfg22))           ; 死存储消除
  (define cfg24 (cfg-dce cfg23))           ; 删除死代码
  cfg24)

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
  (define cfg-inlined (cfg-inline cfg))
  (define after-inline (count-insns cfg-inlined))
  (define cfg0 (cfg-gvn cfg-inlined))
  (define after-gvn (count-insns cfg0))
  (define cfg1 (cfg-pre cfg0))
  (define after-pre (count-insns cfg1))
  (define cfg2 (cfg-copy-prop cfg1))
  (define after-copy-prop (count-insns cfg2))
  (define cfg3 (cfg-phi-opt cfg2))
  (define after-phi-opt-1 (count-phis cfg3))
  (define cfg4 (cfg-phi-prop cfg3))
  (define after-phi-prop (count-insns cfg4))
  (define cfg5 (cfg-sccp cfg4))
  (define after-sccp (count-insns cfg5))
  (define cfg6 (cfg-const-fold cfg5))
  (define after-fold (count-insns cfg6))
  (define cfg7 (cfg-forward-prop cfg6))
  (define after-forward-prop (count-insns cfg7))
  (define cfg8 (cfg-reassoc cfg7))
  (define after-reassoc (count-insns cfg8))
  (define cfg9 (cfg-strength-reduce cfg8))
  (define after-strength (count-insns cfg9))
  (define cfg10 (cfg-algebraic-simplify cfg9))
  (define after-algebra (count-insns cfg10))
  (define cfg11 (cfg-loop-normalize cfg10))
  (define after-loop-normalize (count-insns cfg11))
  (define cfg12 (cfg-ivopts cfg11))
  (define after-ivopts (count-insns cfg12))
  (define cfg13 (cfg-loop-unroll cfg12))
  (define after-loop-unroll (count-insns cfg13))
  (define cfg14 (cfg-licm cfg13))
  (define after-licm (count-insns cfg14))
  (define cfg15 (cfg-jump-thread cfg14))
  (define after-jump-thread (count-insns cfg15))
  (define cfg16 (cfg-if-combine cfg15))
  (define after-if-combine (count-insns cfg16))
  (define cfg17 (cfg-tail-merge cfg16))
  (define after-tail-merge (count-insns cfg17))
  (define cfg18 (cfg-dom-opt cfg17))
  (define after-dom-opt (count-insns cfg18))
  (define cfg19 (cfg-phi-opt cfg18))
  (define after-phi-opt-2 (count-phis cfg19))
  (define cfg20 (cfg-dse cfg19))
  (define after-dse (count-insns cfg20))
  (define cfg21 (cfg-dce cfg20))
  (define after-dce (count-insns cfg21))

  (values cfg21
          `((before . ,before)
            (after-inline . ,after-inline)
            (after-gvn . ,after-gvn)
            (after-pre . ,after-pre)
            (after-copy-prop . ,after-copy-prop)
            (phis-before . ,before-phis)
            (phis-after-phi-opt-1 . ,after-phi-opt-1)
            (after-phi-prop . ,after-phi-prop)
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
            (after-dse . ,after-dse)
            (after-dce . ,after-dce)
            (total-removed . ,(- before after-dce)))))

(provide cfg-optimize-with-stats)
