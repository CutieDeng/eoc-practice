#lang racket/base

;; ============================================================
;; Transform Module - Unified Entry Point
;; ============================================================
;;
;; 提供多种 IR 到 CFG 的统一转换接口
;;
;; 支持的转换：
;;   - L IR (高级函数式表达式) → CFG
;;   - C IR (低级控制流) → CFG
;;   - JVM IR (Java 字节码) → CFG
;;   - CFG → RVSDG (区域值状态依赖图)
;;
;; 使用方式：
;;   (require "compiler/transform/main.rkt")
;;   (define cfg (l-program->cfg my-l-program))
;;   (define cfg (c-program->cfg my-c-program))
;;   (define cfg (jvm-method->cfg my-jvm-method))
;; ============================================================

(require "l-to-cfg.rkt")
(require "c-to-cfg.rkt")
(require "jvm-to-cfg.rkt")
;; (require "cfg-to-rvsdg.rkt")  ;; 如果存在

;; ============================================================
;; L IR → CFG
;; ============================================================
;;
;; 将 L-language Program 直接转换为 CFG
;; 包含完整的控制流结构化：
;;   - If 条件 → 分支块 + 合并块 + PHI 节点
;;   - While 循环 → 头块 + 体块 + 出口块
;;   - Let 绑定 → 值计算 + 变量分配

(provide l-program->cfg)

;; ============================================================
;; C IR → CFG
;; ============================================================
;;
;; 将 CProgram (旧式基于 ral 的控制流图) 转换为新 CFG
;; 适用于已经通过 explicate-control 处理的程序

(provide c-program->cfg)

;; ============================================================
;; JVM IR → CFG
;; ============================================================
;;
;; 将 JVM 字节码方法转换为 CFG
;; 包含栈模拟，生成值流形式

(provide jvm-method->cfg)

;; ============================================================
;; 辅助转换
;; ============================================================

;; 转换后的 CFG 可以使用以下优化管道：
;;   (require "compiler/optim/cfg/pipeline.rkt")
;;   (cfg-optimize cfg)
;;   (cfg-optimize-fixpoint cfg)
;;   (cfg-optimize-with-stats cfg)

;; ============================================================
;; 版本信息
;; ============================================================

(define transform-version "1.0.0")
(define supported-sources '(l-ir c-ir jvm-ir))

(provide transform-version supported-sources)
