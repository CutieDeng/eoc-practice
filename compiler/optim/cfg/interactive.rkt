#lang racket/base

;; ============================================================
;; 交互式优化流水线模块
;; ============================================================
;;
;; 使用方法：
;;   $ racket
;;   > (enter! "compiler/optim/cfg/interactive.rkt")
;;
;; 然后可以直接访问以下全局变量：
;;   jvm-class, methods, method, cfg, cfg-opt, stats
;;   以及各阶段的中间结果
;;
;; ============================================================

(require racket/list racket/format racket/match racket/string)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../core/jvm.rkt")
(require "../../cfg/raw.rkt")
(require "../../frontend/java/reader.rkt")
(require "../../transform/jvm-to-cfg.rkt")

;; 单独加载各优化 pass
(require "const-fold.rkt")
(require "copy-prop.rkt")
(require "sccp.rkt")
(require "gvn.rkt")
(require "licm.rkt")
(require "strength-reduce.rkt")
(require "dce.rkt")
(require "pipeline.rkt")

;; ============================================================
;; 全局变量 - 可直接在 REPL 中访问
;; ============================================================

;; Java 类数据
(define jvm-class #f)
(define methods '())
(define method #f)

;; CFG 数据
(define cfg #f)

;; 各优化阶段的结果
(define cfg-after-gvn #f)
(define cfg-after-copy-prop #f)
(define cfg-after-sccp #f)
(define cfg-after-const-fold #f)
(define cfg-after-strength #f)
(define cfg-after-algebra #f)
(define cfg-after-licm #f)
(define cfg-after-dce #f)

;; 最终优化结果
(define cfg-opt #f)
(define stats '())

;; ============================================================
;; 辅助函数
;; ============================================================

;; 统计指令数
(define (insn-count c)
  (if (not c) 0
      (for/sum ([bid (cfg-all-block-ids c)])
        (define block (cfg-get-block c bid))
        (if block (length (CfgBlock-insns block)) 0))))

;; 统计块数
(define (block-count c)
  (if (not c) 0 (length (cfg-all-block-ids c))))

;; 打印分隔线
(define (sep [char #\=] [width 60])
  (displayln (make-string width char)))

;; 打印 CFG 概览
(define (show-cfg c [label "CFG"])
  (if (not c)
      (printf "~a: (not loaded)\n" label)
      (printf "~a: ~a blocks, ~a instructions\n"
              label (block-count c) (insn-count c))))

;; 打印块详情
(define (show-block c bid-or-num)
  (define bid (if (BlockId? bid-or-num) bid-or-num (BlockId bid-or-num)))
  (define block (cfg-get-block c bid))
  (if (not block)
      (printf "Block ~a not found\n" (BlockId-id bid))
      (begin
        (printf "Block ~a:\n" (BlockId-id bid))
        (printf "  Phi nodes: ~a\n" (length (CfgBlock-phis block)))
        (printf "  Instructions:\n")
        (for ([insn (CfgBlock-insns block)])
          (when (VfInsn? insn)
            (printf "    ~a ~a -> ~a\n"
                    (VfInsn-op insn)
                    (VfInsn-inputs insn)
                    (VfInsn-outputs insn))))
        (printf "  Terminator: ~a\n"
                (match (CfgBlock-terminator block)
                  [(TermJump t) (format "jump ~a" (BlockId-id t))]
                  [(TermBranch c t e)
                   (format "branch ~a ? ~a : ~a" c (BlockId-id t) (BlockId-id e))]
                  [(TermReturn vs) (format "return ~a" vs)]
                  [(TermThrow e) (format "throw ~a" e)]
                  [(TermUnreachable) "unreachable"]
                  [other (format "~a" other)])))))

;; 打印所有块
(define (show-all-blocks c)
  (when c
    (for ([bid (cfg-all-block-ids c)])
      (show-block c bid)
      (newline))))

;; 打印方法列表
(define (show-methods)
  (if (null? methods)
      (displayln "No methods loaded. Use (load-java-class \"path/to/file.dat\")")
      (for ([(m i) (in-indexed methods)])
        (printf "~a: ~a~a\n" i (JvmMethod-name m) (JvmMethod-descriptor m)))))

;; 打印优化统计
(define (show-stats)
  (if (null? stats)
      (displayln "No stats. Run (optimize!) first.")
      (for ([s stats])
        (printf "~a: ~a\n" (car s) (cdr s)))))

;; 打印优化进度
(define (show-progress)
  (sep)
  (displayln "Optimization Progress:")
  (sep)
  (show-cfg cfg "Original")
  (show-cfg cfg-after-gvn "After GVN")
  (show-cfg cfg-after-copy-prop "After Copy Prop")
  (show-cfg cfg-after-sccp "After SCCP")
  (show-cfg cfg-after-const-fold "After Const Fold")
  (show-cfg cfg-after-strength "After Strength Reduce")
  (show-cfg cfg-after-algebra "After Algebraic Simplify")
  (show-cfg cfg-after-licm "After LICM")
  (show-cfg cfg-after-dce "After DCE (final)")
  (sep)
  (when (and cfg cfg-opt)
    (define before (insn-count cfg))
    (define after (insn-count cfg-opt))
    (printf "Total: ~a -> ~a instructions (~a% reduction)\n"
            before after
            (~r (* 100.0 (/ (- before after) (max before 1))) #:precision 1))))

;; ============================================================
;; 加载和处理函数
;; ============================================================

;; 加载 Java 类
(define (load-java-class dat-file)
  (set! jvm-class (read-jvm-class-file dat-file))
  (set! methods (JvmClass-methods jvm-class))
  (printf "Loaded class: ~a\n" (JvmClass-name jvm-class))
  (printf "Methods: ~a\n" (length methods))
  (show-methods))

;; 选择方法（按索引或名称）
(define (select-method idx-or-name)
  (define m
    (cond
      [(number? idx-or-name)
       (list-ref methods idx-or-name)]
      [(string? idx-or-name)
       (findf (λ (m) (string=? (JvmMethod-name m) idx-or-name)) methods)]
      [else #f]))
  (if m
      (begin
        (set! method m)
        (printf "Selected: ~a~a\n" (JvmMethod-name m) (JvmMethod-descriptor m))
        ;; 自动转换为 CFG
        (convert-to-cfg!))
      (displayln "Method not found")))

;; 转换当前方法为 CFG
(define (convert-to-cfg!)
  (if (not method)
      (displayln "No method selected. Use (select-method idx-or-name)")
      (begin
        (set! cfg (jvm-method->cfg method))
        (show-cfg cfg "Converted CFG")
        ;; 清除之前的优化结果
        (set! cfg-after-gvn #f)
        (set! cfg-after-copy-prop #f)
        (set! cfg-after-sccp #f)
        (set! cfg-after-const-fold #f)
        (set! cfg-after-strength #f)
        (set! cfg-after-algebra #f)
        (set! cfg-after-licm #f)
        (set! cfg-after-dce #f)
        (set! cfg-opt #f)
        (set! stats '()))))

;; 逐步执行优化
(define (optimize-step-by-step!)
  (when cfg
    (displayln "Running optimizations step by step...")
    (newline)

    (set! cfg-after-gvn (cfg-gvn cfg))
    (show-cfg cfg-after-gvn "1. After GVN")

    (set! cfg-after-copy-prop (cfg-copy-prop cfg-after-gvn))
    (show-cfg cfg-after-copy-prop "2. After Copy Prop")

    (set! cfg-after-sccp (cfg-sccp cfg-after-copy-prop))
    (show-cfg cfg-after-sccp "3. After SCCP")

    (set! cfg-after-const-fold (cfg-const-fold cfg-after-sccp))
    (show-cfg cfg-after-const-fold "4. After Const Fold")

    (set! cfg-after-strength (cfg-strength-reduce cfg-after-const-fold))
    (show-cfg cfg-after-strength "5. After Strength Reduce")

    (set! cfg-after-algebra (cfg-algebraic-simplify cfg-after-strength))
    (show-cfg cfg-after-algebra "6. After Algebraic Simplify")

    (set! cfg-after-licm (cfg-licm cfg-after-algebra))
    (show-cfg cfg-after-licm "7. After LICM")

    (set! cfg-after-dce (cfg-dce cfg-after-licm))
    (show-cfg cfg-after-dce "8. After DCE")

    (set! cfg-opt cfg-after-dce)
    (newline)
    (show-progress)))

;; 一次性执行所有优化
(define (optimize!)
  (when cfg
    (define-values (result st) (cfg-optimize-with-stats cfg))
    (set! cfg-opt result)
    (set! stats st)

    ;; 也填充中间变量
    (set! cfg-after-gvn (cfg-gvn cfg))
    (set! cfg-after-copy-prop (cfg-copy-prop cfg-after-gvn))
    (set! cfg-after-sccp (cfg-sccp cfg-after-copy-prop))
    (set! cfg-after-const-fold (cfg-const-fold cfg-after-sccp))
    (set! cfg-after-strength (cfg-strength-reduce cfg-after-const-fold))
    (set! cfg-after-algebra (cfg-algebraic-simplify cfg-after-strength))
    (set! cfg-after-licm (cfg-licm cfg-after-algebra))
    (set! cfg-after-dce cfg-opt)

    (show-progress)))

;; ============================================================
;; 合成测试用例
;; ============================================================

;; 创建测试 CFG
(define (make-test-cfg insns terminator)
  (define cfg0 (cfg-empty))
  (define-values (bid cfg1) (cfg-create-block cfg0))
  (define cfg2 (cfg-set-entry cfg1 bid))
  (define cfg3
    (for/fold ([c cfg2]) ([insn insns])
      (cfg-block-append-insn c bid insn)))
  (cfg-block-set-terminator cfg3 bid terminator))

;; 加载合成测试
(define (load-synthetic-test n)
  (set! method #f)
  (set! cfg
    (case n
      [(1) ;; 常量折叠
       (make-test-cfg
         (list
           (VfInsn 'const '(10) (list (VarId 0)) #f #f)
           (VfInsn 'const '(20) (list (VarId 1)) #f #f)
           (VfInsn 'add (list (VarId 0) (VarId 1)) (list (VarId 2)) #f #f)
           (VfInsn 'mul (list (VarId 2) 3) (list (VarId 3)) #f #f))
         (TermReturn (list (VarId 3))))]

      [(2) ;; 死代码消除
       (make-test-cfg
         (list
           (VfInsn 'const '(42) (list (VarId 0)) #f #f)
           (VfInsn 'const '(100) (list (VarId 1)) #f #f)
           (VfInsn 'const '(200) (list (VarId 2)) #f #f)
           (VfInsn 'add (list (VarId 1) (VarId 2)) (list (VarId 3)) #f #f))
         (TermReturn (list (VarId 0))))]

      [(3) ;; 强度削减
       (make-test-cfg
         (list
           (VfInsn 'const '(7) (list (VarId 0)) #f #f)
           (VfInsn 'mul (list (VarId 0) 8) (list (VarId 1)) #f #f)
           (VfInsn 'mul (list (VarId 0) 5) (list (VarId 2)) #f #f)
           (VfInsn 'urem (list (VarId 1) 16) (list (VarId 3)) #f #f))
         (TermReturn (list (VarId 3))))]

      [(4) ;; 代数简化
       (make-test-cfg
         (list
           (VfInsn 'const '(42) (list (VarId 0)) #f #f)
           (VfInsn 'sub (list (VarId 0) (VarId 0)) (list (VarId 1)) #f #f)
           (VfInsn 'xor (list (VarId 0) (VarId 0)) (list (VarId 2)) #f #f)
           (VfInsn 'and (list (VarId 0) 0) (list (VarId 3)) #f #f))
         (TermReturn (list (VarId 1) (VarId 2) (VarId 3))))]

      [(5) ;; 综合测试
       (make-test-cfg
         (list
           (VfInsn 'const '(2) (list (VarId 0)) #f #f)
           (VfInsn 'const '(3) (list (VarId 1)) #f #f)
           (VfInsn 'add (list (VarId 0) (VarId 1)) (list (VarId 2)) #f #f)
           (VfInsn 'mul (list (VarId 2) 4) (list (VarId 3)) #f #f)
           (VfInsn 'add (list (VarId 0) (VarId 1)) (list (VarId 4)) #f #f)
           (VfInsn 'const '(999) (list (VarId 5)) #f #f)
           (VfInsn 'mul (list (VarId 5) 8) (list (VarId 6)) #f #f)
           (VfInsn 'add (list (VarId 3) (VarId 4)) (list (VarId 7)) #f #f))
         (TermReturn (list (VarId 7))))]

      [else
       (displayln "Unknown test. Available: 1-5")
       #f]))

  (when cfg
    (printf "Loaded synthetic test ~a\n" n)
    (show-cfg cfg "Test CFG")
    ;; 清除优化结果
    (set! cfg-after-gvn #f)
    (set! cfg-after-copy-prop #f)
    (set! cfg-after-sccp #f)
    (set! cfg-after-const-fold #f)
    (set! cfg-after-strength #f)
    (set! cfg-after-algebra #f)
    (set! cfg-after-licm #f)
    (set! cfg-after-dce #f)
    (set! cfg-opt #f)
    (set! stats '())))

;; ============================================================
;; 帮助信息
;; ============================================================

(define (help)
  (displayln "
╔══════════════════════════════════════════════════════════════╗
║          CFG Optimization Interactive Module                  ║
╚══════════════════════════════════════════════════════════════╝

Commands:
  (help)                     - Show this help
  (load-java-class \"path\")  - Load a .dat file
  (show-methods)             - List all methods
  (select-method idx-or-name) - Select a method by index or name
  (convert-to-cfg!)          - Convert method to CFG

  (optimize!)                - Run full optimization pipeline
  (optimize-step-by-step!)   - Run optimizations one by one
  (show-progress)            - Show optimization progress
  (show-stats)               - Show optimization statistics

  (show-cfg cfg [label])     - Show CFG summary
  (show-block cfg bid)       - Show block details
  (show-all-blocks cfg)      - Show all blocks

  (load-synthetic-test n)    - Load synthetic test (1-5)

Global Variables:
  jvm-class                  - Loaded Java class
  methods                    - Method list
  method                     - Selected method
  cfg                        - Original CFG

  cfg-after-gvn              - After GVN
  cfg-after-copy-prop        - After Copy Propagation
  cfg-after-sccp             - After SCCP
  cfg-after-const-fold       - After Constant Folding
  cfg-after-strength         - After Strength Reduction
  cfg-after-algebra          - After Algebraic Simplification
  cfg-after-licm             - After LICM
  cfg-after-dce              - After DCE

  cfg-opt                    - Final optimized CFG
  stats                      - Optimization statistics

Example:
  (load-synthetic-test 5)
  (optimize-step-by-step!)
  (show-all-blocks cfg)
  (show-all-blocks cfg-opt)
"))

;; ============================================================
;; 启动信息
;; ============================================================

(displayln "
╔══════════════════════════════════════════════════════════════╗
║          CFG Optimization Interactive Module                  ║
╚══════════════════════════════════════════════════════════════╝

Type (help) for available commands.

Quick start:
  (load-synthetic-test 5)    ; Load a test case
  (optimize-step-by-step!)   ; Run optimizations
  (show-all-blocks cfg-opt)  ; View results
")
