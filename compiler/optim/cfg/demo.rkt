#lang racket/base

;; ============================================================
;; CFG Optimization Interactive Demo
;; ============================================================
;;
;; 使用方法:
;;   $ racket
;;   > (enter! "compiler/optim/cfg/demo.rkt")
;;   > (demo-all)           ; 运行所有演示
;;   > (demo-const-fold)    ; 演示常量折叠
;;   > (demo-dce)           ; 演示死代码消除
;;   > (demo-sccp)          ; 演示稀疏条件常量传播
;;   > (demo-gvn)           ; 演示全局值编号
;;   > (demo-licm)          ; 演示循环不变代码外提
;;   > (demo-inline)        ; 演示函数内联
;;   > (demo-pipeline)      ; 演示完整优化流水线
;;
;; ============================================================

(require racket/match racket/list racket/format racket/string)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")

;; 导入所有优化 pass
(require "const-fold.rkt")
(require "copy-prop.rkt")
(require "sccp.rkt")
(require "gvn.rkt")
(require "licm.rkt")
(require "strength-reduce.rkt")
(require "phi-opt.rkt")
(require "forward-prop.rkt")
(require "reassoc.rkt")
(require "dce.rkt")
(require "dse.rkt")
(require "pre.rkt")
(require "inline.rkt")
(require "pipeline.rkt")

;; ============================================================
;; 辅助函数
;; ============================================================

;; 预定义的 BlockId 常量
(define BID0 (BlockId 0))  ; entry
(define BID1 (BlockId 1))
(define BID2 (BlockId 2))
(define BID3 (BlockId 3))
(define BID4 (BlockId 4))

;; 创建测试 CFG
(define (make-cfg entry blocks [exit #f])
  (define block-table
    (for/hash ([block blocks])
      (values (CfgBlock-id block) block)))
  (Cfg (hash-count block-table)
       100  ; var-cnt (预留足够空间)
       (for/sum ([block blocks]) (length (CfgBlock-insns block)))
       entry
       exit
       block-table
       (hash)))

(define (make-block id phis insns term)
  (CfgBlock id phis insns term))

;; 打印分隔线
(define (print-separator [char #\=] [width 60])
  (displayln (make-string width char)))

;; 打印标题
(define (print-title title)
  (newline)
  (print-separator)
  (displayln (~a "  " title))
  (print-separator))

;; 打印 CFG
(define (print-cfg cfg [label "CFG"])
  (printf "~a:\n" label)
  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (printf "  Block ~a:\n" bid)
      ;; PHIs
      (for ([phi (CfgBlock-phis block)])
        (printf "    PHI: ~a\n" phi))
      ;; Instructions
      (for ([insn (CfgBlock-insns block)])
        (printf "    ~a\n" (format-insn insn)))
      ;; Terminator
      (printf "    Term: ~a\n" (CfgBlock-terminator block))))
  (newline))

;; 格式化指令
(define (format-insn insn)
  (match insn
    [(VfInsn op inputs outputs info id)
     (define out-str
       (if (null? outputs)
           ""
           (format "~a = " (string-join (map ~a outputs) ", "))))
     (define in-str (string-join (map ~a inputs) ", "))
     (format "~a~a(~a)" out-str op in-str)]
    [_ (~a insn)]))

;; 统计指令数
(define (count-insns cfg)
  (for/sum ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if block (length (CfgBlock-insns block)) 0)))

;; ============================================================
;; 演示: 常量折叠
;; ============================================================

(define (demo-const-fold)
  (print-title "常量折叠 (Constant Folding)")
  (displayln "将编译期可计算的表达式替换为常量值")
  (displayln "例如: 3 + 5 → 8, 10 * 2 → 20")
  (newline)

  ;; 示例: x = 3 + 5; y = x * 2
  (define blocks
    (list
     (make-block BID0 '()
                 (list (VfInsn 'add '(3 5) (list (VarId 1)) #f #f)
                       (VfInsn 'mul (list (VarId 1) 2) (list (VarId 2)) #f #f)
                       (VfInsn 'sub '(100 40) (list (VarId 3)) #f #f))
                 (TermReturn (list (VarId 2) (VarId 3))))))

  (define cfg (make-cfg BID0 blocks))

  (displayln "源代码:")
  (displayln "  v1 = 3 + 5")
  (displayln "  v2 = v1 * 2")
  (displayln "  v3 = 100 - 40")
  (displayln "  return (v2, v3)")
  (newline)

  (print-cfg cfg "优化前")

  (define cfg^ (cfg-const-fold cfg))
  (print-cfg cfg^ "优化后")

  (printf "指令数: ~a → ~a\n" (count-insns cfg) (count-insns cfg^)))

;; ============================================================
;; 演示: 死代码消除
;; ============================================================

(define (demo-dce)
  (print-title "死代码消除 (Dead Code Elimination)")
  (displayln "删除结果未被使用的指令")
  (newline)

  ;; 示例: dead = 100 (未使用); live = 42
  (define blocks
    (list
     (make-block BID0 '()
                 (list (VfInsn 'const '(100) (list (VarId 1)) #f #f)       ; dead
                       (VfInsn 'const '(42) (list (VarId 2)) #f #f)        ; live
                       (VfInsn 'add (list (VarId 1) 1) (list (VarId 3)) #f #f)) ; also-dead
                 (TermReturn (list (VarId 2))))))

  (define cfg (make-cfg BID0 blocks))

  (displayln "源代码:")
  (displayln "  v1 = 100      ; dead (未使用)")
  (displayln "  v2 = 42       ; live")
  (displayln "  v3 = v1 + 1   ; also-dead")
  (displayln "  return v2")
  (newline)

  (print-cfg cfg "优化前")

  (define cfg^ (cfg-dce cfg))
  (print-cfg cfg^ "优化后")

  (printf "指令数: ~a → ~a (删除 ~a 条死代码)\n"
          (count-insns cfg) (count-insns cfg^)
          (- (count-insns cfg) (count-insns cfg^))))

;; ============================================================
;; 演示: 复写传播
;; ============================================================

(define (demo-copy-prop)
  (print-title "复写传播 (Copy Propagation)")
  (displayln "将 y = x; z = y + 1 替换为 z = x + 1")
  (newline)

  (define blocks
    (list
     (make-block BID0 '()
                 (list (VfInsn 'const '(10) (list (VarId 1)) #f #f)      ; x = 10
                       (VfInsn 'copy (list (VarId 1)) (list (VarId 2)) #f #f) ; y = x
                       (VfInsn 'add (list (VarId 2) 1) (list (VarId 3)) #f #f)) ; z = y + 1
                 (TermReturn (list (VarId 3))))))

  (define cfg (make-cfg BID0 blocks))

  (displayln "源代码:")
  (displayln "  v1 = 10       ; x")
  (displayln "  v2 = v1       ; y = x (复写)")
  (displayln "  v3 = v2 + 1   ; z = y + 1")
  (displayln "  return v3")
  (newline)

  (print-cfg cfg "优化前")

  (define cfg^ (cfg-copy-prop cfg))
  (print-cfg cfg^ "优化后 (v2 被替换为 v1)"))

;; ============================================================
;; 演示: SCCP
;; ============================================================

(define (demo-sccp)
  (print-title "稀疏条件常量传播 (SCCP)")
  (displayln "结合常量传播和死分支消除")
  (displayln "跟踪条件分支，只分析可达代码")
  (newline)

  (define blocks
    (list
     (make-block BID0 '()
                 (list (VfInsn 'const '(10) (list (VarId 1)) #f #f)    ; x = 10
                       (VfInsn 'const '(10) (list (VarId 2)) #f #f)    ; y = 10
                       (VfInsn 'eq (list (VarId 1) (VarId 2)) (list (VarId 3)) #f #f)) ; cond = x == y
                 (TermBranch (VarId 3) BID1 BID2))
     (make-block BID1 '()
                 (list (VfInsn 'const '(1) (list (VarId 4)) #f #f))
                 (TermReturn (list (VarId 4))))
     (make-block BID2 '()
                 (list (VfInsn 'const '(0) (list (VarId 5)) #f #f))
                 (TermReturn (list (VarId 5))))))

  (define cfg (make-cfg BID0 blocks))

  (displayln "源代码:")
  (displayln "  v1 = 10")
  (displayln "  v2 = 10")
  (displayln "  v3 = v1 == v2   ; 条件总是 true")
  (displayln "  if v3 then")
  (displayln "    return 1      ; 可达")
  (displayln "  else")
  (displayln "    return 0      ; 不可达")
  (newline)

  (print-cfg cfg "优化前")

  (define cfg^ (cfg-sccp cfg))
  (print-cfg cfg^ "优化后 (条件变为常量 #t)"))

;; ============================================================
;; 演示: GVN
;; ============================================================

(define (demo-gvn)
  (print-title "全局值编号 (GVN)")
  (displayln "识别并消除冗余计算")
  (displayln "例如: a = x + y; b = x + y → b = a")
  (newline)

  (define blocks
    (list
     (make-block BID0 '()
                 (list (VfInsn 'const '(5) (list (VarId 1)) #f #f)      ; x = 5
                       (VfInsn 'const '(3) (list (VarId 2)) #f #f)      ; y = 3
                       (VfInsn 'add (list (VarId 1) (VarId 2)) (list (VarId 3)) #f #f)  ; a = x + y
                       (VfInsn 'add (list (VarId 1) (VarId 2)) (list (VarId 4)) #f #f)  ; b = x + y (冗余)
                       (VfInsn 'mul (list (VarId 3) (VarId 4)) (list (VarId 5)) #f #f)) ; c = a * b
                 (TermReturn (list (VarId 5))))))

  (define cfg (make-cfg BID0 blocks))

  (displayln "源代码:")
  (displayln "  v1 = 5        ; x")
  (displayln "  v2 = 3        ; y")
  (displayln "  v3 = v1 + v2  ; a = x + y")
  (displayln "  v4 = v1 + v2  ; b = x + y (冗余!)")
  (displayln "  v5 = v3 * v4  ; c = a * b")
  (displayln "  return v5")
  (newline)

  (print-cfg cfg "优化前")

  (define cfg^ (cfg-gvn cfg))
  (print-cfg cfg^ "优化后 (v4 复用 v3 的值)"))

;; ============================================================
;; 演示: 强度削减
;; ============================================================

(define (demo-strength-reduce)
  (print-title "强度削减 (Strength Reduction)")
  (displayln "用更快的操作替代慢操作")
  (displayln "例如: x * 2 → x << 1, x * 8 → x << 3")
  (newline)

  (define blocks
    (list
     (make-block BID0 '()
                 (list (VfInsn 'const '(10) (list (VarId 1)) #f #f)         ; x = 10
                       (VfInsn 'mul (list (VarId 1) 2) (list (VarId 2)) #f #f)  ; a = x * 2
                       (VfInsn 'mul (list (VarId 1) 8) (list (VarId 3)) #f #f)  ; b = x * 8
                       (VfInsn 'div (list (VarId 1) 4) (list (VarId 4)) #f #f)) ; c = x / 4
                 (TermReturn (list (VarId 2) (VarId 3) (VarId 4))))))

  (define cfg (make-cfg BID0 blocks))

  (displayln "源代码:")
  (displayln "  v1 = 10")
  (displayln "  v2 = v1 * 2   ; 乘法 → 移位 1")
  (displayln "  v3 = v1 * 8   ; 乘法 → 移位 3")
  (displayln "  v4 = v1 / 4   ; 除法 → 移位 2")
  (displayln "  return (v2, v3, v4)")
  (newline)

  (print-cfg cfg "优化前")

  (define cfg^ (cfg-strength-reduce cfg))
  (print-cfg cfg^ "优化后 (乘除变为移位)"))

;; ============================================================
;; 演示: 表达式重结合
;; ============================================================

(define (demo-reassoc)
  (print-title "表达式重结合 (Reassociation)")
  (displayln "重排操作数以启用更多优化")
  (displayln "例如: (a + 1) + 2 → a + 3")
  (newline)

  (define blocks
    (list
     (make-block BID0 '()
                 (list (VfInsn 'const '(10) (list (VarId 1)) #f #f)         ; a = 10
                       (VfInsn 'add (list (VarId 1) 1) (list (VarId 2)) #f #f)   ; t1 = a + 1
                       (VfInsn 'add (list (VarId 2) 2) (list (VarId 3)) #f #f)   ; t2 = t1 + 2
                       (VfInsn 'add (list (VarId 3) 3) (list (VarId 4)) #f #f))  ; result = t2 + 3
                 (TermReturn (list (VarId 4))))))

  (define cfg (make-cfg BID0 blocks))

  (displayln "源代码:")
  (displayln "  v1 = 10")
  (displayln "  v2 = v1 + 1   ; ((a + 1)")
  (displayln "  v3 = v2 + 2   ;  + 2)")
  (displayln "  v4 = v3 + 3   ;  + 3 → a + 6")
  (displayln "  return v4")
  (newline)

  (print-cfg cfg "优化前")

  (define cfg^ (cfg-reassoc cfg))
  (print-cfg cfg^ "优化后 (常量被合并)"))

;; ============================================================
;; 演示: 函数内联
;; ============================================================

(define (demo-inline)
  (print-title "函数内联 (Function Inlining)")
  (displayln "将小函数的调用替换为函数体")
  (displayln "消除调用开销，启用更多优化机会")
  (newline)

  ;; 注册一个简单的被调用函数
  (define callee-blocks
    (list
     (make-block BID0 '()
                 (list (VfInsn 'add (list (VarId 100) (VarId 101)) (list (VarId 102)) #f #f))
                 (TermReturn (list (VarId 102))))))
  (define callee-cfg (make-cfg BID0 callee-blocks))
  (register-function 'add2 callee-cfg)

  ;; 调用者
  (define caller-blocks
    (list
     (make-block BID0 '()
                 (list (VfInsn 'const '(5) (list (VarId 1)) #f #f)      ; x = 5
                       (VfInsn 'const '(3) (list (VarId 2)) #f #f)      ; y = 3
                       (VfInsn 'call (list 'add2 (VarId 1) (VarId 2)) (list (VarId 3)) #f #f))
                 (TermReturn (list (VarId 3))))))

  (define cfg (make-cfg BID0 caller-blocks))

  (displayln "被调用函数 'add2(p0, p1)':")
  (displayln "  return p0 + p1")
  (newline)

  (displayln "调用者源代码:")
  (displayln "  v1 = 5")
  (displayln "  v2 = 3")
  (displayln "  v3 = add2(v1, v2)  ; 调用")
  (displayln "  return v3")
  (newline)

  (print-cfg cfg "调用者 (优化前)")

  (define cfg^ (cfg-inline cfg))
  (print-cfg cfg^ "调用者 (内联后)")

  (printf "指令数: ~a → ~a\n" (count-insns cfg) (count-insns cfg^)))

;; ============================================================
;; 演示: LICM
;; ============================================================

(define (demo-licm)
  (print-title "循环不变代码外提 (LICM)")
  (displayln "将循环内不变的计算移到循环外")
  (newline)

  ;; 简单循环: while (i < n) { t = a * b; i = i + t; }
  (define blocks
    (list
     (make-block BID0 '()
                 (list (VfInsn 'const '(0) (list (VarId 1)) #f #f)     ; i0 = 0
                       (VfInsn 'const '(10) (list (VarId 2)) #f #f)    ; n = 10
                       (VfInsn 'const '(2) (list (VarId 3)) #f #f)     ; a = 2
                       (VfInsn 'const '(3) (list (VarId 4)) #f #f))    ; b = 3
                 (TermJump BID1))
     (make-block BID1
                 (list (PhiInsn (VarId 5)                              ; i
                               (list (cons BID0 (VarId 1))
                                     (cons BID2 (VarId 8)))))
                 (list (VfInsn 'lt (list (VarId 5) (VarId 2)) (list (VarId 6)) #f #f)) ; cond = i < n
                 (TermBranch (VarId 6) BID2 BID3))
     (make-block BID2 '()
                 (list (VfInsn 'mul (list (VarId 3) (VarId 4)) (list (VarId 7)) #f #f) ; t = a * b (不变!)
                       (VfInsn 'add (list (VarId 5) (VarId 7)) (list (VarId 8)) #f #f)) ; i-next = i + t
                 (TermJump BID1))
     (make-block BID3 '()
                 '()
                 (TermReturn (list (VarId 5))))))

  (define cfg (make-cfg BID0 blocks))

  (displayln "源代码:")
  (displayln "  i = 0; n = 10; a = 2; b = 3")
  (displayln "  while (i < n) {")
  (displayln "    t = a * b    ; 循环不变!")
  (displayln "    i = i + t")
  (displayln "  }")
  (displayln "  return i")
  (newline)

  (print-cfg cfg "优化前 (a * b 在循环内)")

  (define cfg^ (cfg-licm cfg))
  (print-cfg cfg^ "优化后 (a * b 应被外提)"))

;; ============================================================
;; 演示: 完整流水线
;; ============================================================

(define (demo-pipeline)
  (print-title "完整优化流水线")
  (displayln "演示多个优化 pass 的组合效果")
  (newline)

  ;; 复杂示例
  (define blocks
    (list
     (make-block BID0 '()
                 (list
                  ;; 常量表达式
                  (VfInsn 'add '(10 20) (list (VarId 1)) #f #f)           ; c1 = 30
                  ;; 冗余计算
                  (VfInsn 'mul (list (VarId 1) 2) (list (VarId 2)) #f #f)  ; a = c1 * 2
                  (VfInsn 'mul (list (VarId 1) 2) (list (VarId 3)) #f #f)  ; b = c1 * 2 (冗余)
                  ;; 复写
                  (VfInsn 'copy (list (VarId 2)) (list (VarId 4)) #f #f)   ; a-copy = a
                  ;; 死代码
                  (VfInsn 'sub '(100 50) (list (VarId 5)) #f #f)           ; dead = 50
                  ;; 使用结果
                  (VfInsn 'add (list (VarId 4) (VarId 3)) (list (VarId 6)) #f #f)) ; result
                 (TermReturn (list (VarId 6))))))

  (define cfg (make-cfg BID0 blocks))

  (displayln "原始代码:")
  (displayln "  v1 = 10 + 20        ; 常量折叠 → 30")
  (displayln "  v2 = v1 * 2         ; a")
  (displayln "  v3 = v1 * 2         ; b (与 v2 冗余)")
  (displayln "  v4 = v2             ; 复写 (复写传播消除)")
  (displayln "  v5 = 100 - 50       ; dead (死代码消除)")
  (displayln "  v6 = v4 + v3        ; result")
  (displayln "  return v6")
  (newline)

  (print-cfg cfg "优化前")
  (printf "优化前指令数: ~a\n\n" (count-insns cfg))

  ;; 手动应用一系列优化
  (displayln "应用优化:")

  (displayln "1. 复写传播...")
  (define cfg1 (cfg-copy-prop cfg))
  (printf "   指令数: ~a\n" (count-insns cfg1))

  (displayln "2. 常量折叠...")
  (define cfg2 (cfg-const-fold cfg1))
  (printf "   指令数: ~a\n" (count-insns cfg2))

  (displayln "3. 死代码消除...")
  (define cfg3 (cfg-dce cfg2))
  (printf "   指令数: ~a\n" (count-insns cfg3))

  (newline)
  (print-cfg cfg3 "优化后")

  (printf "\n总计: ~a → ~a 指令 (删除 ~a 条)\n"
          (count-insns cfg)
          (count-insns cfg3)
          (- (count-insns cfg) (count-insns cfg3))))

;; ============================================================
;; 运行所有演示
;; ============================================================

(define (demo-all)
  (demo-const-fold)
  (demo-dce)
  (demo-copy-prop)
  (demo-sccp)
  (demo-gvn)
  (demo-strength-reduce)
  (demo-reassoc)
  (demo-inline)
  (demo-licm)
  (demo-pipeline))

;; ============================================================
;; 交互式帮助
;; ============================================================

(define (help)
  (displayln "")
  (displayln "CFG 优化演示命令:")
  (displayln "")
  (displayln "  (demo-all)             运行所有演示")
  (displayln "  (demo-const-fold)      常量折叠")
  (displayln "  (demo-dce)             死代码消除")
  (displayln "  (demo-copy-prop)       复写传播")
  (displayln "  (demo-sccp)            稀疏条件常量传播")
  (displayln "  (demo-gvn)             全局值编号")
  (displayln "  (demo-strength-reduce) 强度削减")
  (displayln "  (demo-reassoc)         表达式重结合")
  (displayln "  (demo-inline)          函数内联")
  (displayln "  (demo-licm)            循环不变代码外提")
  (displayln "  (demo-pipeline)        完整优化流水线")
  (displayln "")
  (displayln "  (help)                 显示此帮助")
  (displayln ""))

;; 导出所有演示函数
(provide demo-all
         demo-const-fold
         demo-dce
         demo-copy-prop
         demo-sccp
         demo-gvn
         demo-strength-reduce
         demo-reassoc
         demo-inline
         demo-licm
         demo-pipeline
         help
         ;; 辅助函数
         make-cfg
         make-block
         print-cfg
         count-insns)

;; 启动时显示帮助
(displayln "")
(displayln "╔════════════════════════════════════════════════════════════╗")
(displayln "║           CFG Optimization Interactive Demo                ║")
(displayln "║                   25 Optimization Passes                   ║")
(displayln "╚════════════════════════════════════════════════════════════╝")
(help)
