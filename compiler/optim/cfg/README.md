# CFG 优化模块使用指南

## 1. 运行优化测试

### 运行所有单元测试
```bash
# 在项目根目录执行
racket compiler/optim/cfg/test-const-fold.rkt      # 常量折叠 (10 tests)
racket compiler/optim/cfg/test-dce.rkt             # 死代码消除 (9 tests)
racket compiler/optim/cfg/test-copy-prop.rkt       # 复写传播 (7 tests)
racket compiler/optim/cfg/test-sccp.rkt            # SCCP (13 tests)
racket compiler/optim/cfg/test-gvn.rkt             # GVN (10 tests)
racket compiler/optim/cfg/test-licm.rkt            # LICM (9 tests)
racket compiler/optim/cfg/test-strength-reduce.rkt # 强度削减 (12 tests)
```

### 一次性运行所有测试
```bash
for f in compiler/optim/cfg/test-*.rkt; do echo "=== $f ===" && racket "$f"; done
```

### 运行合成测试（展示优化效果）
```bash
racket compiler/optim/cfg/test-synthetic.rkt
```

## 2. 查看 Java 代码转换结果

### 2.1 准备 Java 字节码

首先需要将 Java 类编译并转换为 `.dat` 格式：

```bash
# 进入 Java 工具目录
cd compiler/frontend/java/java-tools

# 构建工具（首次使用）
racket build.rkt

# 转换 Java 类为 .dat 文件
java -cp target:lib/* com.cutiedeng.ClassTransform <fully.qualified.ClassName>
```

### 2.2 使用 REPL 交互式查看

```bash
racket -i
```

```racket
;; 加载必要模块
(require "compiler/frontend/java/pipeline.rkt")
(require "compiler/optim/cfg/pipeline.rkt")
(require "compiler/core/cfg.rkt")
(require "compiler/cfg/raw.rkt")

;; 读取 Java 类
(define jvm-class (java-file->jvm-class "compiler/test/ClassTransform.dat"))

;; 查看类信息
(JvmClass-name jvm-class)         ; 类名
(length (JvmClass-methods jvm-class))  ; 方法数量

;; 获取方法列表
(define methods (JvmClass-methods jvm-class))
(for ([m methods])
  (printf "~a~a\n" (JvmMethod-name m) (JvmMethod-descriptor m)))

;; 转换单个方法为 CFG
(define method (car methods))
(define cfg (java-method->cfg method))

;; 查看 CFG 信息
(cfg-all-block-ids cfg)           ; 所有块 ID
(cfg-get-entry cfg)               ; 入口块

;; 查看块内容
(define entry-block (cfg-get-block cfg (cfg-get-entry cfg)))
(CfgBlock-insns entry-block)      ; 指令列表
(CfgBlock-terminator entry-block) ; 终止指令

;; 应用优化
(define cfg-opt (cfg-optimize cfg))

;; 对比优化前后
(define (count-insns c)
  (for/sum ([bid (cfg-all-block-ids c)])
    (define block (cfg-get-block c bid))
    (if block (length (CfgBlock-insns block)) 0)))

(printf "Before: ~a instructions\n" (count-insns cfg))
(printf "After: ~a instructions\n" (count-insns cfg-opt))
```

### 2.3 使用测试脚本

```bash
# 在真实 Java 代码上测试优化
racket compiler/optim/cfg/test-pipeline-java.rkt

# 运行合成测试展示各种优化
racket compiler/optim/cfg/test-synthetic.rkt
```

## 3. 优化流水线 API

### 主要函数

```racket
(require "compiler/optim/cfg/pipeline.rkt")

;; 标准优化流水线
(cfg-optimize cfg) → optimized-cfg

;; 迭代优化直到不动点
(cfg-optimize-fixpoint cfg [max-iterations]) → optimized-cfg

;; 带统计的优化
(cfg-optimize-with-stats cfg) → (values optimized-cfg stats-alist)
```

### 单独优化 Pass

```racket
(require "compiler/optim/cfg/const-fold.rkt")
(require "compiler/optim/cfg/dce.rkt")
(require "compiler/optim/cfg/copy-prop.rkt")
(require "compiler/optim/cfg/sccp.rkt")
(require "compiler/optim/cfg/gvn.rkt")
(require "compiler/optim/cfg/licm.rkt")
(require "compiler/optim/cfg/strength-reduce.rkt")

(cfg-const-fold cfg)        ; 常量折叠
(cfg-dce cfg)               ; 死代码消除
(cfg-copy-prop cfg)         ; 复写传播
(cfg-sccp cfg)              ; 稀疏条件常量传播
(cfg-gvn cfg)               ; 全局值编号
(cfg-licm cfg)              ; 循环不变代码外提
(cfg-strength-reduce cfg)   ; 强度削减
(cfg-algebraic-simplify cfg) ; 代数简化
```

## 4. 优化流水线顺序

```
输入 CFG
    ↓
1. GVN (全局值编号) - 消除冗余计算
    ↓
2. Copy Propagation (复写传播) - 传播变量复制
    ↓
3. SCCP (稀疏条件常量传播) - 高级常量传播
    ↓
4. Constant Folding (常量折叠) - 计算常量表达式
    ↓
5. Strength Reduction (强度削减) - 替换昂贵操作
    ↓
6. Algebraic Simplification (代数简化) - 代数恒等式
    ↓
7. LICM (循环不变代码外提) - 优化循环
    ↓
8. DCE (死代码消除) - 删除无用代码
    ↓
输出优化后的 CFG
```

## 5. 示例：手动构建和优化 CFG

```racket
#lang racket

(require "compiler/ftree.rkt")
(require "compiler/core/cfg.rkt")
(require "compiler/cfg/raw.rkt")
(require "compiler/optim/cfg/pipeline.rkt")

;; 创建一个简单的 CFG
(define cfg0 (cfg-empty))
(define-values (bid cfg1) (cfg-create-block cfg0))
(define cfg2 (cfg-set-entry cfg1 bid))

;; 添加指令: v0 = 10, v1 = 20, v2 = v0 + v1
(define v0 (VarId 0))
(define v1 (VarId 1))
(define v2 (VarId 2))

(define cfg3
  (cfg-block-append-insn cfg2 bid
    (VfInsn 'const '(10) (list v0) #f #f)))
(define cfg4
  (cfg-block-append-insn cfg3 bid
    (VfInsn 'const '(20) (list v1) #f #f)))
(define cfg5
  (cfg-block-append-insn cfg4 bid
    (VfInsn 'add (list v0 v1) (list v2) #f #f)))
(define cfg6
  (cfg-block-set-terminator cfg5 bid
    (TermReturn (list v2))))

;; 优化
(define-values (cfg-opt stats) (cfg-optimize-with-stats cfg6))

;; 查看结果
(for ([s stats])
  (printf "~a: ~a\n" (car s) (cdr s)))
```

## 6. 调试技巧

### 打印 CFG 结构
```racket
(define (print-cfg cfg)
  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (printf "Block ~a:\n" (BlockId-id bid))
    (for ([insn (CfgBlock-insns block)])
      (when (VfInsn? insn)
        (printf "  ~a ~a -> ~a\n"
                (VfInsn-op insn)
                (VfInsn-inputs insn)
                (VfInsn-outputs insn))))
    (printf "  terminator: ~a\n\n" (CfgBlock-terminator block))))
```

### 逐步查看优化效果
```racket
(define (trace-optimizations cfg)
  (define cfg1 (cfg-gvn cfg))
  (printf "After GVN: ~a insns\n" (count-insns cfg1))

  (define cfg2 (cfg-copy-prop cfg1))
  (printf "After copy-prop: ~a insns\n" (count-insns cfg2))

  (define cfg3 (cfg-sccp cfg2))
  (printf "After SCCP: ~a insns\n" (count-insns cfg3))

  ;; ... 继续其他优化
  cfg3)
```
