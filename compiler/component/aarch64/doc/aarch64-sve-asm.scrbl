#lang scribble/manual

@title{AArch64-SVE-ASM: 高性能汇编方言}

@author{rvsdg-compiler}

@section{概述}

@tt{aarch64-sve-asm} 是一种基于 S-expression 的 AArch64 汇编增强语言，
专为高性能向量计算设计。它提供：

@itemlist[
  @item{类型安全的寄存器访问}
  @item{SVE/SVE2 可伸缩向量扩展完整支持}
  @item{持久化数据结构的 CFG 表示}
  @item{内置解释器用于测试和验证}
  @item{可配置的编译流水线}
]

@section{快速开始}

@subsection{基本示例}

@codeblock|{
#lang racket/base

(require "compiler/component/aarch64/pipeline/main.rkt")

;; 定义一个简单的函数
(define my-fn
  '(asm-fn add-two
     ([x0 : i64] [x1 : i64])
     -> i64
     (add x0 x0 x1)
     (ret)))

;; 解析并验证
(define ir (parse-and-validate my-fn))

;; 使用解释器测试
(define result (interpret-function ir '(10 32)))
;; => 42
}|

@subsection{SVE 向量计算示例}

@codeblock|{
;; 向量加法：C[i] = A[i] + B[i]
(define vec-add
  '(asm-fn vector-add
     ([x0 : (ptr f32)]    ; A 指针
      [x1 : (ptr f32)]    ; B 指针
      [x2 : (ptr f32)]    ; C 指针
      [x3 : i64])         ; 长度
     -> void

     (block loop
       ;; 生成活跃谓词
       (whilelt p0 xzr x3)
       ;; 加载向量
       (ld1w z0 p0 [x0])
       (ld1w z1 p0 [x1])
       ;; 向量加法
       (fadd z2 p0 z0 z1)
       ;; 存储结果
       (st1w z2 p0 [x2])
       ;; 更新指针
       (incw x0)
       (incw x1)
       (incw x2)
       ;; 更新计数
       (decw x3)
       ;; 循环
       (b.gt loop))

     (block exit
       (ret))))
}|

@section{语法参考}

@subsection{函数定义}

函数使用 @tt{asm-fn} 形式定义：

@codeblock|{
(asm-fn <name>
  ([<reg> : <type>] ...)     ; 参数列表
  -> <return-type>           ; 返回类型
  <body> ...)                ; 函数体
}|

@subsection{寄存器类型}

@tabular[
  #:sep @hspace[2]
  (list
    (list @bold{寄存器} @bold{描述} @bold{示例})
    (list @tt{x0-x30} "64位通用寄存器" @tt{x0, x15, x30})
    (list @tt{w0-w30} "32位通用寄存器" @tt{w0, w10})
    (list @tt{sp} "栈指针" @tt{sp})
    (list @tt{xzr/wzr} "零寄存器" @tt{xzr, wzr})
    (list @tt{v0-v31} "NEON 向量寄存器" @tt{v0, v16})
    (list @tt{z0-z31} "SVE 可伸缩向量寄存器" @tt{z0, z31})
    (list @tt{p0-p15} "SVE 谓词寄存器" @tt{p0, p7})
  )
]

@subsection{数据类型}

@subsubsection{标量类型}

@codeblock|{
i8, i16, i32, i64    ; 有符号整数
u8, u16, u32, u64    ; 无符号整数
f16, f32, f64        ; 浮点数
}|

@subsubsection{向量类型}

@codeblock|{
(vec i32 4)          ; NEON 128位向量 (4x i32)
(vec f32 4)          ; NEON 128位浮点向量
(sve f32)            ; SVE 可伸缩 f32 向量
(sve2 i16)           ; SVE2 可伸缩 i16 向量
}|

@subsubsection{其他类型}

@codeblock|{
(ptr <type>)         ; 指针类型
pred                 ; SVE 谓词
void                 ; 无返回值
}|

@subsection{指令格式}

@subsubsection{算术指令}

@codeblock|{
(add <dst> <src1> <src2>)    ; 加法
(sub <dst> <src1> <src2>)    ; 减法
(mul <dst> <src1> <src2>)    ; 乘法
(and <dst> <src1> <src2>)    ; 按位与
(orr <dst> <src1> <src2>)    ; 按位或
(eor <dst> <src1> <src2>)    ; 按位异或
(neg <dst> <src>)            ; 取负
}|

@subsubsection{数据传送}

@codeblock|{
(mov <dst> <src>)            ; 寄存器/立即数传送
(ldr <dst> [<base>])         ; 加载
(ldr <dst> [<base> <off>])   ; 带偏移加载
(str <src> [<base>])         ; 存储
(str <src> [<base> <off>])   ; 带偏移存储
(ldp <d1> <d2> [<base>])     ; 加载对
(stp <s1> <s2> [<base>])     ; 存储对
}|

@subsubsection{比较与分支}

@codeblock|{
(cmp <src1> <src2>)          ; 比较
(b <label>)                  ; 无条件跳转
(b.eq <label>)               ; 等于时跳转
(b.ne <label>)               ; 不等时跳转
(b.lt <label>)               ; 小于时跳转
(b.gt <label>)               ; 大于时跳转
(csel <d> <s1> <s2> <cond>)  ; 条件选择
(ret)                        ; 返回
}|

@subsubsection{SVE 向量指令}

@codeblock|{
;; 算术
(fadd <zd> <pg> <zn> <zm>)   ; 向量加法
(fsub <zd> <pg> <zn> <zm>)   ; 向量减法
(fmul <zd> <pg> <zn> <zm>)   ; 向量乘法

;; 加载/存储
(ld1w <zd> <pg> [<base>])    ; 连续加载 word
(ld1d <zd> <pg> [<base>])    ; 连续加载 doubleword
(st1w <zd> <pg> [<base>])    ; 连续存储 word
(st1d <zd> <pg> [<base>])    ; 连续存储 doubleword

;; 谓词生成
(whilelt <pd> <rn> <rm>)     ; 循环谓词生成
(ptrue <pd>)                 ; 全真谓词
(pfalse <pd>)                ; 全假谓词

;; 比较
(cmpeq <pd> <pg> <zn> <zm>)  ; 向量相等比较
(cmplt <pd> <pg> <zn> <zm>)  ; 向量小于比较
}|

@subsection{基本块}

函数体可以是扁平指令列表或块结构：

@codeblock|{
;; 扁平结构 (自动创建单一块)
(asm-fn simple
  ([x0 : i64])
  -> i64
  (add x0 x0 1)
  (ret))

;; 块结构 (显式控制流)
(asm-fn with-blocks
  ([x0 : i64])
  -> i64
  (block entry
    (cmp x0 0)
    (b.eq zero-case))
  (block nonzero
    (mul x0 x0 2)
    (ret))
  (block zero-case
    (mov x0 1)
    (ret)))
}|

@section{编译流水线}

@subsection{流水线架构}

@verbatim{
┌─────────────┐
│  S-expr     │ ← 用户代码
└─────┬───────┘
      │ parse-asm-fn
      ▼
┌─────────────┐
│  AsmFunction│ ← IR 结构
└─────┬───────┘
      │ validate-function
      ▼
┌─────────────┐
│  验证结果   │ ← 语法检查
└─────┬───────┘
      │ type-check-function
      ▼
┌─────────────┐
│  类型检查   │ ← 类型兼容性
└─────┬───────┘
      │
      ├──────────────┐
      │              │
      ▼              ▼
┌─────────────┐ ┌─────────────┐
│  解释执行   │ │  代码生成   │
│ (测试验证)  │ │  (汇编输出) │
└─────────────┘ └─────────────┘
}

@subsection{使用流水线}

@codeblock|{
#lang racket/base

(require
  "compiler/component/aarch64/frontend/parser.rkt"
  "compiler/component/aarch64/frontend/validator.rkt"
  "compiler/component/aarch64/frontend/type-check.rkt"
  "compiler/component/aarch64/interp/base.rkt"
  "compiler/component/aarch64/interp/sve.rkt"
  "compiler/component/aarch64/ir/config.rkt")

;; 1. 解析
(define fn-sexp
  '(asm-fn my-func
     ([x0 : i64])
     -> i64
     (add x0 x0 1)
     (ret)))

(define fn-ir (parse-asm-fn fn-sexp))

;; 2. 验证
(define validation-result (validate-function fn-ir))
(when (Err? validation-result)
  (error "Validation failed:" (Err-errors validation-result)))

;; 3. 类型检查
(define type-result (type-check-function fn-ir))
(when (Err? type-result)
  (error "Type check failed:" (Err-errors type-result)))

;; 4. 解释执行 (测试)
(define interp (new interp-aarch64-base%))
(send interp write-x 0 41)  ; 设置参数
(define exec-result (send interp run-function fn-ir '(41)))
;; => 42
}|

@subsection{SVE 配置}

@codeblock|{
;; 便捷配置常量
config/base      ; 基础配置 (无 SVE)
config/sve       ; SVE 配置 (128-bit)
config/sve-256   ; SVE 配置 (256-bit)
config/sve-512   ; SVE 配置 (512-bit)
config/sve2      ; SVE2 配置

;; 或直接构造 (AsmConfig sve? sve2? vl features)
(define config (AsmConfig #t #f 256 '()))

;; 使用 SVE 解释器
(define sve-interp (make-sve-interp #:config config/sve-256))

;; 执行 SVE 代码
(send sve-interp run-function sve-fn args)
}|

@section{API 参考}

@subsection{解析器 (parser.rkt)}

@defproc[(parse-asm-fn [sexp any/c]) AsmFunction?]{
解析 S-expression 为 AsmFunction IR。
}

@defproc[(parse-asm-module [sexps (listof any/c)]) (listof AsmFunction?)]{
解析多个函数定义。
}

@defproc[(parse-reg [sym symbol?]) any-reg?]{
解析寄存器符号。
}

@defproc[(parse-type [sexp any/c]) asm-type?]{
解析类型注解。
}

@defproc[(parse-insn [sexp any/c]) insn?]{
解析单条指令。
}

@subsection{验证器 (validator.rkt)}

@defproc[(validate-function [fn AsmFunction?]) (or/c Ok? Err?)]{
验证函数的语法正确性。返回 @tt{Ok} 或包含错误列表的 @tt{Err}。
}

@defproc[(validate-insn [insn insn?] [config AsmConfig?]) (listof ValidationError?)]{
验证单条指令。
}

@subsection{类型检查器 (type-check.rkt)}

@defproc[(type-check-function [fn AsmFunction?]) (or/c Ok? Err?)]{
检查函数的类型兼容性。
}

@defproc[(register-type-compatible? [reg any-reg?] [type asm-type?]) boolean?]{
检查寄存器与类型是否兼容。
}

@subsection{解释器 (base.rkt, sve.rkt)}

@defclass[interp-aarch64-base% object% ()]{
基础 AArch64 解释器类。

@defmethod[(exec-insn [insn insn?]) void?]{
执行单条指令。
}

@defmethod[(run-cfg [cfg AsmCfg?]) InterpResult?]{
执行 CFG 直到返回或超时。
}

@defmethod[(read-x [id exact-nonnegative-integer?]) exact-integer?]{
读取 X 寄存器。
}

@defmethod[(write-x [id exact-nonnegative-integer?] [val exact-integer?]) void?]{
写入 X 寄存器。
}
}

@defclass[interp-aarch64-sve% interp-aarch64-base% ()]{
SVE 扩展解释器，支持 SVE 向量指令。
}

@defproc[(make-sve-interp [#:config config (or/c AsmConfig? #f) #f]) (is-a?/c interp-aarch64-sve%)]{
创建 SVE 解释器实例。
}

@section{完整示例}

@subsection{SAXPY: y = a*x + y}

@codeblock|{
(define saxpy
  '(asm-fn saxpy
     ([x0 : (ptr f32)]    ; x 向量指针
      [x1 : (ptr f32)]    ; y 向量指针
      [x2 : i64]          ; 长度
      [z0 : (sve f32)])   ; a (广播标量)
     -> void

     (block loop
       ;; 生成循环谓词
       (whilelt p0 xzr x2)
       (b.none exit)

       ;; 加载向量
       (ld1w z1 p0 [x0])   ; z1 = x[i:i+vl]
       (ld1w z2 p0 [x1])   ; z2 = y[i:i+vl]

       ;; z2 = a * z1 + z2
       (fmla z2 p0 z0 z1)

       ;; 存储结果
       (st1w z2 p0 [x1])

       ;; 更新指针和计数
       (incw x0)
       (incw x1)
       (decw x2)
       (b loop))

     (block exit
       (ret))))

;; 使用
(require "compiler/component/aarch64/pipeline/main.rkt")

(define config config/sve-512)
(define result (compile-and-run saxpy
                  #:args (list x-ptr y-ptr n a-scalar)
                  #:config config))
}|

@subsection{矩阵乘法核心}

@codeblock|{
(define matmul-kernel
  '(asm-fn matmul-kernel
     ([x0 : (ptr f32)]    ; A 矩阵
      [x1 : (ptr f32)]    ; B 矩阵
      [x2 : (ptr f32)]    ; C 矩阵
      [x3 : i64]          ; M
      [x4 : i64]          ; N
      [x5 : i64])         ; K
     -> void

     ;; 使用 SVE 进行分块矩阵乘法
     ;; 每次处理 VL 个元素
     ...))
}|

@section{错误处理}

@subsection{解析错误}

@codeblock|{
;; ParseError 包含位置和描述
(struct ParseError (message context) #:prefab)

;; 处理解析错误
(with-handlers ([ParseError?
                 (lambda (e)
                   (printf "Parse error: ~a\n" (ParseError-message e)))])
  (parse-asm-fn bad-sexp))
}|

@subsection{验证错误}

@codeblock|{
;; 验证结果是 Ok 或 Err
(match (validate-function fn)
  [(Ok fn) (displayln "Valid!")]
  [(Err errors)
   (for ([e errors])
     (printf "Error: ~a\n" e))])
}|

@section{性能注意事项}

@itemlist[
  @item{使用 @tt{pvector} 存储指令序列，支持 O(log n) 的随机访问和追加}
  @item{使用 @tt{ordered-map} 存储基本块，保证确定性遍历顺序}
  @item{解释器使用类继承实现 SVE 扩展，便于性能分析和调试}
  @item{生产代码应使用代码生成后端而非解释器}
]

@section{扩展开发}

@subsection{添加新指令}

1. 在 @tt{ir/types.rkt} 添加指令结构
2. 在 @tt{ir/sve-insns.rkt} 添加到相应类别
3. 在 @tt{frontend/parser.rkt} 添加解析规则
4. 在 @tt{interp/base.rkt} 或 @tt{interp/sve.rkt} 添加执行逻辑
5. 添加单元测试

@subsection{添加新后端}

1. 在 @tt{codegen/} 创建新的生成器类
2. 继承 @tt{asm-generator%} 基类
3. 实现 @tt{emit-insn} 方法
4. 在流水线中注册

@section{参考资料}

@itemlist[
  @item{ARM Architecture Reference Manual for A-profile architecture}
  @item{ARM Scalable Vector Extension (SVE) Programmer's Guide}
  @item{Racket Documentation: @url{https://docs.racket-lang.org}}
]
