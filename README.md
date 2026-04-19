# 编译器项目说明（候选 README）

本仓库是一个以 Racket 实现的编译器实验与工程化重构项目，当前同时包含两套代码：

1. 根目录的课程模板/旧流水线（`compiler.rkt`、`run-tests.rkt`、`tests/`）。
2. `src/` 下的新分层架构（kernel / driver / component / pipeline），支持 x86、AArch64 与 Java 字节码相关实验。

如果你是首次阅读本项目，建议优先从 `src/` 体系开始。

## 项目目标

- 实现并验证编译器常见中间表示（AST / CFG / RVSDG）及转换。
- 将通用算法（图、数据流、支配、循环）从具体 IR 中解耦。
- 支持多后端/多前端实验：
  - x86（EoC 风格流水线）
  - AArch64（IR、解释器、寄存器分配）
  - Java 字节码（JVM IR → CFG → SSA → RVSDG）

## 目录结构

```text
.
├── src/
│   ├── kernel/      # 纯数据定义：IR 与基础结构
│   ├── driver/      # 参数化算法：图/数据流/支配/循环/工作队列
│   ├── component/   # 将 driver 算法绑定到具体 IR
│   ├── pipeline/    # 面向语言/目标的流水线组合
│   └── frontend/    # 前端读取与转换（含 Java）
├── test/
│   └── integration/ # 集成夹具（如 ClassTransform.dat）
├── tests/           # 旧课程测试集（var/cond/while/vectors 等）
├── runtime.cc       # 运行时支持（旧流水线使用）
└── run-tests.rkt    # 旧流水线测试入口
```

## 环境要求

- Racket（建议 8.x 或以上）
- C/C++ 编译器（`gcc`/`clang`，用于 runtime）
- 可选：JDK（如需重新生成 Java 前端分析产物）

## 快速开始

### 1) 运行新架构测试（推荐）

以下命令在当前仓库可直接通过：

```bash
raco test src/pipeline/java/pipeline-test.rkt
raco test src/component/aarch64/ir/types-test.rkt
```

也可以一次跑一组核心测试：

```bash
raco test \
  src/component/java/transform/bbs-test.rkt \
  src/component/java/transform/jvm-to-cfg-test.rkt \
  src/component/java/transform/ssa-construct-test.rkt \
  src/component/java/transform/cfg-to-rvsdg-test.rkt \
  src/pipeline/java/pipeline-test.rkt \
  src/driver/graph/graph-test.rkt
```

### 2) 运行旧流水线测试（当前有已知问题）

```bash
make test
# 或
racket run-tests.rkt
```

当前仓库状态下，这条链路会因为 `cutie-ftree` 集合入口缺失而失败（见“已知问题”）。

## 当前实现进展（基于代码状态）

- Java 流水线：`src/pipeline/java/pipeline.rkt`
  - 支持 `jvm-method->cfg`、`jvm-cfg->ssa`、`cfg->rvsdg` 主链路。
  - 支持多块 if/while 与部分嵌套结构 lowering。
  - 对未覆盖控制流形态按方法粒度降级报错，不中断整类处理。

- x86 流水线：`src/pipeline/x86/pipeline.rkt`
  - 已串联 `uniquify → remove-complex → explicate-control → select-instructions → assign-homes → patch-instructions → prelude-conclusion → emit-x86`。

- AArch64：`src/component/aarch64/`
  - 包含 IR、解释器、活性分析与寄存器分配基础能力。
  - 对应单测较完整（`*-test.rkt`）。

## Java 前端数据来源

Java 前端读取的是序列化后的 `.dat`（例如 `test/integration/ClassTransform.dat`），由 `src/frontend/java/java-tools/` 生成。

常用脚本：

```bash
# 下载依赖（ASM）
racket src/frontend/java/java-tools/build.rkt --download

# 编译 Java 工具
racket src/frontend/java/java-tools/build.rkt --compile

# 分析某个 class 并输出 .dat
racket src/frontend/java/java-tools/build.rkt --analyze com/cutiedeng/ClassTransform --output /tmp/ClassTransform.dat
```

## 已知问题与排障

1. `make test` / `run-tests.rkt` 失败

- 现象：`cannot open module file ... vendor/cutie-ftree.rkt/main.rkt`。
- 原因：旧代码路径中有 `(require cutie-ftree)`，但当前 vendor 目录没有 `main.rkt` 作为 collection 入口。
- 影响：旧课程测试链路不可直接运行；`src/` 新架构测试不受此问题影响。

2. 仓库存在“旧模板 + 新架构”并存

- 如果你在做新功能开发，优先落在 `src/`。
- 若要继续维护根目录旧链路，建议先统一 `cutie-ftree` 的 require 方式或补齐 collection 入口。

## 开发建议

- 新功能优先按分层约束放置：
  - 数据定义放 `kernel`
  - 通用算法放 `driver`
  - IR 绑定放 `component`
  - 端到端流程放 `pipeline`
- 提交前至少运行对应模块测试（`raco test <module-test.rkt>`）。

## 许可

- 本项目根目录使用 MIT License（见 `LICENSE`）。
- `vendor/cutie-ftree.rkt` 含 Apache-2.0 / MIT 双许可证文件，请按其子项目许可要求使用。
