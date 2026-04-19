# Quickstart

Last Updated: 2026-04-19

适用范围: 新架构（`src/`）为主，附带旧流水线入口说明。

## 1. Prerequisites

- Racket 8.x+
- `gcc` 或 `clang`
- 可选: JDK（仅在需要重新生成 Java `.dat` 夹具时）

快速检查：

```bash
racket -v
raco -v
gcc --version
```

## 2. 最小可运行路径（推荐）

在仓库根目录执行：

```bash
raco test src/pipeline/java/pipeline-test.rkt
raco test src/component/aarch64/ir/types-test.rkt
raco test src/driver/graph/graph-test.rkt
```

这三组测试覆盖：
- Java 管线串联（JVM IR -> CFG -> SSA -> RVSDG）
- AArch64 IR 基本类型
- 图算法驱动层

## 3. 一次跑核心测试集

```bash
raco test \
  src/component/java/transform/bbs-test.rkt \
  src/component/java/transform/jvm-to-cfg-test.rkt \
  src/component/java/transform/ssa-construct-test.rkt \
  src/component/java/transform/cfg-to-rvsdg-test.rkt \
  src/pipeline/java/pipeline-test.rkt \
  src/driver/graph/graph-test.rkt
```

## 4. 旧流水线入口（根目录）

```bash
make test
# 或
racket run-tests.rkt
```

说明：当前仓库状态下，这条链路默认不可直接通过（见下方故障 1）。

## 5. 常见故障与处理

### 故障 1: `make test` 报 `cannot open module file ... cutie-ftree ... main.rkt`

现象示例：

```text
cannot open module file
module path: cutie-ftree
path: .../vendor/cutie-ftree.rkt/main.rkt
```

原因：旧代码里使用 `(require cutie-ftree)`，但当前 vendor 目录没有 `main.rkt` 集合入口。

处理建议：
- 优先使用 `src/` 下的 `raco test ...` 流程开展开发。
- 若必须跑旧流水线，先统一 `cutie-ftree` 的 require 方式或补齐 collection 入口。

### 故障 2: Java 管线测试找不到 `ClassTransform.dat`

检查文件是否存在：

```bash
ls test/integration/ClassTransform.dat
```

若缺失，可重新生成（需 JDK）：

```bash
racket src/frontend/java/java-tools/build.rkt --download
racket src/frontend/java/java-tools/build.rkt --compile
racket src/frontend/java/java-tools/build.rkt --analyze com/cutiedeng/ClassTransform --output test/integration/ClassTransform.dat
```

## 6. 开发节奏建议

- 改 `src/component/java/transform/*`：至少跑 Java transform + pipeline tests。
- 改 `src/driver/*`：至少跑 `src/driver/graph/graph-test.rkt` 与相关组件测试。
- 改 AArch64：至少跑 `src/component/aarch64/*-test.rkt`。

## 7. 下一篇建议阅读

- `doc/architecture.md`（待创建）：理解分层边界与模块放置规则。
