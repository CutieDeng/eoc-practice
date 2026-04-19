# Testing Guide

Last Updated: 2026-04-19

适用范围: 新架构（`src/`）为主，附带旧流水线测试说明。

## 1. 测试目标

- 快速验证改动未破坏核心行为。
- 将测试成本与改动范围匹配（避免每次全量跑）。
- 在提交前提供最小可复现的验证命令。

## 2. 测试分层

1. 模块级测试
- 目标: 验证单个模块行为。
- 形式: `raco test <module-test.rkt>`。

2. 子系统级测试
- 目标: 验证一条变换链路（如 Java transform 链）。
- 形式: 一次运行多个相关 `*-test.rkt`。

3. 管线级测试
- 目标: 验证端到端入口（如 `pipeline/java/pipeline-test.rkt`）。

4. 旧流水线测试（历史链路）
- 目标: 验证根目录课程模板链路。
- 入口: `make test` 或 `racket run-tests.rkt`。

## 3. 常用命令

### 基础健康检查

```bash
raco test src/driver/graph/graph-test.rkt
raco test src/component/aarch64/ir/types-test.rkt
raco test src/pipeline/java/pipeline-test.rkt
```

### Java 变换链路回归

```bash
raco test \
  src/component/java/transform/bbs-test.rkt \
  src/component/java/transform/jvm-to-cfg-test.rkt \
  src/component/java/transform/ssa-construct-test.rkt \
  src/component/java/transform/cfg-to-rvsdg-test.rkt \
  src/pipeline/java/pipeline-test.rkt
```

### AArch64 相关

```bash
raco test src/component/aarch64/ir/types-test.rkt
raco test src/component/aarch64/ir/cfg-test.rkt
raco test src/component/aarch64/backend/regalloc-test.rkt
```

## 4. 按改动范围选测试

### 改了 `src/driver/*`

至少运行：

```bash
raco test src/driver/graph/graph-test.rkt
```

并补充所有直接依赖该 driver 能力的 component 测试。

### 改了 `src/component/java/transform/*`

至少运行：

```bash
raco test \
  src/component/java/transform/bbs-test.rkt \
  src/component/java/transform/jvm-to-cfg-test.rkt \
  src/component/java/transform/ssa-construct-test.rkt \
  src/component/java/transform/cfg-to-rvsdg-test.rkt \
  src/pipeline/java/pipeline-test.rkt
```

### 改了 `src/component/aarch64/*`

至少运行：

```bash
raco test src/component/aarch64/ir/types-test.rkt
raco test src/component/aarch64/ir/cfg-test.rkt
raco test src/component/aarch64/backend/regalloc-test.rkt
```

### 改了 `src/pipeline/*`

至少运行对应 pipeline test + 上游关键 transform test。

## 5. 提交前最小验证模板

在 PR/提交说明中建议附上：

```text
Validation:
- raco test src/component/java/transform/cfg-to-rvsdg-test.rkt
- raco test src/pipeline/java/pipeline-test.rkt
```

规则：
- 命令应可复制执行。
- 至少覆盖你改动的直接模块与一个上层入口。

## 6. 失败排查

### 场景 1: 旧流水线 `make test` 失败

典型报错：
- `cannot open module file ... cutie-ftree ... main.rkt`

说明：
- 这是根目录旧链路依赖与 vendor collection 入口不一致问题。
- 不影响 `src/` 下多数 `raco test`。

处理：
- 优先跑 `src/` 测试作为主验证。
- 如需修复旧链路，统一 `cutie-ftree` require 方式或补入口文件。

### 场景 2: Java 测试缺少夹具 `.dat`

检查：

```bash
ls test/integration/ClassTransform.dat
```

缺失时重建：

```bash
racket src/frontend/java/java-tools/build.rkt --download
racket src/frontend/java/java-tools/build.rkt --compile
racket src/frontend/java/java-tools/build.rkt --analyze com/cutiedeng/ClassTransform --output test/integration/ClassTransform.dat
```

## 7. CI 建议（后续）

最小 CI 作业建议分 3 组并行：

1. Driver + 通用基础
- `src/driver/graph/graph-test.rkt`

2. Java 链路
- `src/component/java/transform/*-test.rkt`
- `src/pipeline/java/pipeline-test.rkt`

3. AArch64 链路
- `src/component/aarch64/ir/*-test.rkt`
- `src/component/aarch64/backend/regalloc-test.rkt`

## 8. 相关阅读

- [quickstart.md](/Users/cutiedeng/Y2026/M03/D28/compiler.rkt/doc/quickstart.md)
- [architecture.md](/Users/cutiedeng/Y2026/M03/D28/compiler.rkt/doc/architecture.md)
