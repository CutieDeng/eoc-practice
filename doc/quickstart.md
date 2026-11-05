# Quickstart

Last Updated: 2026-11-05

适用范围: `src/`（根目录旧课程流水线已于 2026-11-05 删除）。

## 1. Prerequisites

- Racket 9.3.13+（标准库需含 `racket/pvector`、`racket/intmap`、`racket/intbits`、`racket/graph`、`racket/compare`；无第三方包）
- 可选: JDK（仅在需要重新生成 Java `.dat` 夹具时）

快速检查：

```bash
racket -v
raco -v
racket -e '(require racket/pvector racket/intmap racket/graph) (displayln "ok")'
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

## 3. 一次跑全部测试

```bash
make test            # = scripts/run-tests.sh
make test-java
make test-aarch64
make test-driver
```

## 4. 常见故障与处理

### 故障 1: `collection not found` / `cannot open module file racket/pvector`

原因：Racket 版本过旧，标准库没有 `racket/pvector` 等模块。升级到 9.3.13+。

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

## 5. 开发节奏建议

- 改 `src/component/java/transform/*`：至少跑 Java transform + pipeline tests。
- 改 `src/driver/*`：至少跑 `src/driver/graph/graph-test.rkt` 与相关组件测试。
- 改 AArch64：至少跑 `src/component/aarch64/*-test.rkt`。

## 6. 下一篇建议阅读

- [architecture.md](architecture.md)：分层边界与模块放置规则。
- [roadmap.md](roadmap.md)：重构与开发规划。
