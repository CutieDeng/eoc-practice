# compiler.rkt

Racket 实现的编译器实验与工程化项目。代码全部在 `src/` 下，按
kernel / driver / component / pipeline / frontend 分层；支持 Java 字节码
（JVM IR → CFG → SSA → RVSDG）、AArch64（IR、解释器、寄存器分配）与一条
x86 教学流水线。

## 环境要求

- Racket 9.3.13 或更高（标准库需提供 `racket/pvector`、`racket/intmap`、
  `racket/intbits`、`racket/graph`、`racket/compare`）。没有任何第三方包依赖。
- 可选：JDK（仅在重新生成 Java 前端夹具 `.dat` 时需要）。

## 快速开始

```bash
make test            # 全部单元测试 + aarch64 集成脚本
make test-java       # Java 变换链路
make test-aarch64    # AArch64
make test-driver     # 图算法 driver + idmap
```

单个模块：`raco test src/pipeline/java/pipeline-test.rkt`。

## 目录结构

```text
.
├── src/
│   ├── kernel/      # 纯数据定义：IR 与基础结构（data/：pvector、idmap、intmap、intbits、graph 的统一入口）
│   ├── driver/      # 参数化算法：图遍历、SCC、数据流、支配、循环
│   ├── component/   # 将 driver 算法绑定到具体 IR（cfg、rvsdg、java、aarch64、lvar、x86var）
│   ├── pipeline/    # 面向语言/目标的流水线组合
│   └── frontend/    # 外部输入读取（Java .dat → JVM IR；java-tools 生成 .dat）
├── test/integration/ # 集成夹具（ClassTransform.dat、aarch64/basic.rkt）
├── scripts/run-tests.sh
└── doc/             # 架构、测试、排障、路线图
```

## 数据结构约定

| 键类型 | 结构 |
|---|---|
| typed id（`BlockId`/`VarId`/`NodeId`/`WireId`…） | `idmap-*`（`src/kernel/data/idmap.rkt`，底层 intmap，按 id 升序迭代，可 `write`/`read`） |
| driver 层泛型节点 | 不可变 `equal?` hash |
| symbol / string（info 字典、label 表） | 不可变 `hash` |
| 裸整数 | `racket/intmap`；位集合用 `racket/intbits` |

所有 IR struct 均为 `#:prefab`，跨引用一律存 typed id，整个 IR 可序列化。

## Java 前端数据来源

Java 前端读取序列化后的 `.dat`（如 `test/integration/ClassTransform.dat`），由
`src/frontend/java/java-tools/` 生成：

```bash
racket src/frontend/java/java-tools/build.rkt --download
racket src/frontend/java/java-tools/build.rkt --compile
racket src/frontend/java/java-tools/build.rkt --analyze com/cutiedeng/ClassTransform --output test/integration/ClassTransform.dat
```

## 文档

- `doc/quickstart.md`、`doc/architecture.md`、`doc/testing.md`、`doc/troubleshooting.md`
- `doc/roadmap.md`：重构与开发规划（含决策记录）

## 许可

MIT License（见 `LICENSE`）。
