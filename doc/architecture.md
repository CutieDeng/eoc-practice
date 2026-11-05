# Architecture

Last Updated: 2026-11-05

适用范围: `src/`。

## 1. 设计目标

- 将“数据定义”和“算法实现”解耦。
- 将“通用算法”和“IR 绑定逻辑”解耦。
- 用 pipeline 把可复用 pass 组合成端到端编译流程。

## 2. 分层总览

```text
frontend -> pipeline -> component -> driver
                     \-> kernel <-/
```

- `kernel`: 基础数据结构和 IR 类型定义（尽量纯数据、低依赖）。
- `driver`: 参数化算法（图遍历、数据流、支配、循环、worklist）。
- `component`: 将 driver 算法绑定到具体 IR（CFG/RVSDG/AArch64 等）。
- `pipeline`: 按目标任务编排 pass，形成可执行编译链。
- `frontend`: 外部输入读取与初步转换（如 Java `.dat` -> JVM IR）。

## 3. 目录与职责

### `src/kernel/`

职责：
- 提供稳定的数据模型与类型表示。
- 不承载高层策略和业务流程。

典型内容：
- `ir/ast/*`, `ir/cfg/*`, `ir/rvsdg/*`, `ir/jvm/*`, `ir/x86/*`
- `data/*`：标准库持久结构的统一入口（`racket/pvector`、`racket/intmap`、`racket/intbits`、`racket/graph`、`racket/compare`）与 `idmap`（typed-id 键表，intmap 存储）

### `src/driver/`

职责：
- 提供可复用、参数化的算法，不绑定某个具体语言。

典型模块：
- `graph/`：遍历、SCC、可达性、拓扑
- `dataflow/`：框架与迭代策略
- `dominance/`：支配树/支配边界
- `loop/`：自然循环检测
- `worklist/`：工作队列策略

### `src/component/`

职责：
- 将 driver 算法与具体 IR 节点、边关系连接。
- 提供“可直接被 pipeline 调用”的分析/变换单元。

典型模块：
- `cfg/analysis/*`
- `rvsdg/*`
- `java/transform/*`
- `aarch64/*`
- `lvar/*`, `x86var/*`

### `src/pipeline/`

职责：
- 管理 pass 的执行顺序、依赖关系与组合输出。

典型模块：
- `x86/pipeline.rkt`
- `java/pipeline.rkt`
- `common/pass.rkt`

### `src/frontend/`

职责：
- 将外部输入转成内部可处理结构。

典型模块：
- `java/reader.rkt`（读取 `.dat` 并映射到 JVM IR）
- `java/java-tools/`（生成 `.dat` 的辅助工具）

## 4. 依赖方向（必须遵守）

允许：
- `pipeline -> component -> driver`
- `pipeline -> kernel`
- `component -> kernel`
- `frontend -> kernel/component/pipeline`

不建议/禁止：
- `driver -> component`（算法层反向依赖业务层）
- `kernel -> pipeline`（数据层依赖流程层）
- 跨层循环依赖

## 5. 数据流示例

### Java 链路（当前重点）

```text
ClassTransform.dat
  -> frontend/java/reader
  -> kernel jvm types
  -> component/java/transform (JVM -> CFG -> SSA -> RVSDG)
  -> pipeline/java
```

### x86 链路

```text
Lvar AST
  -> component/lvar passes
  -> component/x86var passes
  -> pipeline/x86
  -> x86 assembly text
```

## 6. 新功能放置规则

1. 先判断是否“纯数据定义”：
- 是 -> `kernel`

2. 再判断是否“可复用算法”：
- 是（与具体 IR 无关）-> `driver`

3. 若依赖具体 IR 节点结构：
- 放 `component/<ir-or-domain>/`

4. 若是编排执行顺序与产物形态：
- 放 `pipeline/`

5. 若是外部格式读取：
- 放 `frontend/`

## 7. 数据结构约定

- IR struct 一律 `#:prefab`，跨引用存 typed id（`BlockId`、`VarId`、`NodeId`…），整个 IR 可 `write`/`read`。
- typed-id 键表用 `idmap-*`；driver 层泛型节点用 `equal?` hash；symbol/string 键用 `hash`；裸整数用 `intmap`，位集合用 `intbits`。
- kernel `Cfg` 不存边：后继由 terminator 推导（`component/cfg/utils/graph-ops.rkt`）。

## 8. 变更检查清单

提交架构相关改动前，至少确认：

- 是否引入跨层反向依赖
- 新模块是否放在正确层级
- 是否补充对应层级测试（模块级 + 管线级）
- 文档是否同步更新（`doc/testing.md`/`doc/quickstart.md`）

## 9. 相关阅读

- [quickstart.md](quickstart.md)
- [README.md](README.md)
- [roadmap.md](roadmap.md)
