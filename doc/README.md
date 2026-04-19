# Documentation Plan

本目录用于维护本项目的工程文档。目标是让新成员可以在 30 分钟内完成环境准备、跑通测试，并理解核心编译流水线。

## Scope

- 覆盖 `src/` 新架构（kernel/driver/component/pipeline）
- 标注根目录旧流水线与新架构的关系
- 明确测试、调试、扩展和发布流程

## Planned Docs

### P0 (先写，阻塞开发协作)

1. `doc/quickstart.md`
- 目标: 新同学 10-30 分钟内跑通核心测试
- 内容: 环境要求、安装、最小可执行命令、常见失败
- 验收: 在新机器按步骤能跑通至少 3 组测试

2. `doc/architecture.md`
- 目标: 解释新架构分层与数据流向
- 内容: kernel/driver/component/pipeline 职责、依赖方向、边界
- 验收: 新增模块时能判断应放在哪一层

3. `doc/testing.md`
- 目标: 统一测试入口与分层测试策略
- 内容: `raco test` 约定、模块级/集成级测试、CI 建议
- 验收: 开发者能按文档挑选正确测试集验证改动

4. `doc/troubleshooting.md`
- 目标: 快速定位常见环境与依赖问题
- 内容: `cutie-ftree` require 问题、runtime 构建失败、路径问题
- 验收: 常见报错可在 5 分钟内找到处置方式

### P1 (稳定迭代后完善)

5. `doc/pipeline-x86.md`
- 目标: 定义 x86 编译流水线输入/输出与 Pass 契约
- 内容: 各 Pass 语义、前后置条件、可观测调试点

6. `doc/pipeline-java.md`
- 目标: 说明 Java 字节码链路与当前支持范围
- 内容: JVM IR -> CFG -> SSA -> RVSDG、deferred 能力、夹具数据来源

7. `doc/aarch64.md`
- 目标: 汇总 AArch64 IR/解释器/寄存器分配能力
- 内容: IR 类型、解释执行、regalloc 约束与测试覆盖

8. `doc/ir-spec.md`
- 目标: 统一 IR 术语和结构定义
- 内容: AST/CFG/RVSDG/JVM/x86/AArch64 关键节点与不变式

### P2 (团队协作成熟后补齐)

9. `doc/contributing.md`
- 目标: 统一代码与文档协作流程
- 内容: 分支规范、提交规范、评审清单、文档更新要求

10. `doc/adr-template.md`
- 目标: 记录架构决策
- 内容: 背景/约束/方案对比/决策/影响

11. `doc/glossary.md`
- 目标: 统一术语，减少沟通成本
- 内容: 编译与项目内部术语对照表

## Suggested Writing Order

1. `quickstart.md`
2. `architecture.md`
3. `testing.md`
4. `troubleshooting.md`
5. `pipeline-java.md`
6. `pipeline-x86.md`
7. `aarch64.md`
8. `ir-spec.md`
9. `contributing.md`
10. `adr-template.md`
11. `glossary.md`

## Definition of Done (for each doc)

- 标注适用范围（旧流水线 / 新架构 / 两者）
- 至少包含一个可复制执行的命令块
- 至少包含一个“失败场景 -> 解决方式”
- 与当前代码结构一致（禁止描述不存在的入口）
- 文档顶部注明最后更新日期

## Next Step

先从 `doc/quickstart.md` 起草，包含“最小可运行路径 + 已知问题”。
