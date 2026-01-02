# Compiler 重构计划：适配 cutie-ftree 新 API

## 概述

cutie-ftree 库已重构：
- `ral` → `pvector`
- `ordl` → `ordered-map`
- 新增 `bitset` 模块
- 新增 `graph` 模块

compiler 有 55 个文件、327 处引用需要更新。

---

## 重构策略：兼容性封装

在 `lib/ftree.rkt` 中提供旧名称别名，最小化对业务代码的更改。

---

## 阶段 1: 更新 lib/ftree.rkt

**目标**：更新底层封装，提供旧 API 别名

**API 映射**：
| 旧名称 | 新名称 |
|--------|--------|
| `ral-empty` | `pvector-empty` |
| `ral-empty?` | `pvector-empty?` |
| `ral-length` | `pvector-length` |
| `ral-consl` | `pvector-cons-left` |
| `ral-consr` | `pvector-cons-right` |
| `ral-dropl` | `pvector-drop-left` |
| `ral-dropr` | `pvector-drop-right` |
| `ral-ref` | `pvector-ref` |
| `ral-set` | `pvector-set` |
| `ral-append` | `pvector-append` |
| `in-ral0` | `in-pvector` |
| `ordl-make-empty` | `ordered-map-empty` |
| `ordl-empty?` | `ordered-map-empty?` |
| `ordl-insert` | `ordered-map-set` |
| `ordl-query` | `ordered-map-query` |
| `ordl-delete` | `ordered-map-delete` |
| `Ordl` | `ordered-map` struct |
| `Ordl?` | `ordered-map?` |

**任务**：
1. 更新 require 路径
2. 添加旧名称别名 (define 或 define-syntax)
3. 更新工具函数实现

---

## 阶段 2: 更新 lib/bset.rkt

**选项 A**：直接使用 cutie-ftree/bitset.rkt
- 删除 lib/bset.rkt
- 从 ftree.rkt 导出 bitset API

**选项 B**：保持 bset 命名，提供别名
- `bset-*` → `bitset-*` 别名

**推荐**：选项 B（减少对现有代码的影响）

---

## 阶段 3: 更新 lib/graph.rkt

**当前状态**：基于 ordl 实现的简单图结构

**任务**：
1. 更新为使用 ordered-map 新 API
2. 考虑是否迁移到 cutie-ftree/graph.rkt（功能更强）

---

## 阶段 4: 验证与测试

1. 运行 compiler 测试套件
2. 检查 rvsdg 相关测试
3. 检查优化 pass 测试

---

## 文件影响分析

### 高频使用文件（>10 处引用）
- `lib/ftree.rkt` (35)
- `lib/graph.rkt` (35)
- `select-instructions.rkt` (32)
- `explicate-control.rkt` (23)

### 按目录分类
- `optim/cfg/` - 14 文件
- `transform/` - 7 文件
- `rvsdg/` - 5 文件
- 根目录 - 29 文件

---

## 执行顺序

1. [ ] 更新 `lib/ftree.rkt` - 添加兼容性别名
2. [ ] 更新 `lib/bset.rkt` - 添加 bitset 别名
3. [ ] 更新 `lib/graph.rkt` - 使用新 API
4. [ ] 运行测试验证
5. [ ] （可选）逐步将业务代码迁移到新 API 名称
