# CFG/RVSDG 优化移植清单

基于 GCC 优化技术的全面分析，规划待移植的优化 Pass。

## 已实现 ✅

| 优化 | 文件 | GCC 参考 | 测试数 |
|-----|------|---------|-------|
| 常量折叠 | `const-fold.rkt` | fold-const.cc | 10 |
| 死代码消除 | `dce.rkt` | tree-ssa-dce.cc | 9 |
| 复写传播 | `copy-prop.rkt` | tree-ssa-copy.cc | 7 |
| SCCP | `sccp.rkt` | tree-ssa-ccp.cc | 13 |
| GVN | `gvn.rkt` | tree-ssa-sccvn.cc | 10 |
| 循环分析 | `loop-analysis.rkt` | cfgloop.cc | 4 |
| LICM | `licm.rkt` | tree-ssa-loop-im.cc | 5 |
| 强度削减 | `strength-reduce.rkt` | tree-ssa-loop-ivopts.cc | 12 |
| 代数简化 | `strength-reduce.rkt` | fold-const.cc | (含上) |
| PHI 节点优化 | `phi-opt.rkt` | tree-ssa-phiopt.cc | 9 |
| 前向传播 | `forward-prop.rkt` | tree-ssa-forwprop.cc | 14 |
| 表达式重结合 | `reassoc.rkt` | tree-ssa-reassoc.cc | 12 |
| 循环展开 | `loop-unroll.rkt` | loop-unroll.cc | 10 |
| 循环规范化 | `loop-normalize.rkt` | tree-ssa-loop-ivcanon.cc | 10 |
| 归纳变量优化 | `ivopts.rkt` | tree-ssa-loop-ivopts.cc | 10 |
| 跳转线程化 | `jump-thread.rkt` | tree-ssa-threadedge.cc | 10 |
| If 表达式合并 | `if-combine.rkt` | tree-ssa-ifcombine.cc | 10 |
| 尾部合并 | `tail-merge.rkt` | tree-ssa-tail-merge.cc | 10 |
| 支配树优化 | `dom-opt.rkt` | tree-ssa-dom.cc | 10 |
| 别名分析 | `alias-analysis.rkt` | tree-ssa-alias.cc | 12 |
| 死存储消除 | `dse.rkt` | tree-ssa-dse.cc | 10 |
| PHI 传播 | `phi-prop.rkt` | tree-ssa-phiprop.cc | 10 |
| 部分冗余消除 | `pre.rkt` | tree-ssa-pre.cc | 12 |
| 循环分布 | `loop-distrib.rkt` | tree-loop-distribution.cc | 10 |
| 循环交换 | `loop-interchange.rkt` | gimple-loop-interchange.cc | 10 |
| 函数内联 | `inline.rkt` | tree-inline.cc | 10 |

**总计: 25 个优化 Pass, 249 个测试用例**

---

## 第一优先级 - 核心 SSA 优化 🔴

这些是 SSA 形式的核心优化，对代码质量影响最大。

### 1. 部分冗余消除 (PRE)
- **GCC 文件**: `tree-ssa-pre.cc`
- **复杂度**: Complex
- **描述**: 通过代码提升消除冗余计算，执行完全和部分冗余消除
- **适用性**: High - 表达式消除适用于 RVSDG
- **依赖**: GVN, 支配树

### 2. PHI 节点优化
- **GCC 文件**: `tree-ssa-phiopt.cc`
- **复杂度**: Medium
- **描述**: 将 PHI 节点转换为直线代码，替换基于 phi 的条件选择为更高效的模式
- **适用性**: High - SSA 关键优化
- **实现要点**:
  - `x = phi(a, a)` → `x = a`
  - `x = phi(c ? a : b)` → 条件移动
  - 识别 min/max/abs 模式

### 3. 前向传播
- **GCC 文件**: `tree-ssa-forwprop.cc`
- **复杂度**: Medium
- **描述**: 将单次使用变量的表达式向前传播，转换模式为更高效形式
- **适用性**: High - 表达式简化

### 4. 表达式重结合
- **GCC 文件**: `tree-ssa-reassoc.cc`
- **复杂度**: Medium
- **描述**: 重排结合/交换表达式的操作数，优化除法序列 (a/x → a*1/x)
- **适用性**: High - 表达式重构

### 5. 代码下沉
- **GCC 文件**: `tree-ssa-sink.cc`
- **复杂度**: Medium
- **描述**: 将语句移动到使用它们的块，减少寄存器压力
- **适用性**: Medium - 需要适配

---

## 第二优先级 - 循环优化 🟡

### 6. 归纳变量优化
- **GCC 文件**: `tree-ssa-loop-ivopts.cc`
- **复杂度**: Complex
- **描述**: 通过强度削减、合并和消除优化循环归纳变量
- **适用性**: High - 循环性能关键
- **当前状态**: 部分实现（基本强度削减）

### 7. 循环展开
- **GCC 文件**: `loop-unroll.cc`
- **复杂度**: Complex
- **描述**: 按常数因子展开最内层循环，减少退出条件测试
- **适用性**: High - 重要循环优化
- **实现要点**:
  - 完全展开小循环
  - 部分展开大循环
  - 展开因子选择

### 8. 循环规范化
- **GCC 文件**: `tree-ssa-loop-ivcanon.cc`
- **复杂度**: Medium
- **描述**: 检测常数迭代循环，添加规范归纳变量
- **适用性**: Medium

### 9. 循环头复制
- **GCC 文件**: `tree-ssa-loop-ch.cc`
- **复杂度**: Medium
- **描述**: 复制循环头以获得更好的优化机会
- **适用性**: Medium - CFG 转换

### 10. 循环条件外提 (Loop Unswitching)
- **GCC 文件**: `tree-ssa-loop-unswitch.cc`
- **复杂度**: Medium
- **描述**: 将循环不变条件的循环转换为多个循环
- **适用性**: Medium

---

## 第三优先级 - 控制流优化 🟢

### 11. 跳转线程化
- **GCC 文件**: `tree-ssa-threadedge.cc`, `tree-ssa-threadbackward.cc`
- **复杂度**: Medium
- **描述**: 检测并线程化 CFG 边中的冗余路径
- **适用性**: Medium - 需要 CFG 结构

### 12. If 表达式合并
- **GCC 文件**: `tree-ssa-ifcombine.cc`
- **复杂度**: Simple
- **描述**: 用逻辑运算合并相邻的 if 表达式，减少分支数
- **适用性**: Medium

### 13. 尾部合并
- **GCC 文件**: `tree-ssa-tail-merge.cc`
- **复杂度**: Medium
- **描述**: 合并 CFG 中相同的尾部块，减少代码重复
- **适用性**: Medium - CFG 结构依赖

### 14. 支配树优化
- **GCC 文件**: `tree-ssa-dom.cc`
- **复杂度**: Medium
- **描述**: 使用支配树进行常量传播和值范围分析
- **适用性**: Medium - 可适配

---

## 第四优先级 - 内存优化 🔵

### 15. 死存储消除 (DSE)
- **GCC 文件**: `tree-ssa-dse.cc`
- **复杂度**: Medium
- **描述**: 使用别名分析消除冗余和死内存存储
- **适用性**: Medium - 需要内存模型

### 16. 别名分析
- **GCC 文件**: `tree-ssa-alias.cc`, `tree-ssa-structalias.cc`
- **复杂度**: Complex
- **描述**: 指向分析的核心基础设施，确定指针引用的内存位置
- **适用性**: High - 内存操作必需

### 17. PHI 传播
- **GCC 文件**: `tree-ssa-phiprop.cc`
- **复杂度**: Medium
- **描述**: 通过 PHI 节点反向传播间接加载
- **适用性**: Medium

---

## 第五优先级 - 高级循环变换 ⚪

### 18. 循环分布
- **GCC 文件**: `tree-loop-distribution.cc`
- **复杂度**: Complex
- **描述**: 将循环语句分离为多个循环，启用部分向量化
- **适用性**: Medium

### 19. 循环交换
- **GCC 文件**: `gimple-loop-interchange.cc`
- **复杂度**: Complex
- **描述**: 重排嵌套循环维度以获得更好的缓存局部性
- **适用性**: Medium

### 20. 循环展开-合并
- **GCC 文件**: `gimple-loop-jam.cc`
- **复杂度**: Complex
- **描述**: 结合循环展开和融合进行缓存优化
- **适用性**: Medium

---

## 第六优先级 - 专用优化 ⬜

### 21. 数学优化
- **GCC 文件**: `tree-ssa-math-opts.cc`
- **复杂度**: Medium
- **描述**: CSE 倒数运算，数学库调用优化
- **适用性**: Medium

### 22. 分支预测
- **GCC 文件**: `predict.cc`
- **复杂度**: Medium
- **描述**: 预测分支结果用于性能分析引导优化
- **适用性**: High - 可指导所有 Pass

### 23. 内联优化
- **GCC 文件**: `tree-inline.cc`
- **复杂度**: Complex
- **描述**: 函数内联，减少调用开销
- **适用性**: High - 跨函数优化

---

## 暂不考虑 ❌

以下优化由于特定于架构或领域，暂不列入计划：

| 优化 | 原因 |
|-----|------|
| 向量化 (SLP, Loop) | SIMD 特定，需要后端支持 |
| RTL 级优化 | 寄存器分配阶段 |
| 自动增减 | 架构特定 |
| 硬件循环 | 架构特定 |
| 字符串优化 | C/C++ 特定 |
| 数组预取 | 架构特定内存优化 |

---

## 实现路线图

### Phase 1: 核心 SSA ✅ (完成)
1. ~~常量折叠~~ ✅
2. ~~DCE~~ ✅
3. ~~复写传播~~ ✅
4. ~~SCCP~~ ✅
5. ~~GVN~~ ✅
6. ~~PHI 优化~~ ✅
7. ~~前向传播~~ ✅
8. ~~表达式重结合~~ ✅

### Phase 2: 循环优化 ✅ (完成)
1. ~~LICM~~ ✅
2. ~~强度削减~~ ✅ (基础)
3. ~~归纳变量优化~~ ✅ (完整)
4. ~~循环展开~~ ✅ (完全展开)
5. ~~循环规范化~~ ✅

### Phase 3: 控制流 ✅ (完成)
1. ~~跳转线程化~~ ✅
2. ~~If 合并~~ ✅
3. ~~尾部合并~~ ✅
4. ~~支配树优化~~ ✅

### Phase 4: 内存 ✅ (完成)
1. ~~别名分析~~ ✅
2. ~~DSE~~ ✅
3. ~~PHI 传播~~ ✅

### Phase 5: 高级 ✅ (完成)
1. ~~PRE (部分冗余消除)~~ ✅
2. ~~循环分布~~ ✅
3. ~~循环交换~~ ✅
4. ~~函数内联~~ ✅

---

## 统计

**GCC 优化总数**: ~65+ 个 Pass

**按类别分布**:
- SSA 控制流: 28 个
- 循环优化: 3 个
- 向量化: 9 个
- 高级循环: 4 个
- 代码移动: 4 个
- RTL/架构特定: 6 个
- 基础设施: 8+ 个

**按复杂度分布**:
- Simple: 8 个 (10%)
- Medium: 38 个 (58%)
- Complex: 19 个 (29%)
- Very Complex: 2 个 (3%)

**按 CFG/RVSDG 适用性分布**:
- High (直接适用): 30 个 (46%)
- Medium (需适配): 25 个 (38%)
- Low (架构/领域特定): 10 个 (15%)

---

## 参考资源

- GCC 源码: `/Users/cutiedeng/blue-repo/gnu-gcc/gcc/`
- GCC 内部文档: https://gcc.gnu.org/onlinedocs/gccint/
- SSA 形式优化论文: Cytron et al., "Efficiently Computing Static Single Assignment Form"
- PRE 算法: Morel & Renvoise, "Global Optimization by Suppression of Partial Redundancies"
