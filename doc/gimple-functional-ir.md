# GIMPLE 层的持久化改写与泛化算法（决策 13）

状态：设计定稿 2026-11-06，实施中（阶段表见末尾）。

## 0. 决策

1. GENERIC/GIMPLE/GIMPLE-CFG 的数据全部是持久化值：prefab struct + pvector + intmap/intset + 不可变 hasheq。没有 box、没有可变 hash、没有 `set!`、没有承载状态的 parameter。
2. 算法纯函数式：所有会"改状态"的函数显式接收状态 `st` 作为第一个参数、返回新状态作为最后一个值。GCC 的原地修改改写为返回新值；`tree *expr_p` 改写为"返回新表达式，由调用方写回父节点"。
3. 与 GCC 的对应关系保留在函数粒度：函数名、分支顺序、注释里的 gcc 源码行号不变；逐行对应不再保证。
4. 新代码放在新目录（`src/kernel/ir/gimple/`、`src/component/gimple/`），每个阶段用现有的 dump 对齐测试验收后，再删除 `src/component/gccast/` 里对应的可变版本。
5. SSA 构造与 CFG → RVSDG 在新表示定稿后按接口泛化（第 6 节），Java 链路与 GIMPLE 链路共用同一份算法。

## 1. 为什么现在改

- 可变版本是移植期的权宜之计（见 roadmap 决策 1：prefab + typed id 是既定方向）。
- 22 个 dump 对齐测试（`.gimple` 8、`.lower` 8、`.cfg` 6）已经就位，改写有完整的回归网。
- 4 408 行里直接访问 `T` 字段的只有 2 处，其余全部经由访问器；身份依赖（`eq?`/`hasheq`）集中在 gimplify（80 处）与 gimple-to-cfg（30 处），可逐条替换。

## 2. 数据定义（`src/kernel/ir/gimple/types.rkt`，全部 prefab）

### 2.1 树

```
#s(Tree code type loc ops attrs)
  code  : 符号，GCC tree code 名（'plus_expr）
  type  : 类型 uid（整数）或 #f
  loc   : (list file line col) 或 #f
  ops   : 操作数 pvector（元素是 Tree）
  attrs : 不可变 hasheq（side-effects、constant、volatile、uid、name、value、elts、stmts、vars ...）
```

三类节点共用这一个 struct，但身份规则不同：

| 类别 | 存放 | 身份 | 可变部分 |
|---|---|---|---|
| 表达式、常量、STATEMENT_LIST、BIND_EXPR | 父节点的 `ops`/`attrs` 里的值 | 无（值语义，重建脊柱） | 无；"修改"=返回新值 |
| 声明（VAR/PARM/RESULT/LABEL/FUNCTION/FIELD_DECL） | 值，嵌在使用处 | `uid` 整数，`decl-eq?` 比较 uid | 可变位在状态表 `decl-flags`（2.3） |
| 类型 | 状态表 `types : intmap uid → Tree` | uid | 只增不改（`pointer-to` 新建） |

声明的 `type` 字段与所有节点一样是类型 uid。类型必须进表，因为类型图有环（`struct P { struct P *next; }`）。声明可以是值，因为它只引用类型（uid）和常量初值。

表达式的共享（同一节点被两个父节点引用）在 C 子集的 PRE_GENERICIZE 输出中只出现于 SAVE_EXPR/TARGET_EXPR；导入时按 TreeId 检测共享并报错，待出现时再决定表示（候选：把共享节点提升为带 uid 的声明类）。

### 2.2 语句与序列

```
#s(gassign  loc flags lhs code rhs1 rhs2 rhs3)
#s(gcall    loc flags lhs fn args fntype internal? ecf)
#s(gcond    loc flags code lhs rhs true-label false-label)
#s(glabel   loc flags label)
#s(ggoto    loc flags dest)
#s(greturn  loc flags retval)
#s(gswitch  loc flags index default-case cases)
#s(gbind    loc flags vars body block)
#s(gpredict loc flags predictor outcome)
#s(gnop     loc flags)
#s(gtry     loc flags eval cleanup kind)
  loc   : #s(gloc spot block discr) 或 #f
  flags : 不可变 hasheq（do-not-emit、ctrl-altering）
```

序列 `seq` 就是语句的 pvector。语句没有身份；修改语句 = 在所在 pvector 的下标处替换。

### 2.3 状态

```
#s(GS version types decl-flags next-uid tmp-var-id ssa-version globals loc ctx)
  version    : "15.2.0"
  types      : intmap uid → Tree
  decl-flags : intmap uid → hasheq   ; addressable used initial seen-in-bind-expr context
                                     ; not-gimple-reg readonly artificial label-uid forced
                                     ; label-break label-continue switch-break-label
  next-uid   : 新声明/类型的 uid（从导入的最大 uid 之上开始）
  tmp-var-id : tmp_var_id_num（翻译单元全局）
  ssa-version: 每函数从 0 起
  globals    : hasheq → void/boolean/size/sizetype/ptr 类型 uid
  loc        : gimplify 的 input_location（其他阶段 #f）
  ctx        : gimplify 上下文栈（不可变 gctx 的 list；其他阶段 '()）
```

一个 GS 加一个函数的 body 就是一个可 `write` 的快照。

### 2.4 CFG

```
#s(GCfg blocks edges label->bb next-label-uid messages altered touched)
  blocks   : intmap index → GBlock      ; 0 = ENTRY, 1 = EXIT
  edges    : intmap src-index → pvector of GEdge   ; 后继顺序 = GCC 的 succ 向量顺序
  label->bb: intmap label-uid → block index
  messages : pvector 字符串（"Removing basic block 4"）
  altered, touched : intbits
#s(GBlock index stmts prev next preds)
  preds    : pvector of src-index（unordered_remove 规则保持）
#s(GEdge dest flags goto-locus)
```

边的身份是 (src, dest)：`make_edge` 保证唯一，所以不需要对象身份。边数据只存一份（在 src 的后继向量里），前驱向量只存下标。

## 3. 状态线程约定

- 签名：`(f st args ...) → (values result ... st)`。只读函数不收 `st` 之外的状态，也不返回它。
- 读访问不经状态的：`tree-code`、`tree-type`（返回 uid）、`tree-operand`、`decl-uid`、`decl-name`、常量值。
- 读访问经状态的：类型属性（`type-precision st ty`）、声明可变位（`decl-addressable st d`）。
- 修改：`(tree-set-operand t i v) → t'`、`(set-decl-flag st d key v) → st`、`(pointer-to st ty) → (values ptr-uid st)`。
- 辅助宏 `let/st`：顺序绑定并自动把 `st` 换成每一步返回的新状态。
- 循环用具名 let / `for/fold` 携带状态；提前返回用 `cond` 分支。
- gimplify：`(gimplify-expr st e pre post test fallback) → (values e ret pre post st)`，`pre`/`post` 是 pvector；GCC 的 `internal_pre/post` 是局部的空 pvector。
- genericize：`(walk-tree st t f) → (values t st)`，`f : st t → (values t walk-subtrees? st)`。
- lower：`(lower-function st fndecl bind) → (values body local-decls st)`。
- cfg：`(build-cfg st fndecl body) → (values gcfg st)`，内部每个 GCC 函数是 `gcfg → gcfg`（需要新建 label 时再带 `st`）。

## 4. 序列化

- `kernel/ir/gimple/datum.rkt`：`gs->datum`/`datum->gs`、`tree->datum`、`cfg->datum`。pvector 在 datum 里写成 list（`write` 出的 `#pvector(...)` 不能 `read` 回来，已验证），hasheq 与 prefab 直接 `write/read`。
- 导入：`Fn` 的 `Node` 向量 → `Tree` 值 + `types` 表，是 `import.rkt` 现有 `res` 的纯函数版本；导出是它的逆。
- 验收：每个阶段的输出 `datum → read → datum` 往返 `equal?`。

## 5. 模块布局

```
src/kernel/ir/gimple/types.rkt       prefab 数据（2.1–2.4）+ 无状态访问器
src/kernel/ir/gimple/datum.rkt       序列化
src/component/gimple/state.rkt       GS、let/st、uid/tmp/ssa 计数、decl-flags、类型表查询
src/component/gimple/tree.rkt        builders、fold、谓词、类型兼容性（原 generic/tree.rkt 的纯函数版）
src/component/gimple/import.rkt      gccast dump → (values fndecl body st)
src/component/gimple/genericize.rkt
src/component/gimple/gimplify.rkt
src/component/gimple/lower.rkt
src/component/gimple/cfg.rkt
src/component/gimple/print.rkt       .gimple/.lower/.cfg 三种 dump
```

`src/component/gccast/` 的可变版本在对应阶段的测试切换到新模块后删除；`src/kernel/ir/gccast/types.rkt`（插件 dump 格式）保留。

## 6. 泛化接口（S6、S7）

SSA（替换 `component/java/transform/ssa-construct.rkt` 的算法部分）：

```
(ssa-construct #:entry id #:blocks rpo-pvector #:preds (id → pvector id)
               #:insns (id → pvector insn) #:defs (insn → pvector var) #:uses (insn → pvector var)
               #:rewrite (insn renaming → insn) #:place-phi (block var → ...)
               #:augment-preds [可选：Java handler 增广])
```

CFG → RVSDG（与 roadmap Phase 3.1 的拆分一起做）：接口 = 访问器（entry、succs、phis、insns、defs/uses、op、归一化终结符 `'(cond c t e)`/`'(jump t)`/`'(switch v cases d)`/`'(ret vs)`/`'(throw e)`/`'(unreachable)`）+ 构造器（新块、改终结符、插指令、设 φ）。kernel `Cfg` 与 `GCfg` 各实现一份。

## 7. 阶段与验收

| 阶段 | 内容 | 验收 |
|---|---|---|
| S1 | kernel/ir/gimple/types.rkt、datum.rkt、component/gimple/{state,tree,import}.rkt | 完成（2026-11-06）：`import-test.rkt` 5 个夹具 × 2 版本导入后 datum 往返 `equal?`，builders/fold/decl-flags 单元测试（11 项） |
| S2 | genericize.rkt、gimplify.rkt、print.rkt（.gimple） | 完成（2026-11-06）：`component/gimple/gimplify-test.rkt` 8/8（纯函数版与可变版并存，S4 后删可变版） |
| S3 | lower.rkt、print（.lower） | `lower-test` 8/8 |
| S4 | cfg.rkt、print（.cfg） | `cfg-test` 6/6；删除 `src/component/gccast/` 可变版本 |
| S5 | 泛型 SSA；Java 链路切换，pipeline-test 输出不变；修 roadmap Phase 1 #3 | 现有 Java 测试 + GIMPLE `.ssa` 对齐（新基准：`-fdump-tree-ssa`，-O0） |
| S6 | cfg-to-rvsdg 泛化（与 Phase 3.1 合并） | Java RVSDG 输出 `equal?`；GIMPLE 夹具生成 RVSDG |

## 8. 风险

- 性能：类型属性与声明可变位走 intmap；表达式修改重建脊柱。用 5 个夹具计时，与可变版本对比，超过 3 倍再优化（候选：类型属性缓存进 Tree 值）。
- 对应性：GCC 的 `GS_OK` 重试循环、`internal_pre/post` 队列、label 劫持在函数式写法下结构会变；每处保留 gcc 行号注释。
- 共享表达式：见 2.1；目前夹具无。
