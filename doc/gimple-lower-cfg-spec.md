<!-- 由源码审读代理于 2026-11-05 产出，作为 roadmap 6.3 里程碑 3b（gimple-low.rkt / gimple-to-cfg.rkt）的实现规格；基准：gcc.git trunk 17.0.0 (352a0b5fc55)，实测用 gcc-15。 -->

# GCC trunk（17.0.0）lowering 到 CFG 的规格说明（C 子集，默认 `-O0`）

源码根目录是 `/Users/cutiedeng/Y2026/M10/D28/gcc.git/gcc/`，下文路径都相对它。全程只读。

我用本机 `/opt/homebrew/bin/gcc-15` 跑过几个小样例做核对，命令是 `printf ... | gcc-15 -O0 -x c - -S -o /dev/null -fdump-tree-{gimple,lower,cfg}=stdout`，输入走 stdin、dump 走 stdout，不产生文件。dump 的格式、声明顺序、`<L2>` 编号、`_1` 临时名都与下面的分析一致。cfg cleanup 的细节在 gcc-15 和 trunk 之间有差异（见 §2.10），有冲突时以 trunk 源码为准。

---

## 0. 先更正几个会影响复现的前提

1. **`_N` 在 `-O0` 的 gimple dump 里就已经出现，不是进 SSA 之后才有。**
   - `gimplify_body` 调用 `push_gimplify_context (true)`（gimplify.cc:21807），于是 `into_ssa=1`。
   - `internal_get_tmp_var`（gimplify.cc:687-726）对 reg 类型的值直接 `make_ssa_name`。
   - 如果 `get_name(val)` 能取到名字（tree.cc:12197），SSA 名会带标识符 `create_tmp_var_name(name)`，打印成 `x.0_1`。
   - `D.NNNN` 只用于没有名字的 VAR_DECL 临时变量，例如 return 用的临时（`create_tmp_reg`）。
   - 实测：`_1 = a * b; D.4478 = _1 + 1;`。
2. **`-fdump-tree-cfg` 打印的是 cleanup 之后的状态。** 顺序是 `build_gimple_cfg` → `cleanup_tree_cfg()` → `compact_blocks()`，然后才 dump。`cleanup_tree_cfg` 在 `-O0` 也会运行，它会：
   - 删除不可达块；
   - 合并块、删除 forwarder 块；
   - 把只剩 2 个 label 的 switch 改写成 gcond；
   - 折叠常量 gcond；
   - 规范化 `_Bool == 0`。
3. **CFG 建好后，简单 goto 语句被删除，gcond 的 label 被置空。**
   - 简单 goto 在 tree-cfg.cc:1434 被删除，对应的边带 `EDGE_FALLTHRU`。
   - gcond 的两个 label 在 tree-cfg.cc:1268-1269 被置 NULL。
   - dump 里的 `goto <bb N>;` 是根据边合成出来的。
4. **`remove_useless_stmts` 已经不存在了**，只剩 tree-cfg.cc:2920 的一句注释。`gimple_split_block` 也不参与 `build_gimple_cfg`：切块用的是 `gsi_split_seq_before`。
5. **`-O0` 的 C 代码里 GIMPLE_TRY_FINALLY 非常普遍**（你要求忽略 EH/try，但这一点必须知道）。
   - 任何非 gimple-reg 的局部变量（数组、struct、被取地址的标量）都会让 `gimplify_bind_expr` 生成 `try { … } finally { a = {CLOBBER(eos)}; }`（gimplify.cc:1601-1614、1680-）。
   - VLA 会生成 stack_save/restore。
   - lower 会保留 GIMPLE_TRY；它由 lower 和 cfg 之间的 `pass_lower_eh` 展开（tree-eh.cc:1715 `lower_try_finally`）。`-O0` 下，如果 finally 里只有 clobber 或 stack_restore，`decide_copy_try_finally`（tree-eh.cc:1633-1665）选择 copy 策略。
   - 不实现这一步，带数组的函数 CFG 对不上。实测 `arr()` 出现了 `Merging blocks 2 and 3`。
6. **`-O0` C 中会出现在 dump 里的额外语句：**
   - GIMPLE_PREDICT：条件上下文中的 `return` 前会有 `// predicted unlikely by early return (on trees) predictor.`（gimplify.cc:1725-1734）；`goto` 前（c/c-typeck.cc:13376）和 `continue` 前（c-family/c-gimplify.cc:575）也有对应的 predict。
   - `main` 的隐式 `return 0;`（c/c-decl.cc:11571-11581，条件是 `flag_isoc99`，位置为 BUILTINS_LOCATION）。
   - lower 阶段对 builtin 调用做 `fold_stmt`：实测 `printf("hi\n")` 变成 `__builtin_puts (&"hi"[0]);`。

---

## 1. gimple-low.cc（pass `"lower"`）

- pass 定义在 gimple-low.cc:220-254。要求 `PROP_gimple_any`，提供 `PROP_gimple_lcf`。
- 在 passes.def:35-41 中的位置：`pass_lower_omp` → **`pass_lower_cf`** → `pass_lower_tm` → `pass_refactor_eh` → `pass_lower_eh` → `pass_coroutine_lower_builtins` → **`pass_build_cfg`**。

### 1.1 状态

- `return_statements_t { tree label; greturn *stmt; }`（61-66）
- `lower_data { tree block; vec<return_statements_t> return_statements; bool cannot_fallthru; }`（69-80）
  - `return_statements` 当栈用（RS）。
  - `cannot_fallthru` 只用来决定"紧跟在后面的 return"要不要删。

### 1.2 `lower_function_body`（122-216）

```
assert body == [唯一一条 GIMPLE_BIND]                              // :133
data.block := DECL_INITIAL(fn); BLOCK_SUBBLOCKS(it):=NULL; BLOCK_CHAIN(it):=NULL; TREE_ASM_WRITTEN(it):=1
data.RS := []; cannot_fallthru := false
lowered := [bind]; lower_gimple_bind(gsi_start(lowered))          // :143-147；lowered 变为全扁平序列
i := gsi_last(lowered)
may := gimple_seq_may_fallthru(lowered)                           // :165，看最后一条非 debug 语句
if may && (RS 为空 || RS.last().stmt 的 retval != NULL):            // :166-169
    在 i 后追加 `return;`   (loc=cfun->function_end_locus, block=DECL_INITIAL(fn)); may := false
while RS 非空:                                                     // :180-194
    t := RS.pop()                                                  // LIFO：最后登记的先出
    追加 glabel(t.label)     // 新建 label 语句：location=0，不设 block
    追加 t.stmt              // 第一次遇到的那条 greturn 对象本身
    if may: loc(t.stmt) := UNKNOWN_LOCATION; may := false
gimple_set_body(fn, lowered)
BLOCK_SUBBLOCKS(root) := blocks_nreverse(...); clear_block_marks(root)   // :209-213
```

### 1.3 `lower_sequence`（261-267）

```
for (gsi = gsi_start(*seq); !end; ) lower_stmt(&gsi)
```

推进 gsi 由 `lower_stmt` 自己负责（`gsi_next` 或 `gsi_remove`）。新插入或替换进来的语句不会再被 lower（注释 256-258）。

### 1.4 `lower_stmt`（693-895）

入口处先对每条语句执行 `gimple_set_block(stmt, data->block)`（697）。然后按语句类型分派：

| code | 动作 | 之后的 `cannot_fallthru` |
|---|---|---|
| GIMPLE_BIND | `lower_gimple_bind` 后直接 return | 不改，即继承 bind 体最后一条语句的状态；空 bind 保持原值 |
| COND / GOTO / SWITCH | `gsi_next` | true |
| RETURN | 若当前为 true：`gsi_remove`，不登记、不生成 goto；否则调用 `lower_gimple_return` | 删除时保持 true；否则置 true |
| TRY_FINALLY | 先 lower eval 序列，记 c1=flag；flag=false 后 lower cleanup 序列；flag \|= c1；`gsi_next`（GIMPLE_TRY 本身保留） | — |
| TRY_CATCH | `lower_try_catch`（974，跳过） | — |
| DEBUG | `-O0` 不存在（opts.cc:1466-1473 要求 `optimize`） | — |
| NOP / ASM / ASSIGN / PREDICT / LABEL | `gsi_next` | false |
| CALL | 见下 | — |
| ASSUME | `lower_assumption`（540），外提成函数，改成 `.ASSUME` | — |

GIMPLE_CALL 的处理（802-868），按顺序：

1. 实参中凡是 `EXPR_P` 的，执行 `TREE_SET_BLOCK(arg, block)`。
2. 如果 fndecl 是 BUILT_IN_NORMAL：
   - `BUILT_IN_SETJMP`：调用 `lower_builtin_setjmp`，flag 置 false，return（跳过）。
   - `POSIX_MEMALIGN`：仅在 `flag_tree_bit_ccp` 时处理，`-O0` 不触发。
   - `ASSUME_ALIGNED` 且 `!optimize`：调用 `lower_builtin_assume_aligned`。它只在 lhs 是指针 SSA 名时设置对齐信息，语句本身不变。然后 flag 置 false，`gsi_next`，return，**不 fold**。
3. 如果 `decl` 存在且 `flags_from_decl_or_type(decl) & ECF_NORETURN`：flag 置 true，`gsi_next`，return，不 fold。
   - 注意这里只认直接调用的 decl。通过 noreturn 函数指针的调用在这里识别不出来，但 `gimple_stmt_may_fallthru` 和 CFG 用的 `gimple_call_flags` 能识别。
4. `IFN_ASAN_MARK`：sanitizer 专用，忽略。
5. 如果 `gimple_call_builtin_p(stmt)`：执行 `fold_stmt(gsi)`（865-866）。
6. flag 置 false，`gsi_next`。

### 1.5 `lower_gimple_bind`（900-969）：变量去向与 BLOCK 记账

```
old := data->block; nb := gimple_bind_block(bind)
if nb == old:  assert nb == DECL_INITIAL(fn); nb := NULL     // C 最外层 bind 的 BLOCK 就是 DECL_INITIAL
elif nb:       assert !TREE_ASM_WRITTEN(nb); TREE_ASM_WRITTEN(nb):=1
               BLOCK_CHAIN(nb):=BLOCK_SUBBLOCKS(old); BLOCK_SUBBLOCKS(old):=nb   // 头插
               BLOCK_SUBBLOCKS(nb):=NULL; BLOCK_SUPERCONTEXT(nb):=old; data->block:=nb
record_vars(gimple_bind_vars(bind))       // 先登记再 lower body，即前序
(把 bind_vars 中不属于 BLOCK_VARS 的前缀断开 DECL_CHAIN，纯 GC 卫生)        // :936-953
lower_sequence(body)
if nb: BLOCK_SUBBLOCKS(nb):=nreverse; data->block:=old
gsi_insert_seq_before(gsi, body, SAME_STMT); gsi_remove(gsi)  // gsi 指向 bind 之后
```

lower 之后还剩下的东西：

- **`cfun->local_decls`** 中的 VAR_DECL。
- **每条语句 location 中的 block**：等于最内层带 BLOCK 的 bind。没有 BLOCK 的 bind 不改变 `data->block`，例如 gimplifier 为 `-Wimplicit-fallthrough` 给 switch 包的 bind（gimplify.cc:~2995）。
- **重建过的 BLOCK 树**：子块恢复源码顺序。`BLOCK_VARS` 不变。

### 1.6 `lower_gimple_return`（1173-1212）与 return 合并

```c
for (i = RS.length () - 1; i >= 0; i--)
    if (gimple_return_retval (stmt) == gimple_return_retval (RS[i].stmt))   // 指针相等，NULL==NULL 也算
      { gimple_set_location (RS[i].stmt, UNKNOWN_LOCATION); tmp_rs = RS[i]; goto found; }
tmp_rs.label = create_artificial_label (cfun->function_end_locus);   // 无名，DECL_ARTIFICIAL=1
tmp_rs.stmt = stmt; RS.safe_push (tmp_rs);
found:
if (!optimize && gimple_has_location (stmt)) DECL_ARTIFICIAL (tmp_rs.label) = 0;   // ★ -O0
t = gimple_build_goto (tmp_rs.label); gimple_set_location (t, gimple_location (stmt));
gsi_insert_before (gsi, t, GSI_SAME_STMT); gsi_remove (gsi, false);
```

retval 的来源见 `gimplify_return_expr`（gimplify.cc:1744-1834）：

- `return;`：retval 为 NULL。
- 非 void 函数中的 `return e;`：共用 `gimplify_ctxp->return_temp`（`D.NNNN`）。若 `aggregate_value_p`，则共用 RESULT_DECL（打印为 `<retval>`）。
- 所以 C 中 RS 最多 2 项：NULL 和那个共享 temp。

★ 处的影响：`-O0` 下 return label 几乎总是**非 artificial**。后果有四个：

- 两轮 `cleanup_dead_labels` 都保留它；
- `-O0` 下禁止把它所在的块合并进前驱；
- 它不会被当作 dead label 删掉；
- 在 cfg dump 中打印为 `<L%d>`。

实测 `h`（lower dump）：

```
  D.4478 = _1 + 1;
  // predicted unlikely by early return (on trees) predictor.
  goto <D.4479>;
  <D.4477>:
  D.4478 = x;
  goto <D.4479>;
  <D.4479>:
  return D.4478;
```

### 1.7 fallthru 判定

`gimple_stmt_may_fallthru`（1099-1158）：

- NULL → true。
- GOTO / RETURN / RESX / SWITCH / COND → false。
- BIND → 递归判 body。
- TRY_FINALLY → eval 与 cleanup 都能 fallthru 才算能。
- CALL → `!gimple_call_noreturn_p`（gimple.h:3551）。
- 其余 → true（包括 LABEL）。

`gimple_seq_may_fallthru`（1164）取序列最后一条非 debug 语句来判断。

### 1.8 `record_vars_into`（1432-1450）和 `record_vars`（1456）

沿 DECL_CHAIN 遍历：

- 跳过 `!VAR_P` 的（LABEL_DECL、TYPE_DECL、FUNCTION_DECL、CONST_DECL）；
- 跳过 `DECL_EXTERNAL` 的；
- 其余执行 `add_local_decl`（function.cc:7226，`vec_safe_push`）。
- TREE_STATIC 的局部变量会被收录，不去重。

bind 中变量的顺序由 gimplifier 决定：temp 先头插到 `ctxp->temps`（gimplify.cc:858），在 `declare_vars`（761-795，由 21912 调用）中 nreverse，再前置到**最外层 bind**。所以最外层 bind 的 vars 是"temp（按创建顺序）++ 前端变量"。

打印时 `FOR_EACH_LOCAL_DECL` 是**逆序**的（function.h:465-466）。实测 lower/cfg dump 的声明顺序是 `int y; int x; int D.4478;`。

lower 之后才新建的 temp（`gimplify_ctxp==NULL` → `record_vars`，gimplify.cc:890-891）会被追加到末尾，因此打印在**最前面**。实测 s1：`unsigned int D.4932; unsigned int D.4931; int D.4929;`。

### 1.9 C 子集的 lower 边界情况

- **空函数**：lowered 为空，`may_fallthru(NULL)` 为 true，补一句 `return;`。
- **非 void 函数从末尾掉出**：在所有 label 之前补一句不带 label 的 `return;`。
- **void 函数里有 `return;` 且末尾掉出**：不新建 return，直接 fallthrough 进 `<L>: return;`，该 return 的 location 清零（实测 `f`）。
- **紧跟控制语句的 return 被删除**：return 前一条若是 goto/cond/switch/noreturn 调用/另一个 return，并且中间没有 label、赋值、predict 之类，就删掉。
  - 例：`abort(); return;` 中的 return 被删。
  - 反例：`exit(0); return 0;` 因中间有 `D=0` 而保留，之后由 CFG 当作不可达块删除。
- **main**：显式 return 与隐式 return 共用 temp。实测出现两份 `D.4940 = 0; goto <D.4941>;`，cfg 中打印 `Removing basic block 3`。
- **以 noreturn 调用结尾的函数**：不补 return（实测 `lab`）。
- **用户 label 的 LABEL_DECL**：位于 bind vars 中，gimple dump 打印为 `void L1 = <<< error >>>;`。`record_vars` 会跳过它，所以 lower/cfg 中不出现。

---

## 2. tree-cfg.cc（pass `"cfg"`）

### 2.1 总流程

`execute_build_cfg`（329-347）：

1. `build_gimple_cfg`（183-216），依次执行：
   - `init_empty_tree_cfg`（146-167）：块数 = `NUM_FIXED_BLOCKS` = 2。ENTRY=0、EXIT=1（basic-block.h:261-265）。
   - `make_blocks`
   - 若一个块都没有：`create_empty_bb(ENTRY)`
   - **`cleanup_dead_labels` 第 1 次**
   - **`group_case_labels`**：在 build_gimple_cfg 中**会**运行（210）
   - `make_edges`
   - `assign_discriminators`
   - **`cleanup_dead_labels` 第 2 次**
2. `gimple_set_body(fn, NULL)`。
3. **`cleanup_tree_cfg()`**：此时 `current_loops==NULL`，所有与循环相关的保护都不生效。
4. `loop_optimizer_init`：向 dump 打印循环信息（loop-init.cc:128）。
5. `replace_loop_annotate`：只处理 ivdep/unroll 等 pragma 产生的 IFN_ANNOTATE。
6. pass manager 更新 properties（passes.cc:2259-2264、2690），然后 dump（2720）。

### 2.2 `make_blocks`（575-677）与 `make_blocks_1`（489-570）

`make_blocks` 本身在默认情况下不做额外的事：debug marker 搬移在 `-O0` 不发生；`#pragma GCC suppress_coverage` 的 nop 插入只在有该 pragma 时发生。然后调用 `make_blocks_1(seq, ENTRY)`：

```
bb:=ENTRY; start_new:=true; first:=true; prev:=stmt:=NULL
for each s:
  prev := stmt                        // 无 debug 语句时 prev 恒为前一条
  stmt := s
  if is_call(s): gimple_call_initialize_ctrl_altering(s)          // 461-483
  if start_new || stmt_starts_bb_p(s, prev):
      if !first: gsi_split_seq_before(i, &seq)   // i 之前留在旧块
      bb := create_basic_block(seq, bb)          // create_bb 682-713: index=last_basic_block++，链到 bb 之后
      start_new:=false; prev:=NULL
  gimple_set_bb(s, bb)   // 对 label：首次分配 LABEL_DECL_UID = cfg->last_label_uid++（gimple.cc:1904-1931）
  if stmt_ends_bb_p(s):
      if 有lhs && stmt_can_make_abnormal_goto(s) && is_gimple_reg_type(lhs):   // setjmp 场景
          tmp:=create_tmp_var; s.lhs:=tmp; 在 s 后插入 `lhs = tmp`（它会开启下一块）
      start_new := true
  first := false
```

**LABEL_DECL_UID** 从 0 开始，按语句顺序给**所有** label 编号，包括之后会被删掉的。cfg dump 中无名 label 打印为 `<L%d>`。实测 `f` 的 return label 是 `<L2>`。

### 2.3 `stmt_starts_bb_p`（2780-2831）

```c
if (stmt == NULL) return false;
if (prev_stmt && is_gimple_debug (prev_stmt)) return false;
if (glabel *l = dyn_cast<glabel *>(stmt)) {
  if (DECL_NONLOCAL (label(l)) || FORCED_LABEL (label(l))) return true;
  if (glabel *p = safe_dyn_cast<glabel *>(prev_stmt)) {
    if (DECL_NONLOCAL (label(p)) || !DECL_ARTIFICIAL (label(p))) return true;
    cfg_stats.num_merged_labels++; return false;
  }
  return true;
}
else if (CALL) { if (flags & ECF_RETURNS_TWICE) return true;
                 if (IFN_PHI && prev && prev 不是 LABEL && prev 不是 IFN_PHI) return true; }
return false;
```

总结起来，开新块的规则：

- 首条语句开新块。
- 块结束语句之后的下一条开新块。
- label 开新块，除非它的前一条是 **artificial 且非 nonlocal** 的 label；但 FORCED/NONLOCAL 的 label 总是开新块。
  - 因此 `<D.1>: foo:` 在同一块，而 `foo: <D.1>:` 是两个块。
- returns_twice 调用（setjmp）开新块。

### 2.4 `stmt_ends_bb_p`（2837）

`stmt_ends_bb_p = is_ctrl_stmt || is_ctrl_altering_stmt`：

- `is_ctrl_stmt`（2701）：COND、SWITCH、GOTO、RETURN、RESX。
- `is_ctrl_altering_stmt`（2721）：带 GF_CALL_CTRL_ALTERING 的 CALL、EH_DISPATCH、带 label 的 ASM、OMP、TRANSACTION，以及 `stmt_can_throw_internal`（C 默认没有 -fexceptions，恒为 false）。

CALL 是否被标为 ctrl-altering 由 `gimple_call_initialize_ctrl_altering` 决定，满足任一条件即是：

- `call_can_make_abnormal_goto`（2664）：`(has_nonlocal_label || calls_setjmp) && side_effects && !ECF_LEAF`；
- `ECF_NORETURN`；
- `BUILT_IN_RETURN`；
- unique IFN。

`calls_setjmp` 由 `notice_special_calls`（2219）在 gimplify 时置位（gimplify.cc:4885、7483）。

### 2.5 `cleanup_dead_labels`（1558-1754）

分三步：

1. **选主 label**：每个块的主 label 取其首个 label；若首个是 artificial，而后续某个前导 label 是用户 label，则改用那个用户 label（1566-1598）。
2. **重定向引用**：gcond 的 true/false label、switch 每个 `CASE_LABEL`、asm label、非 computed goto 的目标，统统改指主 label。`main_block_label`（1483）会把被引用块标记为 `used`。
3. **删除前导 label**：保留"已被引用的主 label、所有用户 label、NONLOCAL、FORCED"，其余前导 label 删除。

第 2 轮运行时，cond 的 label 已为空、简单 goto 已删除，剩下的引用只有 switch（和 asm goto）。所以最终存活的 label 是：用户 label、`-O0` 下的 return label、switch 目标块的主 label。

之后**再也不会运行** `cleanup_dead_labels`（只有 tree-cfgcleanup.cc:1434 的 post-optimizing 会再跑）。因此 switch 被改写成 if 之后，原来的 case label 仍然会被打印（实测 s1 的 `<L3>:`）。

### 2.6 `group_case_labels_stmt`（1761-1831）

- `default_bb` 取 label[0] 所在的块。
- 从 i=1 开始扫描：
  - 目标块为 NULL 或等于 `default_bb` 的 case 被丢弃；
  - 目标块相同且 `CASE_LOW(next) == base_high + 1` 的相邻 case 合并成区间（设置 `CASE_HIGH`）；
  - 结果压缩到 `new_size`。
- trunk 已撤销"删除指向 `__builtin_unreachable` 块的 case"这一逻辑（f108e23d2bf）。

### 2.7 `make_edges`（975-1077）、`make_edges_bb`（861-970）

`make_edges` 先建 **ENTRY 边**：`make_edge(ENTRY, BASIC_BLOCK(2), EDGE_FALLTHRU)`（985-987），然后按链表顺序对每个块调用 `make_edges_bb`。`make_edges_bb` 看块的最后一条语句（块为空则**不建任何边**，867）：

| last | 生成的边 |
|---|---|
| GOTO（简单） | `make_goto_expr_edges`（1422）：到目标块，**EDGE_FALLTHRU**，`goto_locus` = goto 的 loc，然后**删除 goto 语句** |
| GOTO（computed） | 返回 1，之后通过 dispatcher 处理（`handle_abnormal_edges` 748） |
| RETURN | 到 EXIT，flags=0，`goto_locus` = return 的 loc，无 fallthru |
| COND | `make_cond_expr_edges`（1243-1270），见下 |
| SWITCH | `make_gimple_switch_edges`（1379）：对 i=0..n-1（0 为 default）调用 `make_edge(bb, label_bb, 0)`。`make_edge` 按 (src,dest) 去重（cfg.cc:331-343，返回 NULL 并 OR flags），所以**每个不同目标块一条边**，按首次出现排序，default 在最前，不设 goto_locus |
| CALL | `BUILT_IN_RETURN` → EXIT；否则若 `!noreturn` 则 fallthru（noreturn 块没有后继，实测 `5 succs { }`） |
| ASSIGN / 其他 | fallthru：`make_edge(bb, bb->next_bb, EDGE_FALLTHRU)` |

gcond 的边（`make_cond_expr_edges`）：

- 先建 `true → then_bb`，标 `EDGE_TRUE_VALUE`；再建 `false → else_bb`，标 `EDGE_FALSE_VALUE`。
- 每条边的 `goto_locus` 取目标块 `first_stmt` 的 location（通常就是那个 label 语句的 location）。
- 若 then_bb 与 else_bb 相同，只有一条边，flags 为 TRUE|FALSE。
- 最后把两个 label 置 NULL。

每个块的 succ 向量按创建顺序 push（`connect_src`，cfg.cc:215）；删除边用 `unordered_remove`（cfg.cc:235-254，把末尾元素移到被删位置）。loops dump 中的 `;; N succs {…}` 反映这个顺序，实测 switch 块是 `{ 4 3 }`（default 在前）。

### 2.8 `assign_discriminators`（1148-1238）

它在 cleanup 之前运行，并且会改变 location 的相等性，这对 `-O0` 下的合并判断很关键。

- 以行号为 key 维护一个 map，`bb_id` 是递增计数器。
- 遍历每块的每条语句：
  - raw loc ≠ 0 的才处理；与上一条 loc 相同则直接复用上一条的替换结果。
  - 否则调用 `assign_discriminator`：该行首次出现时记 `{bb_id, 0}`；之后若 bb_id 不同，则 discriminator 加 1；discriminator 非 0 时返回 `location_with_discriminator`。
  - 遇到 CALL 后 `bb_id++`。
- 块尾：若不是单后继则 `bb_id++`；然后对每条后继边的 `goto_locus` 同样处理，每条边后 `bb_id++`；最后再 `bb_id++`。

关于 location 的模型：

- 一个 location 等价于元组 **(caret, 源码区间, block, discriminator)**。它们是 adhoc 表中按值哈希的，相同元组得到同一个 `location_t`（libcpp/line-map.cc:176-）。
- `set_block(0, B)` 会产生一个非 0 但 locus 为 0 的位置：lower 之后，几乎所有原本无位置的语句（比如 label）都是这样。
- `location_with_discriminator` 遇到 locus 为 0 时**返回 0**（input.cc:1048-1059），block 也一并丢失。
- `gimple_has_location` 与 `LOCATION_LOCUS` 只看 caret。下文中的"相等"一律是 raw 相等。

### 2.9 `cleanup_tree_cfg`（tree-cfgcleanup.cc）在 `-O0` 的行为

调用链：`cleanup_tree_cfg`（1320）→ `cleanup_tree_cfg_noloop`（1134-1284）。没有 loops，跳过循环头处理。

**A. `cleanup_control_flow_pre`（1007-1117）**

- 从 ENTRY 出发按 succ 顺序做前序 DFS，对首次访问到的块调用 `cleanup_control_flow_bb`（278-365）。
  - 由于此阶段支配信息被设为 `DOM_NONE`，删边只删边本身。
  - 被删掉的边不会被 DFS 继续访问。
- 结束后，所有没访问到的块按 **index 升序**删除，每删一个打印 `Removing basic block N`（remove_bb，tree-cfg.cc:2267）。

`cleanup_control_flow_bb` 对 COND/SWITCH 结尾的块调用 `cleanup_control_expr_graph`（166-232），步骤如下：

- 若块有多个后继：
  1. SWITCH 且恰有 2 个 label 时，**`convert_single_case_switch`**（86-125）把它改写成 gcond：
     - 单值 case：`if (idx == low)`；
     - 区间 case：`generate_range_test`（tree-cfg.cc:9683）生成 `T1 = (unsigned) idx; T2 = T1 + (-low); if (T2 <= high-low)`。新 temp 是 VAR_DECL，经 match.pd 规范化。
     - case 边 OR 上 TRUE，default 边 OR 上 FALSE。
  2. 对 COND 做 **`canonicalize_bool_cond`**（130-160）：bool（或 1 位整型）的 `x == 0` 或 `x != 1` 改成 `x != 0`，并交换两条边的 TRUE/FALSE 标志；同时向 dump 打印 `  Swapped '…'`。
  3. 对 COND 调用 `gimple_simplify`。若结果是 INTEGER_CST（含 `0 != 0` 这类，实测 k0），用 `find_taken_edge`（2378）确定走哪条边。SWITCH 的 index 若为常量也一样；只有 1 个 label 时取 default。
  4. 找到唯一的出边后，删除其余所有边。
- 若块只有一个后继：直接取那条边。
- 最后删除这条 COND/SWITCH 语句，那条边清掉 TRUE/FALSE 标志、加上 FALLTHRU。

**B. 第一轮遍历与 worklist**

- 计算支配信息，`start_recording_case_labels`。
- 对 i ∈ [2, last) 依次调用 `cleanup_tree_cfg_bb`（884-911）。
- 之后处理 worklist：每次 `bitmap_clear_first_set_bit` 取最小 index，执行 `cleanup_control_flow_bb` 再执行 `cleanup_tree_cfg_bb`。

`cleanup_tree_cfg_bb(bb)` 的逻辑：

```
if maybe_remove_forwarder_block(bb): return true
elif single_pred(bb) && want_merge(pred, bb): 把 pred 放进 worklist（留到处理 pred 时再合并）
elif single_succ(bb) && want_merge(bb, succ): merge_blocks(bb, succ)
```

`want_merge` 在此阶段等价于 `gimple_can_merge_blocks_p`，因为 count 都未初始化，`ok_for_merging` 恒为真。

**`maybe_remove_forwarder_block`（484-795）在 C/-O0 下的判定条件**，全部满足才删除：

- bb 有唯一后继，且该后继不是 EXIT、不是 bb 自己，且该边不是 ABNORMAL；
- 没有任何前驱是 ENTRY，没有前驱边是 EH 边；
- **`-O0` 位置检查（前驱边）**：对每条前驱边 e，若 `LOCUS(e.goto_locus)` 或 `LOCUS(locus)` 有一个已知，就要求 `e.goto_locus == locus`，否则拒绝。这里 `locus` 是后继边的 goto_locus。
- **trunk/GCC16 新增**：若 bb 只有一个前驱且该前驱只有一个后继，拒绝，留给合并逻辑处理（40c037adb36）。
- dest 的首个 label 不是 nonlocal 或 EH landing pad；
- bb 中只含 label（非 nonlocal、非 EH landing pad）或 debug 语句。注意 **PREDICT 会让它不算 forwarder**。
- **`-O0` 位置检查（label）**：对每个 label，若 `has_location(label)` 或 `locus` 已知，就要求 `loc(label) == locus`，否则拒绝。

删除时的动作：

- 对每条前驱边调用 `redirect_edge_and_branch`（tree-cfg.cc:6169）：
  - 若前驱的两条出边都将指向 dest，直接删掉 cond/switch，边的 flags 置为 FALLTHRU（6133-6162）；
  - switch 前驱通过 `gimple_block_label(dest)`（6098）改写 `CASE_LABEL`，必要时在 dest 开头新建一个 artificial label（会分配新的 L 号）；
  - 若重定向后与已有边重合，合并 flags（cfghooks.cc:536）。
- 把 bb 中的用户 label、FORCED、NONLOCAL label 移到 dest 开头；
- 删除 bb，打印 `Removing basic block N`。

**`gimple_can_merge_blocks_p(a,b)`（tree-cfg.cc:1855-1955）**，全部满足才合并：

- a 只有一个后继，且该边不是 COMPLEX（ABNORMAL|ABNORMAL_CALL|EH|PRESERVE），后继就是 b；
- b 只有一个前驱；a 不是 ENTRY，b 不是 EXIT；
- a 的最后一条语句若存在，则不能是块结束语句；
- coverage pragma 相关条件（默认无 pragma，不生效）；
- **b 的前导 label 中若有 `!DECL_ARTIFICIAL` 的（在 `-O0` 下即任何用户 label 或 return label），拒绝**；
- **`-O0` 位置检查**：若 a→b 边的 goto_locus（raw）≠ 0，则要求 a 最后一条非 debug 语句的 loc，或 b 第一条非 label 语句的 loc，与 goto_locus 相等，否则拒绝。

合并动作 `gimple_merge_blocks`（2044-2191）：

- 打印 `Merging blocks A and B`；
- 把 b 移到 a 之后；
- 删除 b 的所有 label（FORCED label 移到 a 的开头）；
- 把 b 的语句追加到 a 的末尾；
- b 的后继边归 a（cfghooks.cc:882-962）。

`-O0` 的实际效果：

- 来自 goto 的边带着已知的 goto_locus，因此几乎不会被合并，也几乎不会被当作 forwarder 删除。
- 结果是保留大量只含 `goto <bb N>;` 的空块（实测 `loop` 中的 bb4、bb11）。
- 只含一个 `<Ln>:` 的空块也会保留（实测 s1 的 bb4）。

**C. 收尾**

- `end_recording_case_labels`：对被改动过的 switch 再执行一次 `group_case_labels_stmt`。
- **`compact_blocks`**（cfg.cc:169-196）：按链表顺序从 2 开始重新编号。

### 2.10 编号规则，以及 gcc-15 与 trunk 的差异

- **编号**：bb0 = ENTRY，bb1 = EXIT，所以第一个真实块是 `<bb 2>`。
  - 链表顺序即语句顺序：新块总是 link 到前一块之后；合并时 `move_block_after`。
  - dump 用 `FOR_EACH_BB_FN` 遍历，不包含 ENTRY/EXIT。
  - `Removing` 和 `Merging` 消息中的编号是**压缩前**的 index。
- **gcc-15 与 trunk 的已知差异**：
  - assign_discriminators 被重写（385d9937f0e，GCC16）；
  - forwarder 判定新增了单前驱排除（40c037adb36，GCC16）；
  - maybe_remove_forwarder_block 是由两个旧函数合并而来（10147ee3352）；
  - group_case 的部分撤销（f108e23d2bf）；
  - suppress_coverage pragma（f6a1e69999d，仅 trunk）。
  - 实测 gcc-15 对 k0 打印了 `Removing basic block 4`。按 trunk 规则，同一位置更可能出现的是 `Merging blocks 2 and 4`：bb2→bb4 那条 false 边的 goto_locus 在 assign_discriminators 中被清成 0。但我没有 trunk 二进制，这一点**无法实测**。

### 2.11 其他

- **setjmp / computed goto**：
  - 产生一个 `.ABNORMAL_DISPATCHER (0)` 块，或一个 `gotovar` 分派块（`handle_abnormal_edges` 748-853），它在链表中插在第一个目标块之后。
  - 所有可能做 abnormal goto 的调用块都向它连 ABNORMAL 边，它再连到各个 FORCED label 块和 setjmp 块。
- **`gimple_split_block`**（6348-6391）是 `split_block` 的 hook，不参与 `build_gimple_cfg`。

---

## 3. 谓词定义（逐字）

gimple-expr.h：

```c
// :74
inline bool is_gimple_reg_type (tree type) { return !AGGREGATE_TYPE_P (type); }   // 非 ARRAY/RECORD/UNION/QUAL_UNION
// :82
inline bool is_gimple_variable (tree t)
{ return (VAR_P (t) || TREE_CODE (t) == PARM_DECL || TREE_CODE (t) == RESULT_DECL || TREE_CODE (t) == SSA_NAME); }
// :93
inline bool is_gimple_id (tree t)
{ return (is_gimple_variable (t) || TREE_CODE (t) == FUNCTION_DECL || TREE_CODE (t) == LABEL_DECL
          || TREE_CODE (t) == CONST_DECL || TREE_CODE (t) == STRING_CST); }
// :120
inline bool is_gimple_addressable (tree t)
{ return (is_gimple_id (t) || handled_component_p (t) || TREE_CODE (t) == TARGET_MEM_REF || TREE_CODE (t) == MEM_REF); }
// :130  is_gimple_constant: INTEGER_CST, POLY_INT_CST, REAL_CST, FIXED_CST, COMPLEX_CST, VECTOR_CST, STRING_CST → true；其他 false
```

`handled_component_p`（tree.h:5605）：COMPONENT_REF、BIT_FIELD_REF、ARRAY_REF、ARRAY_RANGE_REF、REALPART_EXPR、IMAGPART_EXPR、VIEW_CONVERT_EXPR。

gimple-expr.cc：

```c
// :593
bool is_gimple_lvalue (tree t)
{ return (is_gimple_addressable (t) || TREE_CODE (t) == WITH_SIZE_EXPR || TREE_CODE (t) == BIT_FIELD_REF); }
// :605 / :620
static bool is_gimple_condexpr_1 (tree t, bool allow_traps, bool allow_cplx)
{ tree op0;
  return (is_gimple_val (t)
          || (COMPARISON_CLASS_P (t) && (allow_traps || !tree_could_throw_p (t))
              && ((op0 = TREE_OPERAND (t, 0)), true)
              && (allow_cplx || TREE_CODE (TREE_TYPE (op0)) != COMPLEX_TYPE)
              && is_gimple_val (op0) && is_gimple_val (TREE_OPERAND (t, 1)))); }
bool is_gimple_condexpr_for_cond (tree t) { return is_gimple_condexpr_1 (t, false, true); }
// :672
bool is_gimple_address (const_tree t)
{ if (TREE_CODE (t) != ADDR_EXPR) return false;
  op = TREE_OPERAND (t, 0);
  while (handled_component_p (op)) {
    if ((TREE_CODE (op) == ARRAY_REF || TREE_CODE (op) == ARRAY_RANGE_REF) && !is_gimple_val (TREE_OPERAND (op, 1)))
      return false;
    op = TREE_OPERAND (op, 0); }
  if (CONSTANT_CLASS_P (op) || TREE_CODE (op) == TARGET_MEM_REF || TREE_CODE (op) == MEM_REF) return true;
  switch (TREE_CODE (op)) { case PARM_DECL: case RESULT_DECL: case LABEL_DECL: case FUNCTION_DECL:
                            case VAR_DECL: case CONST_DECL: return true; default: return false; } }
// :713
bool is_gimple_invariant_address (const_tree t)
{ if (TREE_CODE (t) != ADDR_EXPR) return false;
  op = strip_invariant_refs (TREE_OPERAND (t, 0)); if (!op) return false;
  if (TREE_CODE (op) == MEM_REF) { op0 = TREE_OPERAND (op, 0);
    return (TREE_CODE (op0) == ADDR_EXPR
            && (CONSTANT_CLASS_P (TREE_OPERAND (op0, 0)) || decl_address_invariant_p (TREE_OPERAND (op0, 0)))); }
  return CONSTANT_CLASS_P (op) || decl_address_invariant_p (op); }
// :765
bool is_gimple_min_invariant (const_tree t)
{ if (TREE_CODE (t) == ADDR_EXPR) return is_gimple_invariant_address (t); return is_gimple_constant (t); }
// :788
bool is_gimple_reg (tree t)
{ if (virtual_operand_p (t)) return false;
  if (TREE_CODE (t) == SSA_NAME) return true;
  if (!is_gimple_variable (t)) return false;
  if (!is_gimple_reg_type (TREE_TYPE (t))) return false;
  if (TREE_THIS_VOLATILE (t)) return false;
  if (needs_to_live_in_memory (t)) return false;
  if (VAR_P (t) && DECL_HARD_REGISTER (t)) return false;
  return !DECL_NOT_GIMPLE_REG_P (t); }
// :834
bool is_gimple_val (tree t)
{ if (is_gimple_variable (t) && is_gimple_reg_type (TREE_TYPE (t)) && !is_gimple_reg (t)) return false;
  if (TREE_CODE (t) == OMP_NEXT_VARIANT || TREE_CODE (t) == OMP_TARGET_DEVICE_MATCHES) return true;
  return (is_gimple_variable (t) || is_gimple_min_invariant (t)); }
// :874
bool is_gimple_call_addr (tree t) { return (TREE_CODE (t) == OBJ_TYPE_REF || is_gimple_val (t)); }
// :882
bool is_gimple_mem_ref_addr (tree t)
{ return (is_gimple_reg (t) || poly_int_tree_p (t)
          || (TREE_CODE (t) == ADDR_EXPR
              && (CONSTANT_CLASS_P (TREE_OPERAND (t, 0)) || decl_address_invariant_p (TREE_OPERAND (t, 0))))); }
// :979
bool is_gimple_reg_rhs (tree t) { return get_gimple_rhs_class (TREE_CODE (t)) != GIMPLE_INVALID_RHS; }
```

`is_gimple_mem_rhs`（gimplify.cc:585，static）：

```c
if (is_gimple_reg_type (TREE_TYPE (t))) return is_gimple_val (t);
else return is_gimple_val (t) || is_gimple_lvalue (t);
```

辅助函数：

- `needs_to_live_in_memory`（tree.cc:11493）：`TREE_ADDRESSABLE || is_global_var || (RESULT_DECL && !DECL_BY_REFERENCE && aggregate_value_p)`。
- `decl_address_invariant_p`（tree.cc:3992）：
  - PARM、RESULT、LABEL、FUNCTION_DECL → 真；
  - VAR_DECL：若 static、extern、TLS、`DECL_CONTEXT == current_function_decl` 或 `decl_function_context == current_function_decl` → 真；
  - CONST_DECL：若 static、extern 或局部 → 真。
- `strip_invariant_refs`（tree.cc:12652）：ARRAY_REF 只有在 index 是 `is_gimple_constant` 且操作数 2、3 为空时才剥；COMPONENT_REF 只有在操作数 2 为空时才剥；其余 handled component 直接剥；否则返回 NULL。
- `get_gimple_rhs_class`（gimple.h:2666）查表 gimple.cc:2550-2585：
  - unary → UNARY；
  - binary、comparison、TRUTH_AND/OR/XOR → BINARY；
  - TRUTH_NOT → UNARY；
  - COND_EXPR 等 → TERNARY；
  - constant、declaration、reference、CONSTRUCTOR、OBJ_TYPE_REF、ADDR_EXPR、WITH_SIZE_EXPR、SSA_NAME → SINGLE；
  - 其余 → INVALID。
- C 默认没有 -fexceptions，所以 `tree_could_throw_p` 恒为 false。

---

## 4. 打印格式

### 4.1 三种 dump 的整体结构

- **gimple dump**：由 gimplify.cc:22160 调 `dump_function`（dumpfile.cc:2091），再调 `dump_function_to_file`。**没有** `;; Function` 头。
- **lower 和 cfg dump**：
  - 先打印 `pass_init_dump_file`（passes.cc:2222-2223）调用的 `dump_function_header`（tree-pretty-print.cc:5275-5307），格式为 `"\n;; Function %s (%s, funcdef_no=%d, decl_uid=%d, cgraph_uid=%d, symbol_order=%d)\n\n"`。用 `-nouid` 时省略 decl_uid 一项。
  - 然后是 pass 执行过程中的输出。对 cfg 来说依次是：`Removing…`/`Merging…` 消息，`;; N loops found`、`;;`、`;; Loop k`、`;;  header H, latch L`、`;;  depth D, outer O`、`;;  nodes: …`（cfgloop.cc:111-172），每块一行 `;; B succs { … }`（cfgloop.cc:43-60）。
  - 最后是函数体。函数体与上面的循环信息之间**没有空行**。

### 4.2 `dump_function_to_file`（tree-cfg.cc:8288-8557）

1. 若有 DECL_ATTRIBUTES 或 TYPE_ATTRIBUTES，先打印 `__attribute__((…))` 行。
2. `rettype` + `" %s %s("`，得到 `int f (`。参数按 `type name` 打印，用 `", "` 分隔，然后 `")\n"`。
   - `main(void)` 打印为 `main ()`；可变参数不打印 `...`；`char **argv` 打印为 `char * * argv`。
3. 若有 `PROP_gimple_lcf`（lower 和 cfg）：打印 `"{\n"`，然后每个 local decl 打印 `print_declaration(spc=2)` + `"\n"`（tree-pretty-print.cc:132）。
4. 若已有 CFG：有变量时打印 `"\n"`，然后每块调用 `dump_bb(file, bb, 2, flags)`，最后 `"}\n"`。
   - 否则若 body 是单个 GIMPLE_BIND（gimple dump）：`print_gimple_seq(body, 0)`。
   - 否则（lower）：有变量时打印 `"\n"`，然后 `print_gimple_seq(body, 2)`，最后 `"}\n"`。
5. 结尾打印 `"\n\n"`。

### 4.3 语句格式（gimple-pretty-print.cc）

- **`dump_gimple_seq`**（203）：每条语句先缩进 spc 再打印，语句之间换行；`print_gimple_seq`（223）在末尾补一个换行。lower dump 里 label 也缩进 2。
- **gassign**（633-695）：`lhs = rhs;`。有 volatile 操作时写成 `x ={v} y;`。
  - 2 个操作数时走 `dump_unary_rhs`（334-422）：
    - CONVERT、FLOAT、FIX_TRUNC → `(lhs 的类型) rhs`，当 `op_prio(rhs) < 14` 时 rhs 加括号；
    - NEGATE → `-x`，BIT_NOT → `~x`，TRUTH_NOT → `!x`；
    - ABS → `ABS_EXPR <x>`；
    - 单个操作数（decl、常量、引用、ADDR、CONSTRUCTOR）原样打印。
  - 3 个操作数时走 `dump_binary_rhs`（429-497）：`a op b`，当操作数的 `op_prio <= op_code_prio(code)` 时加括号；MIN/MAX 打印为 `MAX_EXPR <a, b>`。运算符符号见 tree-pretty-print.cc:4943，优先级见 4791。
  - 4 个操作数时 COND_EXPR 打印为 `a ? b : c`。
- **greturn**（702）：`return;` 或 `return x;`。
- **gcall**（888-1019）：
  - 有 lhs 时：`lhs =`[`{v}`] + 空格；然后是函数名（`print_call_name`，tree-pretty-print.cc:5113）+ `" ("` + 实参（以 `", "` 分隔）+ `")"` + `";"`；
  - 内部函数前加 `.`；
  - 可能的后缀：` [return slot optimization]`、` [tail call]` 等。
- **gswitch**（1026-1077）：`switch (x) <default: <D.1>, case 1: <D.2>, case 2 ... 3: <D.3>>`。在 cfg dump 中每个 label 后面还有 ` [INV]`。实测格式与你的预期一致。
- **gcond**（1084-1147）：`if (a > 0)`，后接 ` goto <D.1>;` 和 ` else goto <D.2>;`（仅当 label 非空时）。cfg 中 label 为空，所以只打印 `if (a != 0)`。
- **glabel**（1155）：`label:`；**ggoto**（1179）：`goto <D.n>;`。
- **gbind**（1195-1221）：
  - 先打 `{`；
  - 每个变量前打 `"\n"` 加 2 个空格，再 `print_declaration(var, spc)`（其内部再缩进 spc，所以嵌套 bind 中的变量缩进 4）；
  - 有变量时追加一个换行，再追加一个换行；
  - body 以 spc+2 缩进打印；
  - 最后换行并缩进 spc，打 `}`。
- **NOP** 打印为 `GIMPLE_NOP`；**PREDICT** 打印为 `// predicted {likely|unlikely} by <name> predictor.`（2925-2933，名字取自 predict.def）。

### 4.4 名字（tree-pretty-print.cc）

- **VAR/PARM**（2909，`dump_decl_name` 272-336）：有名字就打印名字；无名字时打印 `D.%u`（DECL_UID，CONST_DECL 为 `C.%u`）；`-nouid` 时打印 `D.xxxx`。
- **LABEL_DECL**（2853-2887）：
  - 有名字时打印名字；
  - 无名字且 `LABEL_DECL_UID != -1`（只有建了 CFG 才会分配）时打印 `<L%d>`；
  - 否则打印 `<D.%u>`。
- **RESULT_DECL** 打印为 `<retval>`。
- **SSA_NAME**（3939）：`[标识符]_版本号[(D)][(ab)]`。版本号每个函数从 1 开始（tree-ssanames.cc:153-160 预留了 0）。
- **带前缀的 temp**：格式 `"%s.%lu"`（defaults.h 的 ASM_PN_FORMAT），计数器是整个 TU 全局的 `tmp_var_id_num`（gimple-expr.cc:420）。例如 `iftmp.0`。
- **整数常量**（2629）：十进制，没有后缀；指针类型的常量打印为 `0B`。
- **`print_declaration`**（4615）：可能有 `static `/`extern ` 前缀，然后是类型、名字（数组为 `int a[4]`）、可选的 ` = init`，最后 `;`。
- DECL_UID 和 tmp 计数器都是全局的，很难逐字复现。建议对比时使用 `-fdump-tree-cfg-nouid`。

### 4.5 CFG 中块的打印

- `dump_bb`（cfghooks.cc:336）→ `gimple_dump_bb`（3154）。
- 块头（2954-2993）：`"  <bb %d> %s:\n"`。count 未初始化（cfg.cc:143）时 %s 为空，所以打印 `  <bb 2> :`。
- label 缩进 0，其余语句缩进 2（3117-3147）。
- 隐式边（3069-3110）：
  - gcond 块打印 `"    goto <bb T>; [INV]\n  else\n    goto <bb F>; [INV]\n"`；
  - 其他块：fallthru 边的目标不是 `next_bb` 时打印 `"  goto <bb N>; [INV]\n"`；
  - return 和 switch 不打印隐式边。
- 每块最后由 `dump_bb` 追加一个空行。
- `[INV]` 的来由：建 CFG 时边的 probability 尚未初始化（cfg.cc:284）。

### 4.6 实测（gcc-15，与 trunk 规则一致的部分）

```
void f (int a)
{
  <bb 2> :
  if (a != 0)
    goto <bb 3>; [INV]
  else
    goto <bb 4>; [INV]

  <bb 3> :
  // predicted unlikely by early return (on trees) predictor.
  goto <bb 5>; [INV]

  <bb 4> :
  g ();

  <bb 5> :
<L2>:
  return;

}
```

这里 label 的编号依次是 L0=`<D.4473>`、L1=`<D.4474>`、L2=return label。前两个是 artificial 且已无引用，在第 2 轮 `cleanup_dead_labels` 中被删除；L2 因 `-O0` 被设为非 artificial 而保留。