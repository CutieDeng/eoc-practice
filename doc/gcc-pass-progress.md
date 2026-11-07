# GCC -O1 流水线对照表：移植进展

更新：2026-11-08（release_ssa 完成后）。数据来源：`gcc-15 -O1 -fdump-tree-all -fdump-ipa-all -fdump-rtl-all` 与 `gcc-12` 同参数在 `gccast-plugin/tests/struct.c` 上实际产生的 dump 文件（只有真正执行的 pass 才产生 dump，gate 关闭的 pass 不出现），以及 `gcc-15 -O1 -Q --help=optimizers` 列出的 96 个启用的优化开关。对齐判据见 `src/component/gimple/o1-test.rkt`：每个夹具（basic.c、calls.c、struct.c、ccp.c）× 两个 GCC 版本（15.2.0、12.5.0）逐函数比较归一化后的 dump 文本。

状态含义：

- **完成**：已移植，4 夹具 × 2 版本 dump 对齐（测试覆盖）。
- **恒等**：未移植算法；夹具上 GCC 的该 pass 不改变函数，测试检查我们的输出与前一 pass 相同且与 GCC dump 一致。换夹具可能触发差异。
- **未移植**：尚未开始。
- **不比较**：pass 不产生可比较的函数 dump（只有诊断 / 汇总信息），或其效果在夹具上不可见；不在对齐范围。
- **范围外**：IPA 跨函数阶段、RTL 后端。本项目的后端是自有 aarch64 组件，不复刻 GCC 的 RTL 流水线。

## 1. 前端与降级阶段（-O0 dump 对齐）

| 阶段 | GCC 对应 | 状态 | 实现 |
|---|---|---|---|
| GENERIC 导入 | gccast 插件 `PLUGIN_PRE_GENERICIZE` 序列化 | 完成 | `gccast-plugin/`, `src/frontend/gccast/`, `src/component/gimple/import.rkt` |
| genericize | `c_genericize` | 完成（.original 对齐） | `genericize.rkt` |
| gimplify | `gimplify_function_tree`（`007t.gimple`） | 完成 | `gimplify.rkt` |
| omplower | `pass_lower_omp`（`010t.omplower`） | 不比较（无 OpenMP） | — |
| lower | `pass_lower_cf`（`011t.lower`） | 完成 | `lower.rkt` |
| eh | `pass_lower_eh`（`014t.eh`） | 不比较（C 无异常） | — |
| cfg | `pass_build_cfg`（`016t.cfg`） | 完成（含 profile 结构、位置模型） | `cfg.rkt` |
| ompexp | `pass_expand_omp` | 不比较 | — |
| fixup_cfg1 | `pass_fixup_cfg`（`022t.fixup_cfg1`） | 不比较（夹具上无变化） | — |
| ssa | `pass_build_ssa`（`023t.ssa`） | 完成（含 remove_unused_locals、use-list 时间戳） | `ssa.rkt`, `src/driver/ssa/construct.rkt` |

## 2. early GIMPLE 优化序列（`pass_build_ssa_passes` → `pass_local_optimization_passes`）

序号为 gcc-15 的 dump 编号；gcc-12 列给出对应编号或"无"。

| gcc-15 | gcc-12 | pass | 状态 | 说明 / 缺口 |
|---|---|---|---|---|
| 024t | 022t | walloca1 | 不比较 | 诊断 pass |
| 027t | 025t | waccess1 | 不比较 | 诊断 pass |
| 029t | 027t | nothrow | 不比较 | C 下所有函数已 nothrow |
| 031t | 029t | fixup_cfg2 | 完成 | `opt/fixup-cfg.rkt`：const/pure 调用重建虚拟操作数（update_stmt）、cleanup。未做：IPA 计数缩放、noreturn 修正、write-only 存储删除 |
| 032t | 030t | local-fnsummary1 | 范围外 | IPA 摘要 |
| 033t | 031t | einline | 恒等（dump 对齐） | 不内联，只输出 "Iterations: 0"；早期内联算法未移植（夹具中 -O1 无内联发生；method.cc 有内联，未在测试集） |
| 035t | 033t | early_objsz | 不比较 | 夹具无 `__builtin_object_size` |
| 036t | 034t | ccp1 | 完成 | `opt/ccp.rkt`：bit-level 格、substitute_and_fold；≤12 双工作表、≥13 常量→拷贝不 meet、≥14 simple_dce。未做：复杂类型、向量常量、builtin 折叠 |
| 037t | 035t | forwprop1 | 完成 | `opt/forwprop.rkt` + `opt/simplify.rkt`（match.pd 子集按夹具触发增量实现）|
| 038t | 036t | ethread | 恒等 | 需 backward threader + path ranger + threadupdate |
| 039t | 037t | esra | 恒等 | 需聚合体寄存器化夹具 |
| 040t | 038t | ealias | 恒等 | points-to 分析未移植；≥13 只有求解器进度行；12 还 dump 约束集合（测试排除 ealias@12） |
| 041t | 无 | phiprop1 | 恒等 | gcc-13 起 |
| 042t | 039t | fre1 | 完成 | `opt/vn.rkt` + `opt/alias.rkt`：RPO VN 非迭代、别名预言机子集。未做：表达式插入、value id、modref 摘要查询 |
| 044t | 041t | mergephi1 | 恒等 | |
| 045t | 042t | dse1 | 完成 | `opt/dse.rkt`。未做：字节级部分死存储、memset/memcpy |
| 046t | 043t | cddce1 | 完成 | `opt/dce.rkt`（-O1 下非 aggressive）；≥15 带 remove_unused_locals |
| 047t | 044t | phiopt1 | 恒等 | |
| 049t | 046t | iftoswitch | 恒等 | |
| 052t | 无 | sccopy1 | 恒等 | gcc-14 起 |
| 053t | 049t | profile_estimate | 完成 | `opt/profile.rkt` + `opt/sreal.rkt` + `opt/loops.rkt` + `opt/niter.rkt`。未做：伪出口边、`__builtin_expect`、hot/cold、不可约循环 |
| 054t | 050t | local-pure-const1 | 完成 | `opt/pure-const.rkt`。未做：异常/nothrow、malloc、builtin 表、PTA |
| 055t | 051t | modref1 | 完成 | `opt/modref.rkt`。未做：fnspec/builtin、static chain/retslot、IPA 模式、PTA |
| — | — | strip_predict_hints（early） | 完成 | `opt/release-ssa.rkt`：只删 PRED_TREE_EARLY_RETURN，TODO_cleanup_cfg；无单独 dump |
| 057t | 053t | release_ssa | 完成 | `opt/release-ssa.rkt`：版本压缩保持相对顺序、默认定义占位、remove_unused_locals。未做：释放版本号的复用（GCC free list） |
| 058t | 054t | local-fnsummary2 | 范围外 | |

## 3. IPA 阶段（`-fdump-ipa-all`，范围外）

000i cgraph / ipa-clones / type-inheritance，019i visibility，021i build_ssa_passes，030i opt_local_passes，059i remove_symbols，072i targetclone，076i free-fnsummary1，079i emutls，082i whole-program，083i profile_estimate（IPA），086i cdtor，089i fnsummary，090i inline，092i pure-const（IPA 传播），093i modref（IPA 传播），094i free-fnsummary2，095i static-var，096i single-use，099i simdclone。gcc-12 同序列（编号 018i–093i，无 bitint 相关差异）。

当前只做了 local 模式的 pure-const / modref（每函数、callee 先于 caller 的顺序）；IPA 传播（跨 SCC 的 pure-const、modref 摘要合并、inline、cdtor、static-var 等）未移植。夹具中 `twice`（static）在 090i inline 被内联后由 remove_symbols 删除，这是 optimized dump 中它消失的原因。

## 4. 晚期 GIMPLE 序列（`all_optimizations`，全部未移植）

| gcc-15 | gcc-12 | pass | gcc-15 | gcc-12 | pass |
|---|---|---|---|---|---|
| 100t | 094t | fixup_cfg3 | 160t | 150t | fix_loops |
| 107t | 101t | adjust_alignment | 161t | 151t | loop |
| 109t | 102t | ccp2 | 162t | 152t | loopinit |
| 110t | 103t | objsz1 | 165t | 154t | sccp |
| 112t | 105t | waccess2 | 168t | 158t | cddce2 |
| 113t | 无 | rebuild_frequencies1 | 169t | 159t | ivcanon |
| 115t | 107t | backprop | 173t | 162t | copyprop3 |
| 116t | 108t | phiprop2 / phiprop | 186t | 175t | cunroll |
| 117t | 109t | forwprop2 | 191t | 180t | ivopts |
| 118t | 110t | alias | 192t | 181t | lim4 |
| 119t | 111t | retslot | 193t | 182t | loopdone |
| 120t | 112t | fre3 | 197t | 186t | veclower21 |
| 121t | 113t | mergephi2 | 198t | 187t | switchlower1 |
| 125t | 116t | dse2 | 199t | 138t | sincos（12 在 ccp3 之后） |
| 126t | 117t | dce2 | 201t | 189t | reassoc2 |
| 127t | 118t | stdarg | 202t | 190t | slsr |
| 128t | 119t | cdce | 207t | 195t | dom3 |
| 129t | 120t | cselim | 208t | 196t | strlen1 |
| 130t | 121t | copyprop1 | 211t | 199t | ccp4 |
| 131t | 122t | ifcombine | 212t | 200t | wrestrict |
| 132t | 123t | mergephi3 | 213t | 201t | dse5 |
| 133t | 124t | phiopt2 | 214t | 210t | dce7（12 在 fab1 之后） |
| 135t | 126t | ch2 | 215t | 203t | forwprop4 |
| 136t | 127t | cplxlower1 | 216t | 204t | sink2 |
| 137t | 无 | bitintlower1 | 217t | 205t | phiopt4 |
| 138t | 128t | sra | 218t | 206t | fab1 |
| 140t | 130t | dom2 | 221t | 202t | cddce3 |
| 141t | 131t | copyprop2 | 222t | 无 | sccopy2 |
| 143t | 133t | reassoc1 | 224t | 211t | crited1 |
| 144t | 134t | dce3 | 226t | 213t | local-pure-const2 |
| 145t | 135t | forwprop3 | 227t | 214t | modref2 |
| 146t | 136t | phiopt3 | 228t | 215t | uncprop1 |
| 147t | 137t | ccp3 | 265t | 247t | nrv |
| 148t | 无 | pow | 266t | 248t | isel |
| 150t | 140t | laddress | 269t | 251t | waccess3 |
| 151t | 141t | lim2 | 270t | 252t | optimized |
| 152t | 142t | walloca2 | 368t–370t | 339t–341t | statistics / earlydebug / debug |
| 154t | 144t | sink1 | | | |
| 158t | 148t | dse3 | | | |
| 159t | 149t | dce4 | | | |

已有的 pass 实现可以直接复用于其中的重复实例：ccp2/3/4（ccp.rkt）、forwprop2/3/4、fre3、dse2/3/5、dce2/3/4/7、cddce2/3（≥O2 的 aggressive 模式未做）、phiprop2、mergephi2/3、sccopy2、local-pure-const2、modref2、profile 重建（rebuild_frequencies1）。全新的：alias（PTA）、retslot、stdarg、cdce、cselim、copyprop、ifcombine、phiopt、ch、cplxlower、bitintlower、sra、dom、reassoc、pow、laddress、lim、sink、循环优化族（fix_loops…loopdone：sccp、ivcanon、cunroll、ivopts）、veclower、switchlower、sincos、slsr、strlen、wrestrict、fab、crited、uncprop、nrv、isel。

## 5. RTL 后端（范围外）

gcc-15 在 -O1 执行 48 个 RTL pass（271r expand … 367r dfinish：vregs、jump、subreg、cse1、fwprop、hardreg_pre、ce1/2/3、loop2 族、dse、auto_inc_dec、combine、split、mode_sw、ldp_fusion、ira、reload、postreload、cmpelim、pro_and_epilogue、cprop_hardreg、rtl_dce、bbro、shorten、final 等）。本项目的代码生成走自有 aarch64 组件，不对齐 RTL dump。

## 6. -O1 启用的优化开关（gcc-15，96 项）与对应状态

| 开关 | 作用层 / pass | 状态 |
|---|---|---|
| -ftree-ccp, -ftree-bit-ccp | ccp1（及 ccp2–4） | 完成（ccp1） |
| -ftree-forwprop | forwprop1（及 2–4） | 完成（forwprop1） |
| -ftree-fre | fre1（及 fre3） | 完成（fre1） |
| -ftree-dse | dse1（及 2/3/5） | 完成（dse1） |
| -ftree-dce, -ftree-builtin-call-dce | cddce1、dce2–7、cdce | 完成（cddce1）；cdce 未移植 |
| -fguess-branch-probability | profile_estimate | 完成 |
| -fipa-pure-const | local-pure-const1/2 + IPA pure-const | 完成（local 1） |
| -fipa-modref, -fipa-strict-aliasing | modref1/2 + IPA modref | 完成（local 1；-O1 无 strict aliasing，alias set 恒 0） |
| -ftree-phiprop | phiprop1/2 | 恒等 |
| -fthread-jumps | ethread（及 thread 族） | 恒等（threader 未移植） |
| -ftree-sra | esra、sra | 恒等 / 未移植 |
| -ftree-pta | ealias、alias | 恒等 / 未移植（PTA） |
| -fssa-phiopt | phiopt1–4 | 恒等 / 未移植 |
| -fearly-inlining, -finline, -finline-functions-called-once, -finline-atomics, -fno-inline-stringops | einline、IPA inline | 未移植 |
| -fipa-profile, -fipa-reference, -fipa-reference-addressable, -fipa-stack-alignment | IPA | 范围外 |
| -ftree-copy-prop | copyprop1–3 | 未移植 |
| -ftree-dominator-opts | dom2/3 | 未移植 |
| -ftree-reassoc | reassoc1/2 | 未移植 |
| -ftree-ch | ch2 | 未移植 |
| -ftree-loop-optimize, -ftree-loop-im, -ftree-loop-ivcanon, -ftree-scev-cprop, -fivopts, -faggressive-loop-optimizations, -fsplit-ivs-in-unroller, -fmove-loop-invariants, -fmove-loop-stores | lim2/4、ivcanon、sccp、ivopts、cunroll | 未移植（loops.rkt / niter.rkt 只有 flow_loops_find 与 affine IV 子集） |
| -ftree-sink | sink1/2 | 未移植 |
| -ftree-slsr | slsr | 未移植 |
| -fssa-backprop | backprop | 未移植 |
| -fstdarg-opt | stdarg | 未移植 |
| -fbit-tests, -fjump-tables | switchlower1 | 未移植 |
| -fallocation-dce, -fmalloc-dce, -flifetime-dse | dce/dse 的分配配对、生命期死存储 | 未移植 |
| -fprintf-return-value, -fmath-errno, -ffp-int-builtin-inexact, -fsigned-zeros, -ftrapping-math | 折叠语义（match.pd / builtin） | 部分：折叠子集按夹具实现 |
| -ftree-ter, -ftree-coalesce-vars | expand 前的 TER / 变量合并（out-of-SSA） | 未移植 |
| -fsemantic-interposition, -ftoplevel-reorder, -fplt | 符号 / 可见性语义 | 简化模型（本单元定义即绑定；pure-const.rkt） |
| -fdce, -fdse, -fforward-propagate, -fcprop-registers, -fcombine-stack-adjustments, -fcompare-elim, -fauto-inc-dec, -fbranch-count-reg, -fdefer-pop, -ffunction-cse, -fgcse-lm, -fif-conversion, -fif-conversion2, -fira-*, -fomit-frame-pointer, -fpeephole, -freorder-blocks, -fsched-*, -fschedule-fusion, -fshrink-wrap, -fshrink-wrap-separate, -fsplit-wide-types, -ffold-mem-offsets, -fstrict-volatile-bitfields, -funwind-tables, -fasynchronous-unwind-tables, -mearly-ldp-fusion, -mlate-ldp-fusion | RTL 后端 | 范围外 |

## 7. 统计

- early GIMPLE 序列（第 2 节）共 27 个有 dump 的 pass：完成 11（fixup_cfg2、ccp1、forwprop1、fre1、dse1、cddce1、profile_estimate、local-pure-const1、modref1、strip_predict_hints、release_ssa），恒等 9（einline、ethread、esra、ealias、phiprop1、mergephi1、phiopt1、iftoswitch、sccopy1），不比较 / 范围外 7。early 序列到 release_ssa 为止全部 dump 对齐。
- 晚期 GIMPLE 序列（第 4 节）76 个 pass 全部未移植；其中 19 个是已实现 pass 的重复实例。
- 测试：`o1-test.rkt` 172 项（4 夹具 × 2 版本 × 阶段）。
- 阶段顺序：测试驱动按 GCC 的顺序先对全部函数 gimplify→SSA，再逐函数跑 early 序列；调用 ECF 位按被调声明动态查询。
