# GCC -O1 流水线对照表：移植进展（完整清单）

更新：2026-11-08。

## 0. 数据来源与口径

- **pass 清单**：`gcc-15 -O1 -fdump-passes` 与 `gcc-12 -O1 -fdump-passes`（Homebrew gcc 15.2.0 / gcc@12 12.5.0）打印的完整 pass 树，含 gate 关闭的 pass。这是 GCC 对 `passes.def` 的实例化结果。此前版本的表只列了夹具实际产生 dump 文件的 pass，漏掉了无 dump 名的 `*` pass 与 gate 关闭的 pass。
- **passes.def 规模**（`gcc.git` 的 releases/gcc-15 与 releases/gcc-12 分支）：NEXT_PASS / INSERT_PASS 条目 426（gcc-15）、406（gcc-12）；不同 pass 名 294（gcc-15）。-fdump-passes 展开为 388 个实例（gcc-15）、360 个（gcc-12）。
- **优化开关**：`gcc-15 -O{0,1,2,3} -Q --help=optimizers`：275 个开关；-O0 启用 54，-O1 新增 43（共 96），-O2 再增 46（共 142），-O3 再增 14（共 156），135 个在 -O0 到 -O3 下都不默认启用。另有 316 个 `--param`，不在本表。
- **对齐判据**：`src/component/gimple/o1-test.rkt`，4 夹具（basic.c、calls.c、struct.c、ccp.c）× 2 版本逐函数比较归一化 dump。状态：**完成** = 已移植且对齐；**恒等** = 未移植算法、夹具上 GCC 不改变函数、dump 对齐（换夹具可能触发差异）；**未移植**；**不比较** = 无函数 dump 或夹具上无效果；**范围外** = IPA 跨函数 / RTL 后端（后端为自有 aarch64）；**gate 关闭** = -O1 下 GCC 不执行。

## 1. 统计（gcc-15 pass 树实例，-O1）

| 类别 | ON | OFF | 完成 | 恒等 | 未移植（ON） |
|---|---|---|---|---|---|
| GIMPLE（tree-） | 141 | 85 | 20 | 16 | 93 |
| IPA（ipa-） | 19 | 17 | 4 | 0 | 0 |
| RTL（rtl-） | 49 | 45 | 0 | 0 | 0 |
| 无 dump 名（*） | 20 | 12 | 1 | 0 | 0 |
| 合计 | 229 | 159 | 25 | 16 | 93 |

完成的 pass 覆盖 early 序列（`ipa-build_ssa_passes` + `ipa-opt_local_passes`）到 `tree-release_ssa`，IPA 阶段的子集（inline into all callers、static 变量 readonly 发现、符号移除；见 `opt/ipa.rkt`、`opt/inline.rkt`），以及 `all_optimizations` 从 fixup_cfg3 到 dce2；之后的晚期序列（含循环优化、向量化降级、out-of-SSA）未移植。gimplify（`-fdump-tree-gimple`，不是 pass）与 genericize 已完成并对齐。

## 2. 夹具上真正改变代码的晚期 pass（gcc-15，release_ssa 之后）

按 dump 两两比较函数体得到；没有列出的晚期 pass 在该夹具上是恒等的（对齐时只需输出与前一 pass 相同的 dump）。这是晚期序列的移植优先级依据。

| 夹具 | 改变代码的 pass |
|---|---|
| basic.c | `135t.ch2`, `140t.dom2`, `141t.copyprop2`, `151t.lim2`, `161t.loop`, `162t.loopinit`, `165t.sccp`, `168t.cddce2`, `169t.ivcanon`, `186t.cunroll`, `194t.no_loop`, `197t.veclower21`, `201t.reassoc2`, `207t.dom3`, `211t.ccp4`, `214t.dce7` |
| calls.c | `100t.fixup_cfg3`, `117t.forwprop2`, `126t.dce2`, `154t.sink1`, `197t.veclower21`, `224t.crited1`, `270t.optimized` |
| struct.c | `109t.ccp2`, `129t.cselim`, `140t.dom2`, `143t.reassoc1`, `151t.lim2`, `154t.sink1`, `162t.loopinit`, `191t.ivopts`, `193t.loopdone`, `198t.switchlower1`, `207t.dom3`, `208t.strlen1`, `211t.ccp4`, `215t.forwprop4`, `224t.crited1`, `270t.optimized` |
| ccp.c | `135t.ch2`, `151t.lim2`, `154t.sink1`, `161t.loop`, `162t.loopinit`, `165t.sccp`, `168t.cddce2`, `191t.ivopts`, `193t.loopdone`, `194t.no_loop`, `197t.veclower21`, `207t.dom3`, `208t.strlen1`, `215t.forwprop4`, `216t.sink2`, `224t.crited1`, `270t.optimized` |

合并去重后需要移植的晚期 pass（按出现次数）：`lim2`×3, `loopinit`×3, `veclower21`×3, `dom3`×3, `sink1`×3, `crited1`×3, `optimized`×3, `ch2`×2, `dom2`×2, `loop`×2, `sccp`×2, `cddce2`×2, `no_loop`×2, `ccp4`×2, `ivopts`×2, `loopdone`×2, `strlen1`×2, `forwprop4`×2, `copyprop2`×1, `ivcanon`×1, `cunroll`×1, `reassoc2`×1, `dce7`×1, `fixup_cfg3`×1, `forwprop2`×1, `dce2`×1, `ccp2`×1, `cselim`×1, `reassoc1`×1, `switchlower1`×1, `sink2`×1。`optimized` 的变化来自 pass_cleanup_cfg_post_optimizing 与 out-of-SSA 前的整理。

## 3. 完整 pass 树（gcc-15 顺序；缩进 = 嵌套层级；gcc-12 列 = 该 pass 在 gcc-12 的 gate 状态，`无` = gcc-12 没有该 pass）

| # | pass（gcc-15） | gcc-15 | gcc-12 | 状态 |
|---|---|---|---|---|
| 1 | 　`*warn_unused_result` | ON | ON | 不比较（诊断） |
| 2 | 　`*diagnose_omp_blocks` | OFF | OFF | gate 关闭（-O1 不执行） |
| 3 | 　`*diagnose_tm_blocks` | OFF | OFF | gate 关闭（-O1 不执行） |
| 4 | 　`tree-omp_oacc_kernels_decompose` | OFF | OFF | gate 关闭（-O1 不执行） |
| 5 | 　`tree-omplower` | ON | ON | 不比较（无 OpenMP） |
| 6 | 　`tree-lower` | ON | ON | 完成 |
| 7 | 　`tree-tmlower` | OFF | OFF | gate 关闭（-O1 不执行） |
| 8 | 　`tree-ehopt` | OFF | OFF | gate 关闭（-O1 不执行） |
| 9 | 　`tree-eh` | ON | ON | 不比较（C 无异常） |
| 10 | 　`tree-coro-lower-builtins` | OFF | OFF | gate 关闭（-O1 不执行） |
| 11 | 　`tree-cfg` | ON | ON | 完成 |
| 12 | 　`*warn_function_return` | ON | ON | 不比较（诊断） |
| 13 | 　`tree-coro-early-expand-ifns` | OFF | OFF | gate 关闭（-O1 不执行） |
| 14 | 　`tree-ompexp` | ON | ON | 不比较 |
| 15 | 　`*build_cgraph_edges` | ON | ON | 范围外（cgraph） |
| 16 | 　`*free_lang_data` | ON | ON | 不比较 |
| 17 | 　`ipa-visibility` | ON | ON | 范围外 |
| 18 | 　`ipa-strubm` | OFF | 无 | gate 关闭（-O1 不执行） |
| 19 | 　`ipa-build_ssa_passes` | ON | ON | （分组） |
| 20 | 　　`tree-fixup_cfg1` | ON | ON | 不比较（SSA 前，夹具无变化） |
| 21 | 　　`tree-ssa` | ON | ON | 完成 |
| 22 | 　　`tree-walloca1` | ON | ON | 不比较（诊断） |
| 23 | 　　`tree-warn-printf` | OFF | OFF | gate 关闭（-O1 不执行） |
| 24 | 　　`*nonnullcmp` | OFF | OFF | gate 关闭（-O1 不执行） |
| 25 | 　　`tree-early_uninit` | OFF | OFF | gate 关闭（-O1 不执行） |
| 26 | 　　`tree-waccess1` | ON | ON | 不比较（诊断） |
| 27 | 　　`tree-ubsan` | OFF | OFF | gate 关闭（-O1 不执行） |
| 28 | 　　`tree-nothrow` | ON | ON | 不比较（C 下恒 nothrow） |
| 29 | 　　`*rebuild_cgraph_edges` | ON | ON | 范围外（cgraph） |
| 30 | 　`ipa-opt_local_passes` | ON | ON | （分组） |
| 31 | 　　`tree-fixup_cfg2` | ON | ON | 完成 |
| 32 | 　　`*rebuild_cgraph_edges` | ON | ON | 范围外 / 不比较 |
| 33 | 　　`tree-local-fnsummary1` | ON | ON | 范围外（IPA 摘要） |
| 34 | 　　`tree-einline` | ON | ON | 恒等（dump 对齐，不内联） |
| 35 | 　　`*infinite-recursion` | OFF | OFF | gate 关闭（-O1 不执行） |
| 36 | 　　`tree-early_optimizations` | ON | ON | （分组） |
| 37 | 　　　`*remove_cgraph_callee_edges` | ON | ON | 范围外（cgraph） |
| 38 | 　　　`tree-early_objsz` | ON | ON | 不比较（夹具无 object_size） |
| 39 | 　　　`tree-ccp1` | ON | ON | 完成 |
| 40 | 　　　`tree-forwprop1` | ON | ON | 完成 |
| 41 | 　　　`tree-ethread` | ON | ON | 恒等 |
| 42 | 　　　`tree-esra` | ON | ON | 恒等 |
| 43 | 　　　`tree-ealias` | ON | ON | 恒等（PTA 未移植） |
| 44 | 　　　`tree-phiprop1` | ON | 无 | 恒等 |
| 45 | 　　　`tree-fre1` | ON | ON | 完成 |
| 46 | 　　　`tree-evrp` | OFF | OFF | gate 关闭（-O1 不执行） |
| 47 | 　　　`tree-mergephi1` | ON | ON | 恒等 |
| 48 | 　　　`tree-dse1` | ON | ON | 完成 |
| 49 | 　　　`tree-cddce1` | ON | ON | 完成 |
| 50 | 　　　`tree-phiopt1` | ON | ON | 恒等 |
| 51 | 　　　`tree-tailr1` | OFF | OFF | gate 关闭（-O1 不执行） |
| 52 | 　　　`tree-iftoswitch` | ON | ON | 恒等 |
| 53 | 　　　`tree-switchconv` | OFF | OFF | gate 关闭（-O1 不执行） |
| 54 | 　　　`tree-ehcleanup1` | OFF | OFF | gate 关闭（-O1 不执行） |
| 55 | 　　　`tree-sccopy1` | ON | 无 | 恒等 |
| 56 | 　　　`tree-profile_estimate` | ON | ON | 完成 |
| 57 | 　　　`tree-local-pure-const1` | ON | ON | 完成 |
| 58 | 　　　`tree-modref1` | ON | ON | 完成 |
| 59 | 　　　`tree-fnsplit` | OFF | OFF | gate 关闭（-O1 不执行） |
| 60 | 　　　`*strip_predict_hints` | ON | ON | 完成（early 实例） |
| 61 | 　　`tree-release_ssa` | ON | ON | 完成 |
| 62 | 　　`*rebuild_cgraph_edges` | ON | ON | 范围外 / 不比较 |
| 63 | 　　`tree-local-fnsummary2` | ON | ON | 范围外（IPA 摘要） |
| 64 | 　`ipa-remove_symbols` | ON | ON | 完成（子集：被完全内联的 static 函数移出单元） |
| 65 | 　`ipa-strub` | OFF | 无 | gate 关闭（-O1 不执行） |
| 66 | 　`ipa-ipa_oacc` | OFF | OFF | gate 关闭（-O1 不执行） |
| 67 | 　　`ipa-pta1` | OFF | OFF | gate 关闭（-O1 不执行） |
| 68 | 　　`ipa-ipa_oacc_kernels` | ON | ON | 范围外（IPA） |
| 69 | 　　　`tree-oacc_kernels` | OFF | OFF | gate 关闭（-O1 不执行） |
| 70 | 　　　　`tree-ch1` | ON | ON | 未移植 |
| 71 | 　　　　`tree-fre2` | ON | ON | 未移植（可复用已实现的 fre） |
| 72 | 　　　　`tree-lim1` | ON | ON | 未移植 |
| 73 | 　　　　`tree-dom1` | ON | ON | 未移植 |
| 74 | 　　　　`tree-dce1` | ON | ON | 未移植（可复用已实现的 dce） |
| 75 | 　　　　`tree-parloops1` | OFF | OFF | gate 关闭（-O1 不执行） |
| 76 | 　　　　`tree-ompexpssa1` | ON | ON | 未移植 |
| 77 | 　　　　`*rebuild_cgraph_edges` | ON | ON | 范围外 / 不比较 |
| 78 | 　`ipa-targetclone` | ON | ON | 范围外（IPA） |
| 79 | 　`ipa-afdo` | OFF | OFF | gate 关闭（-O1 不执行） |
| 80 | 　`ipa-profile` | OFF | OFF | gate 关闭（-O1 不执行） |
| 81 | 　　`tree-feedback_fnsplit` | OFF | OFF | gate 关闭（-O1 不执行） |
| 82 | 　`ipa-free-fnsummary1` | ON | ON | 范围外（IPA） |
| 83 | 　`ipa-increase_alignment` | OFF | OFF | gate 关闭（-O1 不执行） |
| 84 | 　`ipa-tmipa` | OFF | OFF | gate 关闭（-O1 不执行） |
| 85 | 　`ipa-emutls` | ON | ON | 范围外（IPA） |
| 86 | 　`ipa-analyzer` | OFF | OFF | gate 关闭（-O1 不执行） |
| 87 | 　`ipa-odr` | OFF | OFF | gate 关闭（-O1 不执行） |
| 88 | 　`ipa-whole-program` | ON | ON | 范围外（IPA） |
| 89 | 　`ipa-profile_estimate` | ON | ON | 范围外（IPA） |
| 90 | 　`ipa-icf` | OFF | OFF | gate 关闭（-O1 不执行） |
| 91 | 　`ipa-devirt` | OFF | OFF | gate 关闭（-O1 不执行） |
| 92 | 　`ipa-cdtor` | ON | ON | 范围外（IPA） |
| 93 | 　`ipa-cp` | OFF | OFF | gate 关闭（-O1 不执行） |
| 94 | 　`ipa-sra` | OFF | OFF | gate 关闭（-O1 不执行） |
| 95 | 　`ipa-fnsummary` | ON | ON | 完成（子集：无谓词的 size 模型） |
| 96 | 　`ipa-inline` | ON | ON | 完成（子集：inline into all callers；小函数路径 -O1 下对非 inline 声明关闭） |
| 97 | 　`ipa-locality-clone` | OFF | 无 | gate 关闭（-O1 不执行） |
| 98 | 　`ipa-pure-const` | ON | ON | 范围外（IPA 传播未移植） |
| 99 | 　`ipa-modref` | ON | ON | 范围外（IPA 传播未移植） |
| 100 | 　`ipa-free-fnsummary2` | ON | ON | 范围外（IPA） |
| 101 | 　`ipa-static-var` | ON | ON | 完成（子集：ipa_discover_variable_flags 的 readonly 发现） |
| 102 | 　`ipa-single-use` | ON | ON | 范围外（IPA） |
| 103 | 　`ipa-comdats` | OFF | OFF | gate 关闭（-O1 不执行） |
| 104 | 　`ipa-pta2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 105 | 　`ipa-simdclone` | ON | ON | 范围外（IPA） |
| 106 | 　`tree-fixup_cfg3` | ON | ON | 完成（含 inline transform 的 TODO：cleanup、update_ssa、remove_unused_locals） |
| 107 | 　`tree-ehdisp` | OFF | OFF | gate 关闭（-O1 不执行） |
| 108 | 　`tree-oaccloops` | OFF | OFF | gate 关闭（-O1 不执行） |
| 109 | 　`tree-omp_oacc_neuter_broadcast` | OFF | OFF | gate 关闭（-O1 不执行） |
| 110 | 　`tree-oaccdevlow` | OFF | OFF | gate 关闭（-O1 不执行） |
| 111 | 　`tree-ompdevlow` | ON | ON | 未移植 |
| 112 | 　`tree-omptargetlink` | OFF | OFF | gate 关闭（-O1 不执行） |
| 113 | 　`tree-adjust_alignment` | ON | ON | 恒等 |
| 114 | 　`tree-hardcfr` | OFF | 无 | gate 关闭（-O1 不执行） |
| 115 | 　`*all_optimizations` | ON | ON | 范围外 / 不比较 |
| 116 | 　　`*remove_cgraph_callee_edges` | ON | ON | 范围外 / 不比较 |
| 117 | 　　`*strip_predict_hints` | ON | ON | 范围外 / 不比较 |
| 118 | 　　`tree-ccp2` | ON | ON | 完成（nonzero_p：≥15 输出 Global Exported 范围） |
| 119 | 　　`tree-objsz1` | ON | ON | 恒等 |
| 120 | 　　`tree-post_ipa_warn1` | OFF | OFF | gate 关闭（-O1 不执行） |
| 121 | 　　`tree-waccess2` | ON | ON | 不比较（pointer_query 计数器随 GCC 内部缓存变化） |
| 122 | 　　`tree-rebuild_frequencies1` | ON | 无 | 完成（只有 "Profile is consistent"） |
| 123 | 　　`tree-cunrolli` | OFF | OFF | gate 关闭（-O1 不执行） |
| 124 | 　　`tree-backprop` | ON | ON | 恒等 |
| 125 | 　　`tree-phiprop2` | ON | 无 | 恒等 |
| 126 | 　　`tree-forwprop2` | ON | ON | 完成（复用 forwprop） |
| 127 | 　　`tree-alias` | ON | ON | 恒等（PTA 未移植；≥13 求解器进度行；12 排除） |
| 128 | 　　`tree-retslot` | ON | ON | 恒等 |
| 129 | 　　`tree-fre3` | ON | ON | 完成（复用 fre） |
| 130 | 　　`tree-mergephi2` | ON | ON | 恒等 |
| 131 | 　　`tree-threadfull1` | OFF | OFF | gate 关闭（-O1 不执行） |
| 132 | 　　`tree-vrp1` | OFF | OFF | gate 关闭（-O1 不执行） |
| 133 | 　　`tree-bounds` | OFF | 无 | gate 关闭（-O1 不执行） |
| 134 | 　　`tree-dse2` | ON | ON | 完成（复用 dse） |
| 135 | 　　`tree-dce2` | ON | ON | 完成（复用 dce；remove_unused_locals 仅 ≥15） |
| 136 | 　　`tree-stdarg` | ON | ON | 未移植 |
| 137 | 　　`tree-cdce` | ON | ON | 未移植 |
| 138 | 　　`tree-cselim` | ON | ON | 未移植 |
| 139 | 　　`tree-copyprop1` | ON | ON | 未移植 |
| 140 | 　　`tree-ifcombine` | ON | ON | 未移植 |
| 141 | 　　`tree-mergephi3` | ON | ON | 未移植（可复用已实现的 mergephi） |
| 142 | 　　`tree-phiopt2` | ON | ON | 未移植 |
| 143 | 　　`tree-tailr2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 144 | 　　`tree-ch2` | ON | ON | 未移植 |
| 145 | 　　`tree-cplxlower1` | ON | ON | 未移植 |
| 146 | 　　`tree-bitintlower1` | ON | 无 | 未移植 |
| 147 | 　　`tree-sra` | ON | ON | 未移植 |
| 148 | 　　`tree-thread1` | OFF | OFF | gate 关闭（-O1 不执行） |
| 149 | 　　`tree-dom2` | ON | ON | 未移植 |
| 150 | 　　`tree-copyprop2` | ON | ON | 未移植 |
| 151 | 　　`tree-isolate-paths` | OFF | OFF | gate 关闭（-O1 不执行） |
| 152 | 　　`tree-reassoc1` | ON | ON | 未移植 |
| 153 | 　　`tree-dce3` | ON | ON | 未移植（可复用已实现的 dce） |
| 154 | 　　`tree-forwprop3` | ON | ON | 未移植（可复用已实现的 forwprop） |
| 155 | 　　`tree-phiopt3` | ON | ON | 未移植 |
| 156 | 　　`tree-ccp3` | ON | ON | 未移植（可复用已实现的 ccp） |
| 157 | 　　`tree-pow` | ON | 无 | 未移植 |
| 158 | 　　`tree-bswap` | OFF | OFF | gate 关闭（-O1 不执行） |
| 159 | 　　`tree-laddress` | ON | ON | 未移植 |
| 160 | 　　`tree-lim2` | ON | ON | 未移植 |
| 161 | 　　`tree-walloca2` | ON | ON | 未移植 |
| 162 | 　　`tree-pre` | OFF | OFF | gate 关闭（-O1 不执行） |
| 163 | 　　`tree-sink1` | ON | ON | 未移植 |
| 164 | 　　`tree-sancov1` | OFF | OFF | gate 关闭（-O1 不执行） |
| 165 | 　　`tree-asan1` | OFF | OFF | gate 关闭（-O1 不执行） |
| 166 | 　　`tree-tsan1` | OFF | OFF | gate 关闭（-O1 不执行） |
| 167 | 　　`tree-dse3` | ON | ON | 未移植（可复用已实现的 dse） |
| 168 | 　　`tree-dce4` | ON | ON | 未移植（可复用已实现的 dce） |
| 169 | 　　`tree-fix_loops` | ON | ON | 未移植 |
| 170 | 　　`tree-loop` | ON | ON | 未移植 |
| 171 | 　　　`tree-loopinit` | ON | ON | 未移植 |
| 172 | 　　　`tree-unswitch` | OFF | OFF | gate 关闭（-O1 不执行） |
| 173 | 　　　`tree-lsplit` | OFF | OFF | gate 关闭（-O1 不执行） |
| 174 | 　　　`tree-sccp` | ON | ON | 未移植 |
| 175 | 　　　`tree-lversion` | OFF | OFF | gate 关闭（-O1 不执行） |
| 176 | 　　　`tree-unrolljam` | OFF | OFF | gate 关闭（-O1 不执行） |
| 177 | 　　　`tree-cddce2` | ON | ON | 未移植（可复用已实现的 cddce） |
| 178 | 　　　`tree-ivcanon` | ON | ON | 未移植 |
| 179 | 　　　`tree-ldist` | OFF | OFF | gate 关闭（-O1 不执行） |
| 180 | 　　　`tree-crc` | OFF | 无 | gate 关闭（-O1 不执行） |
| 181 | 　　　`tree-linterchange` | OFF | OFF | gate 关闭（-O1 不执行） |
| 182 | 　　　`tree-copyprop3` | ON | ON | 未移植 |
| 183 | 　　　`tree-graphite0` | OFF | OFF | gate 关闭（-O1 不执行） |
| 184 | 　　　　`tree-graphite` | OFF | OFF | gate 关闭（-O1 不执行） |
| 185 | 　　　　`tree-lim3` | ON | ON | 未移植 |
| 186 | 　　　　`tree-copyprop4` | ON | ON | 未移植 |
| 187 | 　　　　`tree-dce5` | ON | ON | 未移植（可复用已实现的 dce） |
| 188 | 　　　`tree-parloops2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 189 | 　　　`tree-ompexpssa2` | ON | ON | 未移植 |
| 190 | 　　　`tree-ch_vect` | OFF | OFF | gate 关闭（-O1 不执行） |
| 191 | 　　　`tree-ifcvt` | OFF | OFF | gate 关闭（-O1 不执行） |
| 192 | 　　　`tree-vect` | OFF | OFF | gate 关闭（-O1 不执行） |
| 193 | 　　　　`tree-dce6` | ON | ON | 未移植（可复用已实现的 dce） |
| 194 | 　　　`tree-pcom` | OFF | OFF | gate 关闭（-O1 不执行） |
| 195 | 　　　`tree-cunroll` | ON | ON | 未移植 |
| 196 | 　　　`*pre_slp_scalar_cleanup` | OFF | OFF | gate 关闭（-O1 不执行） |
| 197 | 　　　　`tree-fre4` | OFF | OFF | gate 关闭（-O1 不执行） |
| 198 | 　　　　`tree-dse4` | ON | ON | 未移植（可复用已实现的 dse） |
| 199 | 　　　`tree-slp1` | OFF | OFF | gate 关闭（-O1 不执行） |
| 200 | 　　　`tree-aprefetch` | OFF | OFF | gate 关闭（-O1 不执行） |
| 201 | 　　　`tree-ivopts` | ON | ON | 未移植 |
| 202 | 　　　`tree-lim4` | ON | ON | 未移植 |
| 203 | 　　　`tree-loopdone` | ON | ON | 未移植 |
| 204 | 　　`tree-no_loop` | OFF | OFF | gate 关闭（-O1 不执行） |
| 205 | 　　　`tree-slp2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 206 | 　　`tree-simduid1` | OFF | OFF | gate 关闭（-O1 不执行） |
| 207 | 　　`tree-veclower21` | ON | ON | 未移植 |
| 208 | 　　`tree-switchlower1` | ON | ON | 未移植 |
| 209 | 　　`tree-sincos` | ON | ON | 未移植 |
| 210 | 　　`tree-recip` | OFF | OFF | gate 关闭（-O1 不执行） |
| 211 | 　　`tree-reassoc2` | ON | ON | 未移植 |
| 212 | 　　`tree-slsr` | ON | ON | 未移植 |
| 213 | 　　`tree-split-paths` | OFF | OFF | gate 关闭（-O1 不执行） |
| 214 | 　　`tree-tracer` | OFF | OFF | gate 关闭（-O1 不执行） |
| 215 | 　　`tree-fre5` | OFF | OFF | gate 关闭（-O1 不执行） |
| 216 | 　　`tree-thread2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 217 | 　　`tree-dom3` | ON | ON | 未移植 |
| 218 | 　　`tree-strlen1` | ON | ON | 未移植 |
| 219 | 　　`tree-threadfull2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 220 | 　　`tree-vrp2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 221 | 　　`tree-ccp4` | ON | ON | 未移植（可复用已实现的 ccp） |
| 222 | 　　`tree-wrestrict` | ON | ON | 未移植 |
| 223 | 　　`tree-dse5` | ON | ON | 未移植（可复用已实现的 dse） |
| 224 | 　　`tree-dce7` | ON | ON | 未移植（可复用已实现的 dce） |
| 225 | 　　`tree-forwprop4` | ON | ON | 未移植（可复用已实现的 forwprop） |
| 226 | 　　`tree-sink2` | ON | ON | 未移植 |
| 227 | 　　`tree-phiopt4` | ON | ON | 未移植 |
| 228 | 　　`tree-fab1` | ON | ON | 未移植 |
| 229 | 　　`tree-widening_mul` | OFF | OFF | gate 关闭（-O1 不执行） |
| 230 | 　　`tree-store-merging` | OFF | OFF | gate 关闭（-O1 不执行） |
| 231 | 　　`tree-cddce3` | ON | ON | 未移植（可复用已实现的 cddce） |
| 232 | 　　`tree-sccopy2` | ON | 无 | 未移植（可复用已实现的 sccopy） |
| 233 | 　　`tree-tailc` | OFF | OFF | gate 关闭（-O1 不执行） |
| 234 | 　　`tree-crited1` | ON | ON | 未移植 |
| 235 | 　　`tree-uninit1` | OFF | OFF | gate 关闭（-O1 不执行） |
| 236 | 　　`tree-local-pure-const2` | ON | ON | 未移植（可复用已实现的 local-pure-const） |
| 237 | 　　`tree-modref2` | ON | ON | 未移植（可复用已实现的 modref） |
| 238 | 　　`tree-uncprop1` | ON | ON | 未移植 |
| 239 | 　`*all_optimizations_g` | OFF | OFF | gate 关闭（-O1 不执行） |
| 240 | 　　`*remove_cgraph_callee_edges` | ON | ON | 范围外 / 不比较 |
| 241 | 　　`*strip_predict_hints` | ON | ON | 范围外 / 不比较 |
| 242 | 　　`tree-cplxlower2` | ON | ON | 未移植 |
| 243 | 　　`tree-bitintlower2` | ON | 无 | 未移植 |
| 244 | 　　`tree-veclower22` | ON | ON | 未移植 |
| 245 | 　　`tree-switchlower2` | ON | ON | 未移植 |
| 246 | 　　`tree-ccp5` | ON | ON | 未移植（可复用已实现的 ccp） |
| 247 | 　　`tree-post_ipa_warn2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 248 | 　　`tree-objsz2` | ON | ON | 未移植 |
| 249 | 　　`tree-fab2` | ON | ON | 未移植 |
| 250 | 　　`tree-strlen2` | ON | ON | 未移植 |
| 251 | 　　`tree-copyprop5` | ON | ON | 未移植 |
| 252 | 　　`tree-dce8` | ON | ON | 未移植（可复用已实现的 dce） |
| 253 | 　　`tree-rebuild_frequencies2` | ON | 无 | 未移植 |
| 254 | 　　`tree-sancov2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 255 | 　　`tree-asan2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 256 | 　　`tree-tsan2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 257 | 　　`tree-crited2` | ON | ON | 未移植 |
| 258 | 　　`tree-uninit2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 259 | 　　`tree-uncprop2` | ON | ON | 未移植 |
| 260 | 　`tree-assumptions` | OFF | 无 | gate 关闭（-O1 不执行） |
| 261 | 　`*tminit` | OFF | OFF | gate 关闭（-O1 不执行） |
| 262 | 　　`tree-tmmark` | ON | ON | 未移植 |
| 263 | 　　`tree-tmmemopt` | OFF | OFF | gate 关闭（-O1 不执行） |
| 264 | 　　`tree-tmedge` | ON | ON | 未移植 |
| 265 | 　`tree-simduid2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 266 | 　`tree-vtable-verify` | OFF | OFF | gate 关闭（-O1 不执行） |
| 267 | 　`tree-lower_vaarg` | ON | ON | 未移植 |
| 268 | 　`tree-veclower` | ON | ON | 未移植 |
| 269 | 　`tree-cplxlower0` | ON | ON | 未移植 |
| 270 | 　`tree-bitintlower0` | ON | 无 | 未移植 |
| 271 | 　`tree-sancov_O0` | OFF | OFF | gate 关闭（-O1 不执行） |
| 272 | 　`tree-switchlower_O0` | OFF | OFF | gate 关闭（-O1 不执行） |
| 273 | 　`tree-asan0` | OFF | OFF | gate 关闭（-O1 不执行） |
| 274 | 　`tree-tsan0` | OFF | OFF | gate 关闭（-O1 不执行） |
| 275 | 　`tree-musttail` | OFF | 无 | gate 关闭（-O1 不执行） |
| 276 | 　`tree-sanopt` | OFF | OFF | gate 关闭（-O1 不执行） |
| 277 | 　`tree-ehcleanup2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 278 | 　`tree-resx` | OFF | OFF | gate 关闭（-O1 不执行） |
| 279 | 　`tree-nrv` | ON | ON | 未移植 |
| 280 | 　`tree-isel` | ON | ON | 未移植 |
| 281 | 　`tree-hardcbr` | OFF | OFF | gate 关闭（-O1 不执行） |
| 282 | 　`tree-hardcmp` | OFF | OFF | gate 关闭（-O1 不执行） |
| 283 | 　`tree-waccess3` | ON | ON | 未移植 |
| 284 | 　`tree-optimized` | ON | ON | 未移植 |
| 285 | 　`*warn_function_noreturn` | OFF | OFF | gate 关闭（-O1 不执行） |
| 286 | 　`rtl-expand` | ON | ON | 范围外（RTL 后端） |
| 287 | 　`*rest_of_compilation` | ON | ON | 范围外 / 不比较 |
| 288 | 　　`rtl-vregs` | ON | ON | 范围外（RTL 后端） |
| 289 | 　　`rtl-into_cfglayout` | ON | ON | 范围外（RTL 后端） |
| 290 | 　　`rtl-jump` | ON | ON | 范围外（RTL 后端） |
| 291 | 　　`rtl-subreg1` | ON | ON | 范围外（RTL 后端） |
| 292 | 　　`rtl-dfinit` | ON | ON | 范围外（RTL 后端） |
| 293 | 　　`rtl-cse1` | ON | ON | 范围外（RTL 后端） |
| 294 | 　　`rtl-fwprop1` | ON | ON | 范围外（RTL 后端） |
| 295 | 　　`rtl-cprop1` | OFF | OFF | gate 关闭（-O1 不执行） |
| 296 | 　　`rtl-hoist` | OFF | OFF | gate 关闭（-O1 不执行） |
| 297 | 　　`rtl-hardreg_pre` | ON | 无 | 范围外（RTL 后端） |
| 298 | 　　`rtl-cprop2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 299 | 　　`rtl-store_motion` | OFF | OFF | gate 关闭（-O1 不执行） |
| 300 | 　　`rtl-cse_local` | OFF | OFF | gate 关闭（-O1 不执行） |
| 301 | 　　`rtl-ce1` | ON | ON | 范围外（RTL 后端） |
| 302 | 　　`rtl-reginfo` | ON | ON | 范围外（RTL 后端） |
| 303 | 　　`rtl-loop2` | ON | ON | 范围外（RTL 后端） |
| 304 | 　　　`rtl-loop2_init` | ON | ON | 范围外（RTL 后端） |
| 305 | 　　　`rtl-loop2_invariant` | ON | ON | 范围外（RTL 后端） |
| 306 | 　　　`rtl-loop2_unroll` | OFF | OFF | gate 关闭（-O1 不执行） |
| 307 | 　　　`rtl-loop2_doloop` | OFF | OFF | gate 关闭（-O1 不执行） |
| 308 | 　　　`rtl-loop2_done` | ON | ON | 范围外（RTL 后端） |
| 309 | 　　`rtl-subreg2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 310 | 　　`rtl-web` | OFF | OFF | gate 关闭（-O1 不执行） |
| 311 | 　　`rtl-cprop3` | OFF | OFF | gate 关闭（-O1 不执行） |
| 312 | 　　`rtl-cse2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 313 | 　　`rtl-dse1` | ON | ON | 范围外（RTL 后端） |
| 314 | 　　`rtl-fwprop2` | ON | ON | 范围外（RTL 后端） |
| 315 | 　　`rtl-auto_inc_dec` | ON | ON | 范围外（RTL 后端） |
| 316 | 　　`rtl-init-regs` | ON | ON | 范围外（RTL 后端） |
| 317 | 　　`rtl-ud_dce` | OFF | OFF | gate 关闭（-O1 不执行） |
| 318 | 　　`rtl-ext_dce` | OFF | 无 | gate 关闭（-O1 不执行） |
| 319 | 　　`rtl-combine` | ON | ON | 范围外（RTL 后端） |
| 320 | 　　`rtl-late_combine1` | OFF | 无 | gate 关闭（-O1 不执行） |
| 321 | 　　`rtl-ce2` | ON | ON | 范围外（RTL 后端） |
| 322 | 　　`rtl-cc_fusion` | OFF | OFF | gate 关闭（-O1 不执行） |
| 323 | 　　`rtl-jump_after_combine` | OFF | OFF | gate 关闭（-O1 不执行） |
| 324 | 　　`rtl-bbpart` | OFF | OFF | gate 关闭（-O1 不执行） |
| 325 | 　　`rtl-outof_cfglayout` | ON | ON | 范围外（RTL 后端） |
| 326 | 　　`rtl-split1` | ON | ON | 范围外（RTL 后端） |
| 327 | 　　`rtl-subreg3` | ON | ON | 范围外（RTL 后端） |
| 328 | 　　`*stack_ptr_mod` | ON | ON | 范围外 / 不比较 |
| 329 | 　　`rtl-mode_sw` | ON | OFF | 范围外（RTL 后端） |
| 330 | 　　`rtl-asmcons` | ON | ON | 范围外（RTL 后端） |
| 331 | 　　`rtl-sms` | OFF | OFF | gate 关闭（-O1 不执行） |
| 332 | 　　`rtl-lr_shrinkage` | OFF | OFF | gate 关闭（-O1 不执行） |
| 333 | 　　`rtl-early_ra` | OFF | 无 | gate 关闭（-O1 不执行） |
| 334 | 　　`rtl-sched1` | OFF | OFF | gate 关闭（-O1 不执行） |
| 335 | 　　`rtl-avoid_store_forwarding` | OFF | 无 | gate 关闭（-O1 不执行） |
| 336 | 　　`rtl-ldp_fusion1` | ON | 无 | 范围外（RTL 后端） |
| 337 | 　　`rtl-early_remat` | OFF | OFF | gate 关闭（-O1 不执行） |
| 338 | 　　`rtl-ira` | ON | ON | 范围外（RTL 后端） |
| 339 | 　　`rtl-reload` | ON | ON | 范围外（RTL 后端） |
| 340 | 　　`*all-postreload` | OFF | OFF | gate 关闭（-O1 不执行） |
| 341 | 　　　`rtl-postreload` | OFF | OFF | gate 关闭（-O1 不执行） |
| 342 | 　　　`rtl-late_combine2` | OFF | 无 | gate 关闭（-O1 不执行） |
| 343 | 　　　`rtl-gcse2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 344 | 　　　`rtl-split2` | ON | ON | 范围外（RTL 后端） |
| 345 | 　　　`rtl-ree` | OFF | OFF | gate 关闭（-O1 不执行） |
| 346 | 　　　`rtl-cmpelim` | ON | ON | 范围外（RTL 后端） |
| 347 | 　　　`rtl-pro_and_epilogue` | ON | ON | 范围外（RTL 后端） |
| 348 | 　　　`rtl-dse2` | ON | ON | 范围外（RTL 后端） |
| 349 | 　　　`rtl-csa` | OFF | OFF | gate 关闭（-O1 不执行） |
| 350 | 　　　`rtl-jump2` | ON | ON | 范围外（RTL 后端） |
| 351 | 　　　`rtl-compgotos` | OFF | OFF | gate 关闭（-O1 不执行） |
| 352 | 　　　`rtl-sched_fusion` | OFF | OFF | gate 关闭（-O1 不执行） |
| 353 | 　　　`rtl-ldp_fusion2` | ON | 无 | 范围外（RTL 后端） |
| 354 | 　　　`rtl-peephole2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 355 | 　　　`rtl-ce3` | ON | ON | 范围外（RTL 后端） |
| 356 | 　　　`rtl-rnreg` | OFF | OFF | gate 关闭（-O1 不执行） |
| 357 | 　　　`rtl-fma_steering` | OFF | OFF | gate 关闭（-O1 不执行） |
| 358 | 　　　`rtl-fold_mem_offsets` | OFF | 无 | gate 关闭（-O1 不执行） |
| 359 | 　　　`rtl-cprop_hardreg` | ON | ON | 范围外（RTL 后端） |
| 360 | 　　　`rtl-rtl_dce` | ON | ON | 范围外（RTL 后端） |
| 361 | 　　　`rtl-speculation` | OFF | OFF | gate 关闭（-O1 不执行） |
| 362 | 　　　`rtl-bbro` | ON | ON | 范围外（RTL 后端） |
| 363 | 　　　`*leaf_regs` | ON | ON | 范围外 / 不比较 |
| 364 | 　　　`rtl-split3` | OFF | OFF | gate 关闭（-O1 不执行） |
| 365 | 　　　`rtl-sched2` | OFF | OFF | gate 关闭（-O1 不执行） |
| 366 | 　　　`*stack_regs` | OFF | OFF | gate 关闭（-O1 不执行） |
| 367 | 　　　　`rtl-split4` | OFF | OFF | gate 关闭（-O1 不执行） |
| 368 | 　　　　`rtl-stack` | ON | ON | 范围外（RTL 后端） |
| 369 | 　　`rtl-smstarts` | OFF | 无 | gate 关闭（-O1 不执行） |
| 370 | 　　`(null)` | OFF | 无 | gate 关闭（-O1 不执行） |
| 371 | 　　`rtl-late_pro_and_epilogue` | OFF | 无 | gate 关闭（-O1 不执行） |
| 372 | 　　`*all-late_compilation` | OFF | OFF | gate 关闭（-O1 不执行） |
| 373 | 　　　`rtl-zero_call_used_regs` | ON | ON | 范围外（RTL 后端） |
| 374 | 　　　`rtl-alignments` | ON | ON | 范围外（RTL 后端） |
| 375 | 　　　`rtl-vartrack` | OFF | OFF | gate 关闭（-O1 不执行） |
| 376 | 　　　`*free_cfg` | ON | ON | 范围外 / 不比较 |
| 377 | 　　　`rtl-mach` | OFF | OFF | gate 关闭（-O1 不执行） |
| 378 | 　　　`rtl-barriers` | ON | ON | 范围外（RTL 后端） |
| 379 | 　　　`rtl-dbr` | OFF | OFF | gate 关闭（-O1 不执行） |
| 380 | 　　　`rtl-split5` | ON | ON | 范围外（RTL 后端） |
| 381 | 　　　`rtl-eh_ranges` | OFF | OFF | gate 关闭（-O1 不执行） |
| 382 | 　　　`rtl-bti` | OFF | OFF | gate 关闭（-O1 不执行） |
| 383 | 　　　`rtl-shorten` | ON | ON | 范围外（RTL 后端） |
| 384 | 　　　`rtl-nothrow` | ON | ON | 范围外（RTL 后端） |
| 385 | 　　　`rtl-dwarf2` | ON | ON | 范围外（RTL 后端） |
| 386 | 　　　`rtl-final` | ON | ON | 范围外（RTL 后端） |
| 387 | 　　`rtl-dfinish` | ON | ON | 范围外（RTL 后端） |
| 388 | 　`*clean_state` | ON | ON | 范围外 / 不比较 |

gcc-12 独有的 pass（gcc-15 树中不存在）：`tree-phiprop`, `rtl-tag_collision_avoidance`。

## 4. 晚期 GIMPLE 序列的复用关系

`all_optimizations` 中可直接复用已实现 pass 的实例：ccp2/3/4（`opt/ccp.rkt`）、forwprop2/3/4、fre3 及后续、dse2/3/4/5、dce2 到 dce7、cddce2/3（≥O2 的 aggressive 模式未做）、phiprop2、mergephi2/3、sccopy2、local-pure-const2、modref2、profile 重建（rebuild_frequencies）。全新的：alias（PTA）、retslot、stdarg、cdce、cselim、copyprop、ifcombine、phiopt、ch、cplxlower、bitintlower、sra、dom、reassoc、pow、laddress、lim、sink、循环优化族（fix_loops 到 loopdone：sccp、ivcanon、cunroll、ivopts）、veclower、switchlower、sincos、slsr、strlen、wrestrict、fab、crited、uncprop、nrv、isel，以及 expand 前的 out-of-SSA（TER、变量合并）。IPA 阶段：inline（夹具中 static `twice` 在此被内联，之后 remove_symbols 删除它）、pure-const / modref 的 SCC 传播、cdtor、static-var、single-use、simdclone、targetclone、profile；cp / icf / sra / devirt 在 -O1 关闭。

## 5. 优化开关全表（`gcc-15 -Q --help=optimizers`）

| 开关 | -O0 | -O1 | -O2 | -O3 | 状态 |
|---|---|---|---|---|---|
| `--param=lto-partition-locality-cloning=` | maximal | maximal | maximal | maximal | 不在 -O1 范围（默认关闭） |
| `-faggressive-loop-optimizations` | ● | ● | ● | ● | 未移植 |
| `-falign-functions` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-falign-functions=` |  |  | 32:16 | 32:16 | 不在 -O1 范围（默认关闭） |
| `-falign-jumps` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-falign-jumps=` |  |  | 4 | 4 | 不在 -O1 范围（默认关闭） |
| `-falign-labels` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-falign-loops` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-falign-loops=` |  |  | 32:16 | 32:16 | 不在 -O1 范围（默认关闭） |
| `-fallocation-dce` | ● | ● | ● | ● | 未移植 |
| `-fallow-store-data-races` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fassociative-math` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fassume-sane-operators-new-delete` | available in C++, ObjC++ | available in C++, ObjC++ | available in C++, ObjC++ | available in C++, ObjC++ | 不在 -O1 范围（默认关闭） |
| `-fasynchronous-unwind-tables` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fauto-inc-dec` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-favoid-store-forwarding` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fbit-tests` | ○ | ● | ● | ● | 未移植（switchlower） |
| `-fbranch-count-reg` | ○ | ● | ● | ● | 未移植 |
| `-fbranch-probabilities` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fcaller-saves` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fcode-hoisting` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fcombine-stack-adjustments` | ○ | ● | ● | ● | 未移植 |
| `-fcompare-elim` | ○ | ● | ● | ● | 未移植 |
| `-fconserve-stack` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fcprop-registers` | ○ | ● | ● | ● | 未移植 |
| `-fcrossjumping` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fcse-follow-jumps` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fcx-fortran-rules` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fcx-limited-range` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fdce` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fdefer-pop` | ○ | ● | ● | ● | 未移植 |
| `-fdelayed-branch` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fdelete-dead-exceptions` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fdevirtualize` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fdevirtualize-speculatively` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fdse` | ○ | ● | ● | ● | 未移植 |
| `-fearly-inlining` | ● | ● | ● | ● | 恒等（不内联） |
| `-fexceptions` | available in D, Modula-2 | available in D, Modula-2 | available in D, Modula-2 | available in D, Modula-2 | 不在 -O1 范围（默认关闭） |
| `-fexcess-precision=[fast|standard|16]` | default | default | default | default | 不在 -O1 范围（默认关闭） |
| `-fexpensive-optimizations` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fext-dce` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-ffinite-loops` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-ffinite-math-only` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-ffloat-store` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-ffold-mem-offsets` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-ffold-simple-inlines` | available in C++, ObjC++ | available in C++, ObjC++ | available in C++, ObjC++ | available in C++, ObjC++ | 不在 -O1 范围（默认关闭） |
| `-fforward-propagate` | ○ | ● | ● | ● | 未移植 |
| `-ffp-contract=[off|on|fast]` | fast | fast | fast | fast | 不在 -O1 范围（默认关闭） |
| `-ffp-int-builtin-inexact` | ● | ● | ● | ● | 部分 |
| `-ffunction-cse` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fgcse` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fgcse-after-reload` | ○ | ○ | ○ | ● | 不在 -O1 范围（-O3） |
| `-fgcse-las` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fgcse-lm` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fgcse-sm` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fgraphite` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fgraphite-identity` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fguess-branch-probability` | ○ | ● | ● | ● | 完成（profile_estimate） |
| `-fhandle-exceptions` | -fexceptions | -fexceptions | -fexceptions | -fexceptions | 不在 -O1 范围（默认关闭） |
| `-fhardcfr-check-noreturn-calls=[always|no-xthrow|nothrow|never]` | default | default | default | default | 不在 -O1 范围（默认关闭） |
| `-fhardcfr-skip-leaf` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fharden-compares` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fharden-conditional-branches` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fharden-control-flow-redundancy` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fhoist-adjacent-loads` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fif-conversion` | ○ | ● | ● | ● | 未移植 |
| `-fif-conversion2` | ○ | ● | ● | ● | 未移植 |
| `-findirect-inlining` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-finline` | ○ | ● | ● | ● | 未移植 |
| `-finline-atomics` | ● | ● | ● | ● | 未移植 |
| `-finline-functions` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-finline-functions-called-once` | ○ | ● | ● | ● | 未移植 |
| `-finline-small-functions` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-finline-stringops` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-finline-stringops=` | default | default | default | default | 不在 -O1 范围（默认关闭） |
| `-fipa-bit-cp` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fipa-cp` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fipa-cp-clone` | ○ | ○ | ○ | ● | 不在 -O1 范围（-O3） |
| `-fipa-icf` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fipa-icf-functions` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fipa-icf-variables` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fipa-modref` | ○ | ● | ● | ● | 完成（local 实例）；IPA 传播未 |
| `-fipa-profile` | ○ | ● | ● | ● | 范围外 |
| `-fipa-pta` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fipa-pure-const` | ○ | ● | ● | ● | 完成（local 实例）；IPA 传播未 |
| `-fipa-ra` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fipa-reference` | ○ | ● | ● | ● | 范围外 |
| `-fipa-reference-addressable` | ○ | ● | ● | ● | 范围外 |
| `-fipa-reorder-for-locality` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fipa-sra` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fipa-stack-alignment` | ● | ● | ● | ● | 范围外 |
| `-fipa-strict-aliasing` | ● | ● | ● | ● | -O1 无 strict aliasing，alias set 恒 0 |
| `-fipa-vrp` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fira-algorithm=[CB|priority]` | CB | CB | CB | CB | 不在 -O1 范围（默认关闭） |
| `-fira-hoist-pressure` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fira-loop-pressure` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fira-region=[one|all|mixed]` | one | one | one | one | 不在 -O1 范围（默认关闭） |
| `-fira-share-save-slots` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fira-share-spill-slots` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fisolate-erroneous-paths-attribute` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fisolate-erroneous-paths-dereference` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fivopts` | ● | ● | ● | ● | 未移植 |
| `-fjump-tables` | ○ | ● | ● | ● | 未移植（switchlower） |
| `-fkeep-gc-roots-live` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-flate-combine-instructions` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-flifetime-dse` | ● | ● | ● | ● | 未移植 |
| `-flifetime-dse=<0,2>` | 2 | 2 | 2 | 2 | 不在 -O1 范围（默认关闭） |
| `-flimit-function-alignment` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-flive-patching` | -flive-patching=inline-clone | -flive-patching=inline-clone | -flive-patching=inline-clone | -flive-patching=inline-clone | 不在 -O1 范围（默认关闭） |
| `-flive-patching=[inline-only-static|inline-clone]` | default | default | default | default | 不在 -O1 范围（默认关闭） |
| `-flive-range-shrinkage` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-floop-interchange` | ○ | ○ | ○ | ● | 不在 -O1 范围（-O3） |
| `-floop-nest-optimize` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-floop-parallelize-all` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-floop-unroll-and-jam` | ○ | ○ | ○ | ● | 不在 -O1 范围（-O3） |
| `-flra-remat` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fmalloc-dce` | ● | ● | ● | ● | 未移植 |
| `-fmalloc-dce=<0,2>` | 0 | 0 | 0 | 0 | 不在 -O1 范围（默认关闭） |
| `-fmath-errno` | ● | ● | ● | ● | 部分（折叠子集） |
| `-fmin-function-alignment=` | 0 | 0 | 0 | 0 | 不在 -O1 范围（默认关闭） |
| `-fmodulo-sched` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fmodulo-sched-allow-regmoves` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fmove-loop-invariants` | ○ | ● | ● | ● | 未移植 |
| `-fmove-loop-stores` | ○ | ● | ● | ● | 未移植 |
| `-fno-inline-stringops` | ● | ● | ● | ● | 未移植 |
| `-fno-threadsafe-statics` | available in C++, ObjC++ | available in C++, ObjC++ | available in C++, ObjC++ | available in C++, ObjC++ | 不在 -O1 范围（默认关闭） |
| `-fnon-call-exceptions` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fnothrow-opt` | available in C++, ObjC++ | available in C++, ObjC++ | available in C++, ObjC++ | available in C++, ObjC++ | 不在 -O1 范围（默认关闭） |
| `-fomit-frame-pointer` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fopenmp-target-simd-clone=` | none | none | nohost | nohost | 不在 -O1 范围（默认关闭） |
| `-fopt-info` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-foptimize-crc` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-foptimize-sibling-calls` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-foptimize-strlen` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fpack-struct` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fpartial-inlining` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fpcc-struct-return` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fpeel-loops` | ○ | ○ | ○ | ● | 不在 -O1 范围（-O3） |
| `-fpeephole` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fpeephole2` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fplt` | ● | ● | ● | ● | 范围外 |
| `-fpredictive-commoning` | ○ | ○ | ○ | ● | 不在 -O1 范围（-O3） |
| `-fprintf-return-value` | ● | ● | ● | ● | 部分（折叠子集） |
| `-fprofile-partial-training` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fprofile-reorder-functions` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-freciprocal-math` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-free` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-freg-struct-return` | -fpcc-struct-return | -fpcc-struct-return | -fpcc-struct-return | -fpcc-struct-return | 不在 -O1 范围（默认关闭） |
| `-frename-registers` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-freorder-blocks` | ○ | ● | ● | ● | 未移植 |
| `-freorder-blocks-algorithm=[simple|stc]` | simple | simple | stc | stc | 不在 -O1 范围（默认关闭） |
| `-freorder-blocks-and-partition` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-freorder-functions` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-frerun-cse-after-loop` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-freschedule-modulo-scheduled-loops` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-frounding-math` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-frtti` | available in C++, D, ObjC++ | available in C++, D, ObjC++ | available in C++, D, ObjC++ | available in C++, D, ObjC++ | 不在 -O1 范围（默认关闭） |
| `-fsave-optimization-record` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fsched-critical-path-heuristic` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fsched-dep-count-heuristic` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fsched-group-heuristic` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fsched-interblock` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fsched-last-insn-heuristic` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fsched-pressure` | ○ | ● | ● | ● | 未移植 |
| `-fsched-rank-heuristic` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fsched-spec` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fsched-spec-insn-heuristic` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fsched-spec-load` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fsched-spec-load-dangerous` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fsched-stalled-insns` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fsched-stalled-insns-dep` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fsched2-use-superblocks` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fschedule-fusion` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fschedule-insns` | ○ | ○ | ○ | ● | 不在 -O1 范围（-O3） |
| `-fschedule-insns2` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fsection-anchors` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fsel-sched-pipelining` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fsel-sched-pipelining-outer-loops` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fsel-sched-reschedule-pipelined` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fselective-scheduling` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fselective-scheduling2` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fsemantic-interposition` | ● | ● | ● | ● | 简化模型 |
| `-fshort-enums` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fshort-wchar` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fshrink-wrap` | ○ | ● | ● | ● | 未移植 |
| `-fshrink-wrap-separate` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fsignaling-nans` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fsigned-zeros` | ● | ● | ● | ● | 部分 |
| `-fsimd-cost-model=[unlimited|dynamic|cheap|very-cheap]` | unlimited | unlimited | unlimited | unlimited | 不在 -O1 范围（默认关闭） |
| `-fsingle-precision-constant` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fsplit-ivs-in-unroller` | ● | ● | ● | ● | 未移植 |
| `-fsplit-loops` | ○ | ○ | ○ | ● | 不在 -O1 范围（-O3） |
| `-fsplit-paths` | ○ | ○ | ○ | ● | 不在 -O1 范围（-O3） |
| `-fsplit-wide-types` | ○ | ● | ● | ● | 未移植 |
| `-fsplit-wide-types-early` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fssa-backprop` | ● | ● | ● | ● | 未移植 |
| `-fssa-phiopt` | ○ | ● | ● | ● | 恒等 / 未移植 |
| `-fstack-clash-protection` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fstack-protector` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fstack-protector-all` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fstack-protector-explicit` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fstack-protector-strong` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fstack-reuse=[all|named_vars|none]` | all | all | all | all | 不在 -O1 范围（默认关闭） |
| `-fstdarg-opt` | ● | ● | ● | ● | 未移植 |
| `-fstore-merging` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fstrict-aliasing` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-fstrict-enums` | available in C++, ObjC++ | available in C++, ObjC++ | available in C++, ObjC++ | available in C++, ObjC++ | 不在 -O1 范围（默认关闭） |
| `-fstrict-volatile-bitfields` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fthread-jumps` | ○ | ● | ● | ● | 恒等（threader 未移植） |
| `-ftoplevel-reorder` | ○ | ● | ● | ● | 简化模型 |
| `-ftracer` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-ftrapping-math` | ● | ● | ● | ● | 部分 |
| `-ftrapv` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-ftree-bit-ccp` | ○ | ● | ● | ● | 完成（ccp1） |
| `-ftree-builtin-call-dce` | ○ | ● | ● | ● | 未移植（cdce） |
| `-ftree-ccp` | ○ | ● | ● | ● | 完成（ccp1） |
| `-ftree-ch` | ○ | ● | ● | ● | 未移植 |
| `-ftree-coalesce-vars` | ○ | ● | ● | ● | 未移植（out-of-SSA） |
| `-ftree-copy-prop` | ○ | ● | ● | ● | 未移植 |
| `-ftree-cselim` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-ftree-dce` | ○ | ● | ● | ● | 完成（cddce1；dce2+ 未） |
| `-ftree-dominator-opts` | ○ | ● | ● | ● | 未移植 |
| `-ftree-dse` | ○ | ● | ● | ● | 完成（dse1） |
| `-ftree-forwprop` | ● | ● | ● | ● | 完成（forwprop1） |
| `-ftree-fre` | ○ | ● | ● | ● | 完成（fre1） |
| `-ftree-loop-distribute-patterns` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-ftree-loop-distribution` | ○ | ○ | ○ | ● | 不在 -O1 范围（-O3） |
| `-ftree-loop-im` | ● | ● | ● | ● | 未移植 |
| `-ftree-loop-ivcanon` | ● | ● | ● | ● | 未移植 |
| `-ftree-loop-optimize` | ● | ● | ● | ● | 未移植 |
| `-ftree-loop-vectorize` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-ftree-lrs` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-ftree-parallelize-loops=<number>` | 1 | 1 | 1 | 1 | 不在 -O1 范围（默认关闭） |
| `-ftree-partial-pre` | ○ | ○ | ○ | ● | 不在 -O1 范围（-O3） |
| `-ftree-phiprop` | ● | ● | ● | ● | 恒等 |
| `-ftree-pre` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-ftree-pta` | ○ | ● | ● | ● | 恒等 / 未移植（PTA） |
| `-ftree-reassoc` | ● | ● | ● | ● | 未移植 |
| `-ftree-scev-cprop` | ● | ● | ● | ● | 未移植 |
| `-ftree-sink` | ○ | ● | ● | ● | 未移植 |
| `-ftree-slp-vectorize` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-ftree-slsr` | ○ | ● | ● | ● | 未移植 |
| `-ftree-sra` | ○ | ● | ● | ● | 恒等（esra）/ 未移植（sra） |
| `-ftree-switch-conversion` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-ftree-tail-merge` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-ftree-ter` | ○ | ● | ● | ● | 未移植（out-of-SSA） |
| `-ftree-vectorize` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-ftree-vrp` | ○ | ○ | ● | ● | 不在 -O1 范围（-O2） |
| `-ftrivial-auto-var-init=[uninitialized|pattern|zero]` | uninitialized | uninitialized | uninitialized | uninitialized | 不在 -O1 范围（默认关闭） |
| `-funconstrained-commons` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-funreachable-traps` | ● | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-funroll-all-loops` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-funroll-completely-grow-size` | ○ | ○ | ○ | ● | 不在 -O1 范围（-O3） |
| `-funroll-loops` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-funsafe-math-optimizations` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-funswitch-loops` | ○ | ○ | ○ | ● | 不在 -O1 范围（-O3） |
| `-funwind-tables` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-fvar-tracking` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fvar-tracking-assignments` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fvar-tracking-assignments-toggle` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fvar-tracking-uninit` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fvariable-expansion-in-unroller` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fvect-cost-model=[unlimited|dynamic|cheap|very-cheap]` | default | default | very-cheap | dynamic | 不在 -O1 范围（默认关闭） |
| `-fversion-loops-for-strides` | ○ | ○ | ○ | ● | 不在 -O1 范围（-O3） |
| `-fvpt` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fweb` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fwrapv` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-fwrapv-pointer` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-gstatement-frontiers` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-mearly-ldp-fusion` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-mearly-ra=` | none | none | all | all | 不在 -O1 范围（默认关闭） |
| `-mlate-ldp-fusion` | ● | ● | ● | ● | -O0 即启用（语义 / 后端开关） |
| `-mlow-precision-div` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-mlow-precision-recip-sqrt` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |
| `-mlow-precision-sqrt` | ○ | ○ | ○ | ○ | 不在 -O1 范围（默认关闭） |

● 启用，○ 关闭，其他为开关的非布尔取值。

## 6. 测试

- `o1-test.rkt` 276 项（4 夹具 × 2 版本 × 阶段），全套通过（aarch64 scribble 文档模块因缺 scribble collection 加载失败，与本工作无关）。
- 测试驱动按 GCC 的阶段顺序：先对全部函数 gimplify 到 SSA（`ipa-build_ssa_passes`），再逐函数跑 `ipa-opt_local_passes`；调用的 ECF 位按被调声明动态查询（`gimple_call_flags` 语义）。
