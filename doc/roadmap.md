# Roadmap: 重构与开发规划

Last Updated: 2026-11-05

适用范围: `src/` 新架构。根目录旧链路在 Phase 0 中删除。

结论标注: **已验证** = 本机实测 / **推断** = 有间接证据 / **未知**。

## 0. 当前状态（2026-11-05 实测）

| 项目 | 数值 | 来源 |
|---|---|---|
| `src/` 非测试代码 | 84 个 .rkt，16 805 行 | `wc -l` |
| `src/` 测试代码 | 14 个 `*-test.rkt` + 1 个集成脚本，5 637 行 | `wc -l` |
| 测试基线 | 260 个 test，0 失败（需 cutie-ftree 临时 link） | 已验证 |
| 根目录旧链路 | 9 674 行 .rkt + runtime.c/cc，require 24 个不存在的 `compiler/*.rkt` | 已验证，无法加载 |
| 环境 | Racket v9.3.13 [cs]（homebrew `racket@9`），定制版，内置 pvector/graph/intbits/intmap/omap/compare | 已验证 |
| 分支 | `trunk` == `origin/trunk` == `pri-github/trunk`；`trunk-java` 落后 4 个 commit | 已验证 |

阻塞项：`cutie-ftree` 在本机不是 collection，`src/` 全部测试 require 失败。用户决定：不再依赖 cutie-ftree，改用标准库。

## Phase 0: 去 cutie-ftree 依赖 + 清理旧链路（预计 0.5 天）

目标：`raco test src test` 在干净机器上零配置通过。

### 0.1 标识符缺口（已验证，脚本统计）

`src/` 从 cutie-ftree 使用 52 个导出标识符 + ordered-map 族：

| 族 | 标准库 | 缺口 |
|---|---|---|
| pvector（26 个，含 `pvector` 构造/match、`in-pvector`、`for/pvector`） | `racket/pvector` | 0 |
| graph（14 个，含 `graph` 匹配、`vertex-id-*`） | `racket/graph` | 0，API 同名同签名 |
| comparator（`integer-compare`/`symbol-compare`/`string-compare`） | `racket/compare` | 0，返回 `'<`/`'=`/`'>` 协议一致 |
| bitset（12 个） | `racket/intbits` | 12 个全部需改名；表示法相同（非负整数位图，empty = 0） |
| ordered-map（约 700 处调用，17 个 API） | 无通用等价物（`racket/omap` 的 `define-omap` 是按比较器单态化的宏） | 需要 shim |

### 0.2 任务

1. 新建 `src/kernel/data/bitset.rkt`：12 个别名。
   `bitset-empty`=`intbits-empty`、`bitset-add`→`intbits-set`、`bitset-remove`→`intbits-clear`、`bitset-union`→`intbits-union`、`bitset-subtract`→`intbits-subtract`、`bitset-member?`→`intbits-ref`、`bitset-count`→`intbits-count`、`bitset-equal?`→`=`、`bitset?`→`exact-nonnegative-integer?`、`bitset-empty?`→`intbits-empty?`、`in-bitset`→`in-intbits`、`(bitset …)`→`intbits`。
2. ordered-map 不保留。按键类型替换（164 处 `ordered-map-empty`，已验证分布）：
   - driver 层泛型节点（27 处 `node-compare`）→ 删除比较器参数，内部 visited/index 用 `equal?` 不可变 hash；返回的有序集合（支配树子节点、支配边界、循环 exits）一律按 `nodes` 输入顺序给出 pvector，不依赖 hash 迭代序。
   - symbol / string 键（info 字典、label 表，26 处）→ 不可变 `hash`。
   - 裸整数键（aarch64 内存、block-data、SCC 邻接）→ `racket/intmap`。
   - typed-id 键（BlockId/VarId/NodeId/WireId/InputId/OutputId，约 440 处调用）→ `src/kernel/data/idmap.rkt`：表示就是 `intmap[id-int -> (cons typed-id value)]`，键取 id 的整数载荷（一次 `unsafe-struct-ref`，无比较器闭包），迭代按 id 升序并返回 typed id，`write`/`read` 往返成立（IR 的 `Region`/`Cfg` 内嵌这些表，必须可序列化）。`#f` 作为键排在所有 id 之前（method-end try 窗口用）。
   - `ordered-map-delete` 2 值返回 → `idmap-remove`/`intmap-remove`/`hash-remove` 1 值，4 处改写。
   - 迁移脚手架 `src/kernel/data/ordered-map.rkt`（闭包版 WBT + `gen:dict`）只用于分步迁移期间保持测试绿，迁移完成即删除。
3. `src/kernel/data/data.rkt`：把 `(require cutie-ftree/cutie-ftree)` 换成 `racket/pvector racket/graph racket/compare` + 上面两个 shim，`provide` 列表显式枚举。删除 `kernel/ir/cfg/types.rkt:192-196` 的本地 `integer-compare`，去掉 17 处 `(except-in … integer-compare)`。
4. 批量替换 20 个文件里的 `cutie-ftree/<x>` require（清单：`grep -rl cutie-ftree src test`）。aarch64 子树 9 个文件直接 require 了 cutie-ftree，统一改为 require `kernel/data/data.rkt`。
5. 删除 `vendor/`、`.gitmodules`。
6. 删除根目录旧链路：`compiler.rkt`、`run-tests.rkt`、`run-test-2.rkt`、`utilities.rkt`、`interp*.rkt`、`type-check-*.rkt`、`casts.rkt`、`heap.rkt`、`graph.rkt`、`graph-printing.rkt`、`multigraph.rkt`、`priority_queue.rkt`、`runtime-config.rkt`、`runtime.{c,cc,h,o}`、`Makefile`、`SUBMISSIONS.md`、`tests/`、`src/ui/box.png`。保留 `LICENSE`、`.commit`。已验证 `src/` 无任何 require 指向这些文件。
7. 新 `Makefile`：`test` 目标 = `raco test src test`；`test/integration/aarch64/basic.rkt` 改成 `module+ test`（当前顶层 check 失败时退出码仍为 0）。
8. 文档：readme / doc/*.md 删除 cutie-ftree 故障说明；`doc/architecture.md`、`doc/testing.md`、`doc/troubleshooting.md` 末尾的绝对路径链接指向旧盘 `M03/D28`，改为相对路径。

`racket/graph` 的 `vertex-id` 不是 prefab（`write` 为 `#(struct:vertex-id 0)`，`graph` 结构 `read` 不回来，已验证），与决策 1 冲突，因此 Phase 0 内一并处理：kernel `BlockId` 改为 `(struct BlockId (id) #:prefab)`；`Cfg` 删除 `graph` 字段（kernel 侧从未读取）、新增 `block-cnt` 计数字段；后继一律由 terminator 推导（`cfg-make-successors`），新增 `cfg-block-count`/`cfg-edge-count`/`cfg-fresh-block-id`。`racket/graph` 只保留给 aarch64 的 `AsmCfg`（它真的读图的边），其 prefab 化放到 Phase 2.4。

验收：`make test` 全绿，260 个 test 数字不变；`grep -r cutie-ftree src test` 为空；`grep -r ordered-map src test` 为空。

### 0.3 进度（2026-11-05）

| 步骤 | 状态 |
|---|---|
| cutie-ftree require 全部替换为标准库；bitset → intbits 改名；`data.rkt` 重写；去掉 17 处 `except-in integer-compare` | 完成，2880 项测试通过（原 260 + 脚手架对拍 2620） |
| driver 层 5 个文件重写为 hash/intmap，`node-compare` 参数删除，调用方同步 | 完成，全绿 |
| 死模块删除：`driver/dataflow/framework.rkt`、`driver/worklist/`、`component/cfg/analysis/{liveness,ssa}.rkt`、`component/cfg/transform/`、`component/rvsdg/utils/graph-ops.rkt`，umbrella 同步 | 完成 |
| symbol/string 键 → hash；aarch64 整数键 → intmap | 完成，全绿 |
| prefab `BlockId`、`Cfg` 去 graph、typed-id 键 → idmap（`src/` 与 `test/` 中 ordered-map 调用归零） | 完成，全绿 |
| 删除脚手架 `ordered-map.rkt`、删除根目录旧链路（43 个 .rkt + tests/ + runtime.*）、删除 `vendor/` 与 `.gitmodules`、新 Makefile + `scripts/run-tests.sh`、readme/doc 更新 | 完成；终测 `make test` 274 项通过 |

顺带修正的行为差异（原代码的隐性缺陷，转换时自然消失）：`component/aarch64/analysis/liveness.rkt` 的 `block-predecessors` 原来返回 `ordered-map-query` 的 `(key . value)` 对而不是前驱 pvector；regalloc 用 `vid-val` 查 `LivenessResult-block-info` 依赖比较器把 int 与 vertex-id 视为同键。两者现在都按 BlockId 直接查。

## Phase 1: 修复已证实的缺陷（预计 1 天）

全部来自审计的动态验证，现有测试覆盖不到这些路径。

| # | 位置 | 缺陷 | 修法 |
|---|---|---|---|
| 1 | `driver/loop/analysis.rkt:222-228, 273-279` | `for/fold` 返回 2 值赋给单个 `define`，`build-loop-forest`/`get-innermost-loop` 一调用就 arity 错误 | 接两个值；补嵌套循环测试 |
| 2 | `component/cfg/analysis/loops.rkt:72, 79-80` | 对 pvector 用 `null?`/`length` | 改 `pvector-empty?`/`pvector-length`；空分支返回类型与主路径一致 |
| 3 | `driver/dominance/dominator.rkt:56,113` 要求 RPO，但 `component/cfg/analysis/dominance.rkt:52` 与 `ssa-construct.rkt:160-168` 传的是 vertex-id 分配序 | 已验证构造 CFG（0→2, 2→1\|3, 1→3）上 idom(3) 算成 0，正确为 2 | **已修（2026-11-06）**：旧 driver 删除，统一用 `driver/dominance/dom.rkt`（自己按后继序 DFS 求 RPO；CHK idom；支配树 DFS 区间，`dom-dominates?` O(1)；Cooper 支配边界；`dom-fix` 局部重算）。随机 CFG 上与集合定义逐项比对（`dom-test.rkt`）；Java ssa/rvsdg/pipeline 输出不变 |
| 4 | `driver/dominance/dominator.rkt:126` | `cfg-compute-post-idom` exit 为 #f 时返回 void | 显式报错或返回空映射 |
| 5 | `component/common/analysis-framework.rkt:131-134` | 缓存命中后结果被丢弃，每次重算 | 返回 `cached` |
| 6 | `driver/dataflow/liveness.rkt:176-200` | Step 5 的 SCC 结果从未被读取，且遍历方向与 `scc.rkt:85` 声明的逆拓扑序相反 | 删除 Step 5，或修正方向后接入并加测试 |
| 7 | `driver/dataflow/liveness.rkt:343` + `driver/graph/scc.rkt:143` | `ordered-map-delete*` 定义两次 | 移入 `kernel/data/ordered-map.rkt` |

## Phase 2: 死代码删除 + 分层收口（预计 1 天）

### 2.1 删除（0 个使用方，已验证反向依赖图）

- `component/cfg/analysis/liveness.rkt`（独立重写，非 driver 绑定）
- `component/cfg/analysis/ssa.rkt`（382 行，连 `analysis.rkt` 都未汇总）
- `component/cfg/transform/traversal.rkt`（8 个转发包装）
- `component/rvsdg/utils/graph-ops.rkt`、`component/rvsdg/analysis/analysis.rkt`、`component/rvsdg/transform/transform.rkt`（后两者是空 `(provide)` + TODO）
- `driver/dataflow/framework.rkt`、`driver/worklist/worklist.rkt`（API 无外部调用点）
- `pipeline/common/id-generator.rkt`（无使用方；各模块各自计数）
- 8 处未使用的 require（`dominator.rkt:14`、`loop/analysis.rkt:14`、`cfg/analysis/dominance.rkt:12,16`、`loops.rkt:12,16`、`cfg/analysis/liveness.rkt:15`、`ssa-construct.rkt:48`）

薄转发模块（12-14 行，共 12 个）保留与否按 2.3 决定，不单独处理。

### 2.2 pvector 辅助函数归位到 `kernel/data/data.rkt`

`jvm-to-cfg.rkt:330 pvector-take`（标准库已有，删）、`ssa-construct.rkt:203 pvector-member?`、`:289 pvector-take-left`、`:154 pvector-extend-unique`、`graph-test.rkt:64 pvector-index-of`（data.rkt 已有，删）、`parser.rkt:451-460` 遮蔽 racket/list 的 `last`/`drop-right`/`take`（删）。ordered-map 当集合用的辅助函数（`dominator.rkt:37-47`、`loop/analysis.rkt:40-45`）合并为一份。

### 2.3 分层规则补全（改 `doc/architecture.md`）

- 明确 component 内部子层：`component/{cfg,rvsdg,common}` 是 IR 组件，`component/{java,aarch64,lvar,x86var}` 是语言/目标组件，后者可以依赖前者，前者之间不互相依赖。当前 `cfg-to-rvsdg.rkt:165-167`、`normalize-try-exits.rkt:55` 的依赖即属此类，补规则后合法。
- 汇总入口 `kernel.rkt`/`component.rkt`/`driver.rkt`/`pipeline.rkt` 与注释不一致（`kernel/ir/ir.rkt` 只导出 common+cfg，`component.rkt` 缺 java/aarch64，`pipeline.rkt` 缺 aarch64-asm）：要么补齐并加一个"全部入口可加载"的 smoke test，要么删除汇总入口。推荐补齐 + smoke test。
- `pipeline/java/ir/types.rkt` 纯转发 `kernel/ir/jvm/types.rkt`，删除。

### 2.4 prefab 同名冲突（已验证运行时同型）

- `component/aarch64/ir/cfg.rkt:96-108` 的 `Term:*` 与 `kernel/ir/cfg/types.rkt:143-164` 同名：`Term:jump` 同字段数 → 同一类型；`Term:ret` 字段数不同 → 谓词互不认。
- `kernel/ir/ast/types.rkt:82 (Var id)` 与 `kernel/ir/x86/x86var.rkt:29 (Var name)` 是同一类型。
- `Imm`：`aarch64/ir/types.rkt:223` 与 `kernel/ir/x86/types.rkt:52`。

方案：aarch64 IR 数据定义（`ir/types.rkt`、`ir/cfg.rkt` 的 struct 部分、`ir/config.rkt`）迁入 `kernel/ir/aarch64/`，并给每套 IR 的 struct 加前缀（`Asm:Term:ret` 或改用非 prefab + 显式 `#:transparent`）。改前先决定 prefab 是否必要（序列化需求：**未知**，需确认）。

## Phase 3: 结构重构（预计 2-3 天）

### 3.1 拆分 `cfg-to-rvsdg.rkt`（2 757 行，64 个顶层定义）

障碍：`translate-segment`（358-551）与 gamma/theta/kappa 互递归，Racket 模块不能循环 require。

步骤：
1. 新建 `cfg-to-rvsdg/ctx.rkt`：`Theta-Ctx`、`Kappa-Group`、5 个 parameter（`current-arm-scope/shared-set/theta-ctxs/kappa-groups/exn-out`）、再加一个 `current-translate-segment` parameter 作为回调注入点。
2. 按行段拆：`gamma.rkt`（645-1341）、`theta.rkt`（1682-2080）、`kappa.rkt`（2081-2738）、`insn.rkt`（555-635）、`arm-utils.rkt`（1342-1681）、主文件只留入口 + `translate-segment` + sink 安装。
3. 同时合并 4 份前向可达计算（`arm-reach-set` 767、`arm-blocks-set` 1451、`theta-body-blocks` 1486、`kappa-forward-reach` 2412）为一个参数化函数。
4. 测试文件 `cfg-to-rvsdg-test.rkt`（2 023 行）按同样边界拆。
5. 过时注释修正：`cfg-to-rvsdg.rkt:193-195`（Kappa 已接入）、`java/pipeline.rkt:81-84`（try/catch 已支持）、`pipeline/pipeline.rkt:13,32`（java 不是 stub）。

验收：拆分前后 pipeline-test 对 `ClassTransform.dat` 的输出逐方法 `equal?`。

进度（2026-11-06）：步骤 3 已做——`structurize/reach.rkt` 的 `forward-reach`（`#:stop?` `#:leaf?` `#:switch?` `#:strict?`）替代 4 份遍历，4 个旧名保留为包装；步骤 5 已做（三处注释）；步骤 4（拆 2 023 行测试文件）放弃：按构造拆测试没有收益，golden 已覆盖回归。步骤 1–2 已做——`src/component/rvsdg/structurize/{ctx,reach,regions,insn,sinks,gamma,theta,kappa}.rkt`，主文件 459 行只剩入口与 `translate-segment`，递归经 `ctx.rkt` 的 `current-translate-segment` 注入；golden 逐方法相同。步骤 3（合并 4 份前向可达）、4（拆测试）、5（注释）待做。步骤 0 已做——算法整体移到 `src/component/rvsdg/structurize.rkt`，CFG 访问全部改走 `src/component/rvsdg/port.rkt`（决策 13 的端口模式），Java 侧 `component/java/transform/cfg-to-rvsdg.rkt` 只剩适配；验收基线固化为 `test/integration/java-rvsdg-golden.rktd`（5 个方法的 RVSDG datum），`src/pipeline/java/golden-test.rkt` 逐方法比对。步骤 1–5 待做。

**结构化器的已知缺口（2026-11-06，由 C 夹具暴露，`component/gimple/rvsdg-test.rkt` 以预期错误钉住）**：
- 短路梯形：`if (a > 0 && b < 10) … else if (a == 0 || !p) …`（calls.c `use`）——else 目标同时从 then 臂内部可达，`pick-phi-source` 在汇合块找不到来自该臂的 φ 来源。
- 循环体内 `break`（struct.c `sum`）——循环有两个出口在循环后汇合，`translate-segment` 报 unresolved phi。
javac 对同类源码生成相同形状，只是现有 Java 夹具没覆盖。根治方向是 Bahmann/Reissmann 的分支重构 + 循环重构（引入谓词变量把任意可归约 CFG 变成严格嵌套的 Gamma/Theta），而不是继续给 `translate-segment` 加特判；建议作为 Phase 3 的独立子项。

### 3.2 `jvm-to-cfg.rkt`（717 行）

`translate-insn`（369-564，29 分支 match）改为 opcode → handler 的分派表；按描述符解析（48-85）/栈模型（315-338）/指令（364-621）/terminator（622-717）拆 4 个文件。

### 3.3 pass 抽象二选一

现状：`pass.rkt` 只有 x86 pipeline 部分使用，`run-pass` 忽略 `requires`/`invalidates`；java 和 aarch64-asm 直接函数组合。
- 选项 A（**已决定**）：保留 `pass.rkt`，`run-pass` 校验 `requires`，java pipeline 的 4 步与 aarch64-asm 的 3 步接入；x86 的两条重复 pass 列表（111-131）用 `pipeline-append`。
- 选项 B：删除 `pass.rkt`，三条 pipeline 统一为显式函数组合。

### 3.4 分析注册表

`component/common/analysis-framework.rkt` 的全局可变注册表靠模块加载副作用注册（`ssa-construct.rkt:48` 就是为此 require 的）。Phase 1 修缓存后，评估是否保留；若保留，改为显式注册函数而非加载副作用。

## Phase 4: AArch64 子树修复（预计 1 天，#2-#5 挂起）

全部为已验证缺陷，当前 `test/integration/aarch64/basic.rkt` 不经过 parser/regalloc，发现不了。**决策：`emit.rkt` 直出汇编模式挂起，后端未来接入 rktasm，#2-#5 不修。**

| # | 位置 | 缺陷 |
|---|---|---|
| 1 | `frontend/parser.rkt:483-484` | `find-or-create-block-id` 返回 `Label:named` 而非 vertex-id，`cfg-add-block` 过滤掉非 vertex-id 后继 → 解析出的多块函数 0 条边，跨块 liveness/regalloc 不可靠 |
| 2 | `backend/emit.rkt:287-293` vs `319-324` | 块标签用 `AsmBlock-label`（`done:`），跳转目标输出 `.L<vertex-id>`（`b .L1`），对不上 |
| 3 | `backend/emit.rkt:82-89` | `emit-operand` 缺 `Reg:v/b/h/s/d/q/ffr` 分支，实测输出 `fmov #s(Reg:d 0), …` |
| 4 | `backend/emit.rkt:267` | `AsmInsn` 无分支，`ptrue` 等 SVE 指令输出为 `// unknown:` 注释；`analysis/liveness.rkt:80,140` 同样缺 `AsmInsn` def/use |
| 5 | `backend/emit.rkt:295-296, 352, 260, 348/358` | `Term:switch` 未实现；按 vertex-id 顺序输出块不看 entry；`whilelt` 宽度写死 `.s`；`.type/.size` 是 ELF 伪指令，darwin 上能否汇编 **未验证** |
| 6 | `frontend/parser.rkt:463-480` | `bl` 被判为 terminator 但 `parse-terminator` 无 `bl` 分支；476-478 的 `else-target` 固定 #f |
| 7 | `backend/regalloc.rkt:292, 347` | 整体 liveness 算后未用（实际用 310-312 的分类三份）；347 注释说转换 vreg-id 但无代码 |
| 8 | `analysis/liveness.rkt:147-154` | 用 `struct->vector` 反射访问 `regalloc.rkt:93-97` 的 `VRegIndex`，原因是循环依赖 | 把 `VRegIndex` 移到 `ir/` |
| 9 | `interp/base.rkt:466-468` | 未处理指令静默返回 void | 改为报错 |
| 10 | `pipeline/aarch64-asm/aarch64-asm.rkt:286-288` | 需 spill 时直接报错，无溢出代码生成 | 后续开发项 |

验收：新增 `test/integration/aarch64/multi-block-test.rkt`，走 parser → regalloc，断言解析出的多块函数边数正确、跨块活跃性正确；不检查汇编文本。

## Phase 5: 开发路线（Phase 0-2 完成后启动）

按 `pipeline/java/pipeline.rkt:147-176` 的 `supported` 列表与 `cfg-to-rvsdg.rkt` 的报错点：

1. **Java 链路未支持形态**（有明确报错点，可逐项变成测试）
   - ≥3 条 back-edge 的循环（`cfg-to-rvsdg.rkt:136-138, 2000-2003`）
   - 空 arm（1437）、body pre-dispatch 块含 phi（1713）、Theta header 非 `Term:cond` 结尾（1892）
   - switch arm 索引分派（99, 1004 推迟到后续 pass）
   - 未支持 opcode（`jvm-to-cfg.rkt:31`）：对 `ClassTransform.dat` 跑一遍统计 `'error` 方法数和报错分布，按频次排序
2. **RVSDG 之后**：`component/rvsdg/{analysis,transform}` 目前为空。候选：死节点消除、Gamma/Theta 不变量外提、RVSDG → CFG 回写（为 AArch64 后端打通 Java → AArch64）。
3. **x86 链路**：整条链（lvar 5 + x86var 7 + pipeline）0 个测试，仅能经孤儿入口到达。先补 `pipeline/x86/pipeline-test.rkt`（已验证 `compile-program` 能对小程序输出汇编），再决定保留或删除。
4. **AArch64 后端**：spill 代码生成（Phase 4 #10）、`Term:switch`、SVE 宽度参数化。
5. **工程**：CI（`doc/testing.md` 第 7 节的三组并行作业）；`frontend/java/java-tools/build.rkt:19-20` 声明下载 asm 9.7 而仓库放的是 9.9，统一。

## Phase 6: gccast 前端（GCC GENERIC → 现有框架）

### 6.1 插件（独立目录 `gccast-plugin/`，GPLv3，不在 `src/` 内）

**已验证（2026-11-05）**：最小插件（30 行 C++，`PLUGIN_PRE_GENERICIZE`）在 macOS 上用 Homebrew gcc-15 15.2.0 与 gcc@12 12.5.0 各自构建后都能 `-fplugin` 加载，C 函数触发回调，`DECL_SAVED_TREE` 为 `bind_expr`。构建需加 `-I/opt/homebrew/include`（gmp/mpfr/mpc 头）与 `-Wl,-undefined,dynamic_lookup`。15 构建的插件装进 12 被 `plugin_default_version_check` 拒绝：每个 GCC 版本一个产物。

**决策 11：插件源码由 Racket 生成器按 GCC 版本生成。** 版本漂移的事实依据（gcc-12 → gcc-15，已用脚本比对）：
- `tree.def` + `c-common.def` + `cp-tree.def`：342 → 350 个 tree code，删 10、增 18；
- 6 个 C 前端语句的操作数个数变了：`FOR_STMT` 5→8、`WHILE_STMT` 2→5、`SWITCH_STMT` 4→5、`DO_STMT` 2→3、`BREAK_STMT`/`CONTINUE_STMT` 0→1。
手写一份 C++ 想同时兼容只能堆 `#if GCCPLUGIN_VERSION`；生成器直接读目标 GCC 自带的 `.def`/`.h`，一次生成插件 C++ 表与 Racket 侧解码表，两端同步。

生成器输入（都在 `$(gcc -print-file-name=plugin)/include`）：
- `DEFTREECODE (CODE, "name", tcc_class, nargs)`：code 表、类别、操作数个数；
- `#define FOO_BAR(NODE) TREE_OPERAND (FOO_CHECK (NODE), n)` 形式的访问宏（`tree.h`、`c-family/c-common.h`、`cp/cp-tree.h`）：操作数位置 → 语义名，供 Racket 侧按名访问；
- `plugin-version.h`：basever，用于产物目录名与 Racket 侧 `.dat` 头部版本号。

生成物：
- `gccast-plugin/build/<basever>/tree-codes.inc`：C++ `switch`/表；
- `src/frontend/gccast/tables/<basever>.rktd`：Racket 侧 code → (class nargs (operand-names…))；
- 手写部分只用跨版本稳定的 API（`TREE_CODE`、`TREE_OPERAND`、`TREE_TYPE`、`DECL_NAME`、`DECL_UID`、`TYPE_UID`、`wi::to_wide`、`expand_location`）；需要分版本的 API 差异放在生成器的 shim 表里，不散落在 C++ 源码中。

目录：
```
gccast-plugin/
  LICENSE            GPLv3
  gen/               Racket 生成器（treedef.rkt 解析 .def/.h；emit.rkt 产出 .inc/.rktd；main.rkt CLI）
  src/gccast.cc      手写：plugin_init、回调、序列化 walker、节点表
  build.rkt          对列表中的每个 GCC：生成 → 用该版本 g++ 编译 → 冒烟加载
  tests/*.c|*.cc     夹具源码；生成的 .dat 提交到 test/integration/gccast/
```

序列化格式（Racket `read` 直读的 prefab s-expression，每个翻译单元一个文件）：
- `#s(TU gcc-version file nodes roots)`，`nodes` 为向量，下标即 `TreeId`；
- `#s(Node code type loc fields)`：`code` 为 tree code 符号；`type` 为 `#s(TreeId n)` 或 #f；`loc` 为 `(file line col)`；`fields` 按类别固定布局，操作数一律 `#s(TreeId n)`；
- 树是 DAG（类型、声明共享），节点表 + 引用保证共享与环（`TREE_TYPE` 回指）都能表示；
- 常量：`INTEGER_CST` 精确整数、`REAL_CST` 十六进制串、`STRING_CST` 字节串；
- 未覆盖的 code 输出 `#s(Node UNKNOWN-<code> …)`，不中断。
- 首批覆盖的 tree code 见下。

- 回调：`PLUGIN_PRE_GENERICIZE`（逐函数，C 与 C++ 前端都触发），`PLUGIN_FINISH_TYPE`/`PLUGIN_FINISH_DECL`（全局声明与类型），`PLUGIN_FINISH_UNIT`（写文件）。必须导出 `int plugin_is_GPL_compatible;`。
- 首批覆盖的 tree code：声明类（FUNCTION/VAR/PARM/FIELD/TYPE/LABEL/RESULT/CONST_DECL）、类型类（INTEGER/REAL/POINTER/ARRAY/RECORD/UNION/ENUMERAL/FUNCTION/VOID/BOOLEAN_TYPE）、语句类（BIND_EXPR、STATEMENT_LIST、DECL_EXPR、COND_EXPR、SWITCH_EXPR、CASE_LABEL_EXPR、LABEL_EXPR、GOTO_EXPR、RETURN_EXPR、LOOP_EXPR、EXIT_EXPR、FOR/WHILE/DO/BREAK/CONTINUE/SWITCH_STMT 等 C 前端语句）、表达式类（MODIFY_EXPR、CALL_EXPR、全部算术/比较/逻辑、CONVERT/NOP_EXPR、ADDR_EXPR、INDIRECT_REF/MEM_REF、COMPONENT_REF、ARRAY_REF、COMPOUND_EXPR、TRUTH_*、TARGET_EXPR、CONSTRUCTOR）。`tcc_exceptional` 的 13 个基础 code（IDENTIFIER_NODE、TREE_LIST、TREE_VEC、BLOCK、CONSTRUCTOR、STATEMENT_LIST、SSA_NAME 等）手写。
- 测试：`gccast-plugin/tests/*.c` + 生成脚本；生成的 `.dat` 提交到 `test/integration/gccast/`，Racket 侧测试不依赖本机 gcc。

### 6.2 Racket 侧（遵守分层）

| 层 | 模块 | 内容 |
|---|---|---|
| kernel | `kernel/ir/gccast/types.rkt` | `TU`/`Node`/`TreeId` prefab；tree code 符号表；访问器 |
| frontend | `frontend/gccast/reader.rkt` | `file->value` → `TU`；校验节点表引用闭合 |
| component | `component/gccast/transform/lower-control.rkt` | GENERIC 结构化控制流 → kernel `Cfg`（`CfgBlock`/`VfInsn`/`Term:*`）；每个 `VAR_DECL`/`PARM_DECL` 分配 `VarId`，类型表写入 `Cfg-info`（决策 9） |
| component | `component/gccast/transform/lower-expr.rkt` | 表达式树 → `VfInsn` 序列（op 命名空间 `'(gccast . MODIFY_EXPR)` 形式，与 JVM op 隔离） |
| component | 复用 `ssa-construct`（推断可复用：异常表为空时其闭包退化为普通闭包；需确认它不硬依赖 `'java/block-order`，若依赖则把泛型部分移到 `component/cfg/analysis/`） | |
| component | 复用 `cfg-to-rvsdg` | 要求：op 透传；`Term:*` 形态在已支持列表内 |
| pipeline | `pipeline/gccast/pipeline.rkt` | 走 `pass.rkt`：`read → lower-control → ssa → memory-state → cfg->rvsdg`，`run-pass` 校验 `requires` |

### 6.3 GENERIC → GIMPLE：对齐 GCC trunk 的降级模块（决策 12）

目标：读入的 PRE_GENERICIZE GENERIC 先按 GCC 自己的算法降到 GIMPLE，再进现有的 `Cfg → SSA → RVSDG`。算法参照 `/Users/cutiedeng/Y2026/M10/D28/gcc.git`（trunk 17.0.0，`352a0b5fc55`）；对齐基准是本机 gcc-15 对同一源码的 `-fdump-tree-{original,gimple,lower,cfg}`（`build.rkt` 已随夹具一起生成到 `test/integration/gccast/<name>.<ver>.{original,gimple,lower,cfg}`）。trunk 与 15.2 在 C 子集上的差异逐条记录。

GCC 侧四段链路及对应的 Racket 模块（放 `src/component/gccast/`）：

| GCC | 参照文件 | Racket 模块 | 基准 dump |
|---|---|---|---|
| `c_genericize`：FOR/WHILE/DO/SWITCH/BREAK/CONTINUE_STMT → LOOP/COND/GOTO/LABEL/SWITCH_EXPR | `c-family/c-gimplify.cc`（`genericize_c_loop` 等，1064 行） | `transform/genericize.rkt` | `.original` |
| `gimplify_function_tree`：GENERIC → 高层 GIMPLE（`gbind`/`gcond`/`gassign` 三地址/`gcall`/`gswitch`/`greturn`） | `gimplify.cc` 非 OMP 部分（约 1 万行；核心 `gimplify_expr`、`gimplify_modify_expr(_rhs)`、`gimplify_cond_expr`、`shortcut_cond_expr`、`gimplify_compound_lval`、`gimplify_self_mod_expr`、`gimplify_call_expr`、`gimplify_bind_expr`、`gimplify_return_expr`、`gimplify_switch_expr`、`gimplify_loop_expr`、`internal_get_tmp_var`）+ `gimple-expr.cc` 谓词（`is_gimple_val/reg/lvalue/...`、`useless_type_conversion_p`） | `generic/tree.rkt`（可变 GENERIC 树 + GCC 宏同名访问器）、`transform/gimplify.rkt` | `.gimple` |
| `lower_function_body`：拍平 `gbind`、合并 return | `gimple-low.cc` | `transform/gimple-low.rkt` | `.lower` |
| `build_gimple_cfg`：按 label/goto/cond/switch/return 切块建边 | `tree-cfg.cc` | `transform/gimple-to-cfg.rkt`（产出 kernel `Cfg`） | `.cfg` |

实现约定：
- 对齐测试：我方 `gimple-print.rkt` 按 `gimple-pretty-print.cc` 的文本格式输出，与基准 dump 做规范化 diff（`D.NNNN`、`_N`、`name.K_N` 按首次出现顺序重编号；GCC 的 `tmp_var_id_num` 是翻译单元全局计数，SSA 版本号每函数从 1 起）。
- 已确认的 -O0 行为（从 15.2 dump 读出，与 trunk 源码一致）：`gimplify_body` 以 `into_ssa=true` 进入，`internal_get_tmp_var` 对寄存器类型直接造 SSA 名（`_1`），有名字的来源得 `n.0_16`；`create_tmp_var` 造的 `D.NNNN`（`iftmp`、`retval`、`return_temp`）在 bind 头部声明；`maybe_fold_stmt` 在 -O0 也运行，至少 `get_symbol_constant_value` 可见（`static const double half` 被替换为 `5.0e-1`）；`i < 10` 已由前端折叠为 `i <= 9`，`?:` 已折叠为 `MAX_EXPR`，这些发生在插件取树之前。
- 暂不实现：OpenMP/OpenACC、EH（`TRY_*`、`CLEANUP_POINT_EXPR` 只透传）、VLA（`gimplify_type_sizes`/`WITH_SIZE_EXPR`）、`asm`、`va_arg`、`TARGET_EXPR`、`gimplify_init_constructor` 的完整形态（首批只做标量初始化）、`fold_stmt` 的 match.pd 规则（只做常量折叠与只读符号替换，其余按基准 diff 逐条补）。
- 目标相关量：`aggregate_value_p`（结构体是否经内存返回）按 AArch64 AAPCS64 近似为"聚合且大于 16 字节"，以参数形式暴露。
- 许可：这些模块是 GCC（GPLv3）算法的逐函数改写，属派生作品；放在 `src/component/gccast/` 并在文件头标注来源与 GPLv3，仓库其余部分维持 MIT。是否接受这一许可混合由用户决定。

里程碑 3 改为：`basic.c`、`struct.c`、`calls.c` 三个夹具的 `.gimple` 规范化 diff 为空（12.5.0 与 15.2.0 两套基准）；随后 `.lower`、`.cfg`；最后接 `ssa-construct`/`cfg-to-rvsdg`。

### 6.4 新增共用工作：内存状态线程化

GIMPLE 侧完成（2026-11-06）：内存状态就是 GCC 的虚拟操作数 `.MEM`，作为泛型 SSA 端口的最后一个变量（`component/gimple/ssa.rkt`）：读内存（内存引用、非寄存器对象、每条 `return`、pure 调用）的语句有 VUSE，写内存（内存 lhs、非 const/pure 调用、clobber）的语句有 VDEF（同时 VUSE 旧状态）；名字存在语句 flags 的 `'vuse`/`'vdef`，分配顺序为 uses、VDEF、实定义，与 GCC 的版本号一致。对齐基准 `-fdump-tree-ssa-vops`（`.ssavops` 夹具）：`# .MEM_N = VDEF <.MEM_M>`、`# VUSE <.MEM_M>`、`.MEM` 的 PHI 在三个 C 夹具 × gcc 12/15 上逐行相同（`ssa-test.rkt` 12 项）。RVSDG 适配器（`component/gimple/rvsdg.rkt`）把 VUSE 作 load/store/call 的最后一个输入、VDEF 作额外输出、`return` 带上最终状态、`.MEM_N(D)` 作参数——状态边由此自然进入 Lambda/Gamma/Theta。-O0 只有一条 `.MEM` 链，没有别名分区，比 GCC -O1+ 保守。

Java 侧待做：`jvm-to-cfg` 的 GETFIELD/PUTFIELD/INVOKE* 以同样方式接入（Java 端口的 `variables` 多一个状态变量）。原设想的独立 pass `component/cfg/transform/thread-memory-state.rkt` 不再需要。

### 6.5 里程碑

| # | 内容 | 状态（2026-11-05） |
|---|---|---|
| 1 | 空插件在 macOS gcc-15 上加载（验证 `-fplugin` 可用） | 完成：gcc-15 15.2.0 与 gcc@12 12.5.0 都能加载；`PRE_GENERICIZE` 对 C/C++ 函数触发 |
| 2 | `.dat` 能被 `reader.rkt` 读成 `TU`，节点表引用闭合 | 完成：`gccast-plugin/` 骨架 + `src/kernel/ir/gccast/types.rkt` + `src/frontend/gccast/reader.rkt`；夹具 `test/integration/gccast/{basic.c,struct.c,method.cc}.{12.5.0,15.2.0}.dat`；`reader-test.rkt` 10 项（含按访问宏名取 `FOR_COND`/`FOR_BODY`，12 与 15 操作数个数不同但同一测试通过） |
| 3a | GENERIC → GIMPLE 与 GCC 对齐（6.3） | 完成（2026-11-05）：`src/component/gccast/{generic/tree.rkt, generic/import.rkt, generic/gimple.rkt, transform/genericize.rkt, transform/gimplify.rkt, transform/gimple-print.rkt}`；`gimplify-test.rkt` 对 basic.c / calls.c / struct.c / locals.c × gcc 12.5.0 / 15.2.0 的 `.gimple` 规范化 diff 为空（8 项） |
| 3b-1 | `gimple-low.rkt`（pass_lower_cf）：拍平 bind、按 retval 合并 return（-O0 下带位置的 return label 置为非 artificial）、删不可达 return、补尾部 `return;`、透传 `gtry` | 完成（2026-11-06）：`lower-test.rkt` 对 4 夹具 × 2 版本的 `.lower` 规范化 diff 为空（8 项） |
| 3b-2 | `gimple-to-cfg.rkt`：make_blocks/cleanup_dead_labels×2/group_case_labels/make_edges/assign_discriminators，再复现 -O0 的 `cleanup_tree_cfg`（常量条件折叠、不可达块、forwarder、合并、2 标签 switch → cond）对齐 `.cfg` | 完成（2026-11-06）：`cfg-test.rkt` 对 basic.c / calls.c / struct.c × 2 版本的 `.cfg` 规范化 diff 为空（6 项，含 `;; N succs` 边序与 `Removing`/`Merging` 消息）；locals.c 需先有 pass_lower_eh；规格见 `doc/gimple-lower-cfg-spec.md` |
| 3c | （已被决策 13 取代）不再转 kernel `Cfg`；改为持久化 GIMPLE 表示 + 泛化 SSA / cfg-to-rvsdg，阶段 S1–S6 见 `doc/gimple-functional-ir.md` 第 7 节 | S1–S5 完成（2026-11-06）：`src/kernel/ir/gimple/` + `src/component/gimple/`，`.gimple`/`.lower`/`.cfg`/`.ssa` 对齐 8/8/6/6；泛型 SSA 在 `driver/ssa/construct.rkt`，Java 与 GIMPLE 各一个端口；S6（cfg-to-rvsdg 泛化）待做 |
| 4 | 指针、结构体、数组、调用 + 内存状态边 | 待做 |

实现备注（3a，gimplify 对齐过程中确认的 GCC 行为）：
- 基准 dump 里 `_1`、`n.0_16` 这类名字说明 `gimplify_body` 以 `into_ssa=true` 运行：寄存器类型的形式临时量直接是 SSA 名，有名来源时标识符为 `create_tmp_var_name(name)`（`tmp_var_id_num` 翻译单元全局）；`create_tmp_var` 产生的 `D.NNNN` 只有 `iftmp`/`retval`/return_temp。
- `maybe_fold_stmt` 在 -O0 生效且可见：可交换运算与条件按 `tree_swap_operands_p` 规范化操作数顺序（`acc + _19`，常量靠右、SSA 名靠右、decl 靠右）；只读静态变量的常量初值被代入（`half.2_22 = 5.0e-1`）。
- -O0 的标签劫持规则依赖源码位置：`if (c) break;` 因 COND_EXPR 与 BREAK 生成的 GOTO 位置不同而不劫持，生成 `<L1>: goto <break>; <L2>:`；循环出口 `COND_EXPR` 的两个 GOTO 没有位置，所以劫持。genericize 必须像 `build1_loc` 一样给 break/continue 的 GOTO 带位置。
- CLOBBER 清理已实现（`locals.c` 夹具：数组、结构体、取址标量、嵌套作用域；gcc 12 打印 `CLOBBER(eol)`，14+ 打印 `CLOBBER(eos)`）。未覆盖：`gimplify_init_constructor` 完整形态（聚合初始化器在 -O0 会走静态常量 `a = *.LC0` 路径）、builtin 折叠、VLA、EH。
- 插件产物固定命名 `gccast.so`（macOS 也是）：GCC 以去掉 `.so` 后的文件名作为插件名解析 `-fplugin-arg-<name>-*`，`.dylib` 会匹配不上。
- 不输出 decl 的 `TREE_CHAIN`：命名空间级声明的 chain 是全部兄弟声明（含 builtin），`method.cc` 曾因此膨胀到 16 022 节点；改为只在拥有链的构造上物化列表（`BIND_EXPR` 的 `vars`、`TYPE_FIELDS`、`DECL_ARGUMENTS`、`BLOCK` 的 `vars`）后为 87 节点。
- 每个函数独立节点表：`PRE_GENERICIZE` 回调内同步序列化，指针→下标映射只在该函数内有效，避免 GC 复用地址造成跨函数别名；跨函数的类型/声明同一性靠 `DECL_UID`/`TYPE_UID` 恢复。
- 常量：`INTEGER_CST` 能放进 host word 时直接十进制，否则 `#s(Wide precision limbs)`（`wide_int` 的 `get_len`/`elt` 在 12 与 15 都稳定）；`REAL_CST` 用 `real_to_hexadecimal`；`STRING_CST` 字节串含结尾 NUL。
- `PLUGIN_FINISH_DECL`/`PLUGIN_FINISH_TYPE`（全局声明与类型）尚未接入；函数可达的类型/声明已随函数序列化。

实现备注（3b，lower/cfg 对齐过程中确认的 GCC 行为）：
- 位置模型：GIMPLE 语句的 location 是 (spot, BLOCK, discriminator) 三元组（`gloc`），`#f` 为 UNKNOWN。lower 的 `set_block` 让无 spot 的语句（label、人工 goto）得到"只有 BLOCK"的非零位置；`assign_discriminators`（trunk 版，385d9937f0e）给这种位置分配非零 discriminator 时 `location_with_discriminator` 返回 UNKNOWN。cleanup 的 -O0 判据（forwarder 的前驱边/label 位置检查、`gimple_can_merge_blocks_p` 的 goto_locus 检查）比较的都是整个三元组。
- gimplify 的位置来源：`input_location` 在每次 `gimplify_expr` 进入时取 `EXPR_LOCATION`、退出时恢复；语句级 `annotate_all_with_location_after` 只给没有位置、未标 do-not-emit 且不是 label 的语句赋值；cond/label/case/switch/assign/call 另有显式 `gimple_set_location`；`gimplify_cond_expr` 的 `label_cont` goto 标 do-not-emit；循环与 break/continue 的人工 label 带循环起始位置；return label 带 `function_end_locus`。
- -O0 下几乎所有 goto 边都带 goto_locus，因此 forwarder 极少被删、块极少合并：只含 `goto <bb N>;` 的空块（struct.c 的 bb4）与只含 `<Ln>:` 的块都保留。`;; N succs { … }` 的顺序由 succ 向量"创建时 push、删除时 unordered_remove（末尾填洞）"决定，测试把这些行保留下来比对。
- 按 trunk 实现、与 gcc-15 不同的点（三个夹具未触发差异）：`assign_discriminators` 重写（GCC16）、forwarder 判定新增"单前驱且前驱单后继则留给合并"（40c037adb36）、`group_case_labels` 撤销对 `__builtin_unreachable` 目标的删除（f108e23d2bf）。
- 未移植：abnormal/EH 边、computed goto、setjmp、`convert_single_case_switch` 的区间分支（`generate_range_test`）、`gimple_simplify` 的非常量折叠、循环树输出（`;; N loops found` 等行测试中忽略）。

## Phase 7: GIMPLE 层对齐 GCC -O1 优化 pass（决策 14，2026-11-06 起）

目标：把 GCC 的 -O1 tree 优化 pass 按 dump 顺序逐个移植到 `src/component/gimple/opt/`，每个 pass 以 `-O1 -fdump-tree-all` 的对应 dump 为基准逐函数对齐。方法论延续 6.3：基准由 `gccast-plugin/build.rkt` 生成（`<name>.<ver>.O1.<pass>`，另有 `-O1` 下的插件输出 `<name>.<ver>.O1.dat`，因为 C 前端在优化时折叠得不同：短路 `&&`/`||` 变 `TRUTH_AND/OR_EXPR`，const 静态变量读被初值替换）。

### 7.0 -O1 输入对齐（完成，2026-11-06）

`GS-optimize` 字段 + GCC 自己按 `optimize` 分叉的 6 处：`lookup_tmp_var` 复用形式临时量、`gimplify_cond_expr` 的 goto 劫持不查位置、lower 不把 return label 置为非 artificial、cleanup_tree_cfg 的 forwarder/merge 不查 goto_locus、`remove_unused_locals`（pass_build_ssa 的 TODO，只在优化时删未引用局部变量）。`o1-test.rkt`：3 夹具 × 2 版本的 `-O1` gimple/lower/cfg/ssa 逐函数对齐 24/24（函数按名比对：-O1 下 dump 的函数顺序是调用图序）。未做：`execute_update_addresses_taken`（现有夹具无取址局部变量）、`pass_fixup_cfg`（在现有夹具上无效果）。

### 7.1 pass 序列（gcc-15 -O1 实测 dump 顺序，全部对象）

early（`pass_build_ssa_passes` → `pass_all_early_optimizations`）：ccp1 → forwprop1 → ethread → esra → ealias → phiprop1 → fre1 → mergephi1 → dse1 → cddce1 → phiopt1 → iftoswitch → sccopy1 → profile_estimate → local-pure-const1 → modref1 → release_ssa；之后 IPA，再 `all_optimizations`：ccp2 … optimized。

逐 pass 移植顺序 = dump 顺序。已知的结构性障碍：forwprop/fre/ccp 的语句折叠依赖 `match.pd` 生成的 `gimple-match.cc`（十万行量级），无法逐条移植；策略是按夹具实际触发的模式增量实现折叠规则，并把 dump 不一致的地方逐条记录为"缺失规则"而不是绕过。

| pass | 状态 |
|---|---|
| ccp1 | 完成（2026-11-06）：`opt/propagate.rkt`（ssa_propagation_engine）、`opt/ccp.rkt`（bit-level 格、evaluate_stmt、substitute_and_fold、simple_dce_from_worklist、TODO_cleanup_cfg）、`opt/fold.rkt`（常量折叠子集）、`opt/ssa-ops.rkt`；4 夹具 × 2 版本逐函数对齐 |
| forwprop1 | 完成（2026-11-06）：`opt/mfn.rkt`（函数的可变工作副本：语句单元链、SSA def/立即使用表）、`opt/simplify.rkt`（fold_stmt_1 + match.pd 子集）、`opt/forwprop.rkt`；4 夹具 × 2 版本逐函数对齐 |
| ethread | 未移植，夹具无触发（恒等 stage，dump 须与前一 pass 相同，测试检查）。需要 backward threader + path ranger + threadupdate |
| esra | 未移植，夹具无触发（恒等 stage）。需要寄存器化聚合的夹具 |
| ealias | 未移植 points-to 分析；gcc ≥ 13 的 dump 只有求解器进度行（`opt/passes.rkt` 原样输出）；gcc-12 还 dump 约束与 points-to 集，测试排除 ealias@12.5.0 |
| phiprop1 | 未移植，夹具无触发（恒等 stage；gcc-12 此处无该 pass） |
| fre1 | 完成（2026-11-06）：`opt/vn.rkt`（RPO VN 非迭代驱动 + 内联消除）、`opt/alias.rkt`（别名预言机子集：get_ref_base_and_extent、refs_may_alias_p、stmt_may_clobber_ref_p、walk_non_aliased_vuses）；4 夹具 × 2 版本逐函数对齐 |
| mergephi1 | 未移植，夹具无触发（恒等 stage） |
| dse1 | 完成（2026-11-06）：`opt/dse.rkt`（逆 RPO、`dse_classify_store` 的 vdef 使用链游走、死 SSA 定义/死 PHI 删除）；alias.rkt 补 `stmt_kills_ref_p`、`ref_may_alias_global_p`、`walk_aliased_vdefs`。未做字节级部分死存储裁剪、memset/memcpy builtin、冗余零存储 |
| cddce1 | 完成（2026-11-06）：`opt/dce.rkt`。-O1 下 `pass_cd_dce` 的 aggressive = `optimize >= 2` 为假，即普通 DCE（条件/switch 恒必要，无控制依赖）；gcc ≥ 15 才带 `TODO_remove_unused_locals`（pass 表第三列，测试据此重算局部变量表）。未做 aggressive 模式、malloc/free 配对、atomics |
| phiopt1 / iftoswitch / sccopy1 | 未移植，夹具无触发（恒等 stage；sccopy 自 gcc-14 起存在） |
| profile_estimate | 完成（2026-11-06）：`opt/profile.rkt`（tree_estimate_probability：GIMPLE_PREDICT / noreturn / 递归调用的后支配路径预测、返回值 PHI 预测、循环预测（迭代次数闭式与 loop_niter_by_eval 暴力求值、loop exit、iv 比较、loop guard、extra exit）、call 与 opcode 启发式、combine_predictions_for_bb 的去重 / first match / DS theory、estimate_bb_frequencies 的 sreal 马尔可夫传播与计数缩放）、`opt/sreal.rkt`（GCC sreal 与 profile_probability 逐位复刻）、`opt/loops.rkt`（flow_loops_find、LOOPS_NORMAL 的 preheader / 单后继 latch 规范化）、`opt/niter.rkt`（affine simple_iv、闭式迭代次数、finite_loop_p 子集）；GCfg 增加 `profile` 字段（GProf：块计数 + 边概率），cfg.rkt 维护概率随边变动，print.rkt 输出 `[local count: N]` / `[P%]`；4 夹具 × 2 版本逐函数对齐。版本分叉：gcc ≤ 13 计数 `to_int(x + 1/2)`，≥ 14 `to_nearest_int`；DS theory 组合 gcc ≤ 14 double，≥ 15 int64。未做：伪出口边、hot/cold 属性、`__builtin_expect`、不可约循环、IPA 计数 |
| local-pure-const1 | 完成（2026-11-06）：`opt/pure-const.rkt`（analyze_function 的逐语句扫描 dump、check_decl / check_op / check_call、finite_function_p、声明更新）。被调函数的 const/pure/looping/noreturn 作为 GS 声明覆盖位保存，`gimplify-call-expr` 合入 call 的 ECF 位（函数按 GCC 的 callee-first 顺序逐个走完整 pass 序列，所以后处理的调用者能看到）；dump 名 `name/N` 的 N 由 gccast 插件在 IPA 开始时记录（gcc ≥ 15 为 cgraph uid，之前为 symbol order）。未做：异常 / nothrow、malloc 发现、builtin 表、points-to（指针只在源自局部变量地址时视为局部） |
| modref1 | 完成（2026-11-06）：`opt/modref.rkt`（analyze_parms 的 EAF 标志 DFS / 数据流及其逐步 dump、modref 树（base/ref/access，`modref_access_node` 的包含 / 合并 / 强制合并 / kill 合并）、loads / stores / kills 记录、调用按 callee 摘要 / ECF 位 / 无 fnspec 的 unknown 或 global 访问处理、finite_function_p、useful_p / finalize / dump；摘要存为 callee 的 `'modref` 声明标志）。dump 中 "Analyzing stmt" 的顺序是 GCC immediate-use 链表的顺序（最近一次 update_stmt / add_phi_arg 的使用在前），为此全链路引入 use-list 时间戳：语句 `'stamp`、PHI 每参数 `'arg-stamps`（gimple.rkt `restamp`），into-SSA 按支配树重写顺序打戳，mfn-replace! / 插入、ccp 的 substitute_and_fold、cfg.rkt 的 replace_uses_by / remove_edge / 前趋重定向按 GCC 的 relink 规则更新（split_edge 不动 PHI 参数），`mfn-imm-uses` 按戳降序。版本分叉：≥ 13 `find_always_executed_bbs`（dump "Always executed bbbs"），≤ 12 只有入口后继；≥ 15 `useful_p` 与 `ignore_nondeterminism_p` 的新形式。未做：fnspec / builtin、static chain / retslot、异常、IPA 模式、points-to |
| fixup_cfg2 / einline | 完成（2026-11-08）：`opt/fixup-cfg.rkt`（execute_fixup_cfg：已知 const/pure 的调用重建虚拟操作数、释放 VDEF、relink，TODO_cleanup_cfg）；einline 不内联，只输出 "Iterations: 0"。测试驱动改为 GCC 的阶段顺序：先对全部函数做 gimplify→SSA（`pass_build_ssa_passes`），再逐函数跑 early 序列，调用的 ECF 位改为按被调声明动态查询（`call-ecf`），与 GCC `gimple_call_flags` 一致 |
| strip_predict_hints / release_ssa | 完成（2026-11-08）：`opt/release-ssa.rkt`（早期只删 PRED_TREE_EARLY_RETURN 的 predict 语句 + cleanup；SSA 版本按相对顺序压缩，"Released N names, P%, removed H holes"，默认定义占位不释放；TODO_remove_unused_locals）。GCC 复用已释放的版本号（free list），移植未复用——夹具上 N/H 一致，因为版本号差异只在被释放的名字之间 |
| IPA 阶段 | 完成子集（2026-11-08）：`opt/ipa.rkt`（ipa-fnsummary 的 size 模型：estimate_num_insns + eliminated_by_inlining_prob + uninlined 开销，无谓词；inline into all callers 的决策 growth_positive_p / check_callers / can_remove_node_now_p；被完全内联的 static 函数移出单元；ipa_discover_variable_flags 的 readonly 发现）、`opt/inline.rkt`（expand_call_inline：split_block、按序复制块与边、SSA 名重映射、参数替换、返回变量 `D.N` 与 `lhs = retvar`、callee 局部变量复制、cleanup + update_ssa（`ssa.rkt update-ssa-vars`：只重命名新变量，不匿名化））。未做：inline 声明函数的小函数路径、IPA pure-const / modref 传播、profile 传播、cdtor / ICF / devirt / IPA-CP / SRA |
| fixup_cfg3 … dce2 | 完成（2026-11-08）：fixup_cfg3、adjust_alignment（恒等）、ccp2（ccp 的 nonzero_p：≥15 输出 "Global Exported: … [irange] …" 的位掩码范围；readonly static 的初始值折叠与 `*&"str"` 字节折叠）、objsz1（恒等）、waccess2（不比较：pointer_query 计数器）、rebuild_frequencies1（≥14，"Profile is consistent"）、backprop、phiprop2 / phiprop（恒等）、forwprop2、alias（≥13 求解器行；12 排除）、retslot、fre3、mergephi2、dse2、dce2（remove_unused_locals 仅 ≥15）。修复：merge_blocks / compact_blocks 未改写 PHI 参数的前趋编号 |
| stdarg … sra | 完成（2026-11-08）：stdarg（恒等；gcc 12 的 TODO_remove_unused_locals 在 pass 开始处）、cdce（恒等）、cselim（只做 loop_optimizer_init (LOOPS_NORMAL) 的 latch 重建，struct.c 的 bb 10；条件存储消除未移植）、copyprop1 / ifcombine / mergephi3 / phiopt2（恒等）、ch2（`opt/ch.rkt`：should_duplicate_loop_header_p 的 15 版（不变量 / 静态分类、ch_win / ch_possible_zero_cost、"复制以得到 do-while"）与 12 版（指令上限、IV 分类）、do_while_loop_p、SEME 区域复制（split_edge_bb_loc 定位、PHI 复制为新名字、入口边重定向与 flush_pending_stmts、add_phi_args_after_copy）、≥15 的 update_profile_after_ch（静态出口：复制的条件折叠为常量、update_bb_profile_for_threading 重算原头块边概率）、≤14 同时拆 preheader 与 latch 边、按名字表的增量 update_ssa（def/use 站点、IDF 的支配裁剪与 prune_unused_phi_nodes、支配树重写，dump "SSA replacement table … Number of blocks to update"）、复制区域的 VN 子集（单参数 PHI 的拷贝传播、条件折叠与操作数规范化）、TODO_cleanup_cfg）、cplxlower1 / bitintlower1（≥14）/ sra（恒等）。修复：merge_blocks 丢失被合并块后继边的概率；replace_uses_by 现在做 fold_stmt 的操作数顺序规范化；forwarder 的"单前驱单后继留给合并"只属 GCC 16（40c037adb36），12/15 不适用；split_edge 的新块计数取边计数 |
| dom2 | 完成（2026-11-08）：`opt/irange.rkt`（GCC 15 irange：子区间 + 显式位掩码，union / intersect / invert / set_range_from_bitmask / update_bitmask 及其 changed 语义，按 GCC 格式打印；ccp2 的 Global Exported 改用它）、`opt/range-info.rkt`（SSA_NAME_RANGE_INFO：set_range_info 的交集与"变了才打印"，duplicate_ssa_name 复制，gimple_range_global）、`opt/bit-value.rkt`（从 ccp.rkt 抽出的 bit_value_unop / binop）、`opt/range-op.rkt`（加减乘、取负、取反、类型转换、六种比较、按位与或异或、min/max 的 fold_range / op1_range / op2_range，wi_fold_in_parts、value_range_with_overflow、update_known_bitmask）、`opt/ranger.rkt`（gimple_ranger + ranger_cache + GORI：range_of_expr / on_entry / on_exit / on_edge / range_of_stmt、prefill_stmt_dependencies、全局表与 temporal 过期判定、range_from_dom / resolve_dom / propagate_cache、exports / def chain / may_recompute_p、条件与 switch 出边范围、compute_operand_range 与逻辑 && / \|\| 组合、export_global_ranges）、`opt/dom.rkt`（pass_dominator：record_edge_info 的条件等价与 derive_equivalences、const_and_copies 与可用表达式表的作用域栈、cprop_into_stmt（SSA_NAME_VALUE 或 ranger 单值，BIV 不传播）、record_equivalences_from_phis、冗余计算 / 冗余存储、fold_cond、前向线程化 thread_across_edge / thread_through_normal_block / thread_around_empty_blocks 的语句计数（struct.c 的 "threading bb 5 kills 2 stmts / up to 17 stmts"）与 cancel_invalid_paths（basic.c 的 latch 线程被取消；4 夹具上无线程留存，已确认）、loop_optimizer_init 的 "Created preheader block"）。gcc-12 的 DOM 用 EVRP 而非 ranger、不输出范围；移植两版共用 ranger，只在 ≥15 打印。未做：关系 oracle、推断范围 oracle（解引用后非空）、path ranger、线程化的 CFG 更新（遇到留存的线程直接报错）、test_for_singularity、可用表达式的别名遍历、frange / prange |
| copyprop2 … fix_loops | 完成（2026-11-08）：`opt/copyprop.rkt`（pass_copy_prop：stmt_may_generate_copy、copy-of 格的 visit_assignment（≥13 gimple_fold_stmt_to_constant_1，12 只看 rhs1）/ visit_phi / 条件，substitute_and_fold 的 PHI 与拷贝删除、use 替换、后继 PHI 常量；copyprop1 也改用它）、`opt/reassoc.rkt`（init_reassoc 的 rank（默认定义 3、4…，块 ++rank<<16，PHI 的循环偏置只在 reassoc2）、break_up_subtract 的语句编号、后支配树逆序遍历、linearize_expr_tree / linearize_expr、sort_by_operand_rank、常量合并与 eliminate_using_constants、swap_ops_for_binary_stmt、串行 rewrite_expr_tree（改变的内层语句取新 SSA 名，find_insert_point 定位）、无用已访问语句的删除与释放）、SSA 名空闲表（`state.rkt`：make_ssa_name 先取最近释放的版本，pass 结束 flush，release_ssa 清空；`passes.rkt run-pass` 把未登记的释放按降序入队——struct.c 的 reassoc1 因此得到 `_10`）、sink1（loop_optimizer_init + `cfg.rkt split-edges-for-insertion`：关键边与控制语句出边，switch 源边在新块建 case 标号 `<L9>`）、lim2（只有 LOOPS_NORMAL 规范化，ccp.c 的 "Created preheader block"）、dce3 / forwprop3 / ccp3 / dse3 / dce4（复用）、phiopt3 / pow（gcc-12 为 sincos）/ laddress / walloca2 / fix_loops（恒等）。未做：reassoc 的 range test、undistribute、加法转乘法、并行重写、减法拆分；sink 的语句下沉；lim 的不变量外提 |
| loop … optimized | 后续：循环流水线（loop、loopinit、sccp、cddce2、ivcanon、cunroll、ivopts、loopdone、no_loop）、尾段 … 见 `doc/gcc-pass-progress.md` 第 2 节 |

profile_estimate 移植记录：

- 预测的记录顺序决定 dump 顺序与去重结果（`prune_predictions_for_bb` 按出现顺序找 single edge / edge pair duplicate），所以 `tree_bb_level_predictions` → `predict_loops`（内层先）→ 逐块 `tree_estimate_probability_bb` 的顺序与 GCC 一致，PHI 级别的 `expr_expected_value` 只做常量 PHI 子集。
- 频率传播用 `opt/sreal.rkt` 逐位复刻 GCC sreal（31 位有效数、归一化舍入、`shift_right` 舍入），计数才能逐个相等：如 bb 频率 1/(1−0.9091) 的 11.0011005… 与 `freq_max = 2^30 / max` 后的 97603128 / 976138696。
- `split-edge` 建 preheader / latch 块时新边概率恒为 always，块放置遵循 `split_edge_bb_loc`（struct.c 的 latch 块 bb 10 排在 bb 3 之后）。
- 夹具 `calls.c` 的 `use` 函数：bb 5 的 `twice` 调用不触发 call 启发式，因为 `twice` 在之前已被 local-pure-const1 判定为 const（函数按 callee-first 顺序完整跑完 early 序列）；这是把 local-pure-const1 一并移植的原因。

fre1 移植记录：

- -O1 下 `pass_fre` 不迭代（`iterate_p = may_iterate && optimize > 1`），因此是 `do_rpo_vn_1 (iterate=false, eliminate=true)`：`rev_post_order_and_mark_dfs_back_seme` 的 SCC 连续 RPO（含顶层 SCC 区间 → `max_rpo`）、RPO 序工作表、`process_bb` 内联 `eliminate_stmt`、`rpo_elim` 的可用性链表（值 → (块 . leader)）与 `dominated_by_p_w_unex` 的 exec-idom 链。
- -O1 没有 `-fstrict-aliasing`：所有 alias set 为 0，预言机的 TBAA 分支不走；没有 points-to（ealias 未移植）时指针解引用对取址变量保守地 may-alias，与 GCC 无 PTR_INFO 时一致。夹具中 `_11 = *p_18(D)` 的 store→load 转发靠同基址 `p_18` 的偏移区间判别（`MEM[p + 4]` 与 `*p` 不重叠），不需要 PTA。
- 引用键 = `copy_reference_ops_from_ref` 的操作数记录列表（SSA 操作数取值、地址经 `p = &x` / `p = q p+ C` 定义前推、`&a.b` 折叠），相等按 `vn_reference_eq (lexical=false)` 的偏移合并比较；表按 vuse 版本分桶线性比较。
- 未移植：迭代驱动与回退、表达式插入（needs_insertion）、`visit_nary_op` 的转换/截断/BIT_AND 载入/BIT_FIELD_REF/取反/移位特例、`lookup_3` 的部分定义与常量内存折叠、modref 摘要、跨块 PHI 等价（带控制条件的 `vn_phi_eq`）、value id。gcc-12 差异（`update_exec_idom`、PHI 前驱重排 `seen_nonconstant`）未分叉，夹具未触发。

forwprop1 移植记录：

- pass 内部用 `mfn`（gcfg→mfn→gcfg）：GCC 的 gsi 游标 + immediate-use 风格算法直接移植；GCfg 仍是 pass 之间的格式，CFG 结构（块/边）不经 mfn 改动。
- 版本分叉：gcc ≥ 13 才有 EDGE_EXECUTABLE 可达性跳块与 `find_taken_edge` 标边；gcc ≥ 14 才有 `simple_dce_from_worklist` 与 `to_remove_defs`（之前只删排队语句）。
- 夹具触发的效果：拷贝格传播（`n.0_16 = n_24` 等）、`fold_stmt` 的交换律操作数规范化（`r_13 + _7` → `_7 + r_13`，依 `tree_swap_operands_p` 的 SSA 版本号）、`p+ 4` 传入解引用（`*_9` → `MEM[(int *)p_18(D) + 4B]`，`forward_propagate_addr_expr`）、match.pd 两条规则串联 `-(A + 1)` → `-1 - A` → `~A`。
- match.pd 只移植了 simplify.rkt 头部列出的规则；结果需要多于一条语句的规则不应用。`combine_cond_expr_cond` 只覆盖常量、比较结果与 0/1 的比较、布尔转换与常量的比较。未移植：复数/向量/CONSTRUCTOR 改写、聚合拷贝/清零传播与 DSE、builtin 调用简化、rotate/ctz/长乘法识别、`__builtin_unreachable`、`get_addr_base_and_unit_offset` 的 valueize 变体。

ccp1 移植记录：

- 引擎按 trunk 实现，三处按 GCC 版本分叉：gcc ≤ 12 用双工作表（`cfg_blocks_back`/`ssa_edge_worklist_back`，r13-5077 去掉）；gcc ≥ 13 的 `set_lattice_value` 对"常量 → 拷贝"不做 meet（r13-3875 "Restore CCP copy propagation"，这决定 `_5 = s_1; return _5;` 是否被折成 `return s_1;`）；gcc ≥ 14 的 finalize 用 `simple_dce_from_worklist`（r14-402），之前只删排队的语句。
- 拷贝传播来源：VARYING 的 SSA 操作数以"值 = 该 SSA 名、mask = -1 的 CONSTANT"进入格（`get_value_for_expr`、`evaluate_stmt` 末尾），finalize 时 `get_constant_value` 把它当作可替换值。
- cfg.rkt 为 SSA 之后的 cleanup 补了两处：`remove_edge` 同步删 PHI 实参（与 preds 一样 unordered_remove），`merge_blocks` 把 B 的单实参 PHI 用 `replace_uses_by` 传播掉。forwarder 删除在 dest 有 PHI 时的实参复制仍未做。
- 未移植：`evaluate_stmt` 的 builtin 分支（malloc/alloca/assume_aligned/bswap）、nonzero-bits/range info、ipa-cp 参数位、`&DECL` 以外的指针对齐（且 dump 无 DECL_ALIGN，用对象 TYPE_ALIGN 代替，与 GCC 的局部变量对齐提升有差）、`fold_stmt` 的 `follow_single_use_edges` 与 match.pd 恒等式、`fold_const_aggregate_ref`。插件未导出 `DECL_BUILT_IN_CLASS`，builtin 判定按 `__builtin_` 前缀。

## 6. 执行顺序与依赖

```
Phase 0 ──> Phase 1 ──> Phase 2 ──> Phase 3 ──> Phase 5 / Phase 6
                 └──────> Phase 4（与 2/3 独立，可并行）
Phase 6.1 插件（独立目录）不依赖任何 Phase，可立即并行启动；6.2 依赖 Phase 2.4（自有 prefab BlockId）与 Phase 3.3（pass 框架接入）。
```

Phase 0 之前不能跑任何测试，是唯一的硬前置。Phase 1 #3（支配 RPO）可能改变 SSA 输出，必须在 Phase 3 拆分前完成，否则拆分的"输出 equal?"验收基线会漂移。

## 7. 提交约定（沿用现有）

- 每个 commit 的 message 以 `(FEAT|FIX|REFACT|REFACTOR|TEST "subject" "body")` 形式同步写入根目录 `.commit`。
- body 末尾列 `Modified:` 文件清单与测试计数变化（如 `258 → 260 (+2)`）。
- 提交前至少跑本阶段验收命令。

## 8. 决策记录

### 2026-11-05

1. **prefab struct 保留**。IR 中的引用尽可能以 typed id（`VarId`、`BlockId`、`NodeId` 等 prefab 包装的整数）存储，保证 `write`/`read` 可序列化、可持久化。Phase 2.4 的同名冲突按"加前缀、仍为 prefab"处理，不改用非 prefab。
2. **`component/aarch64/backend/emit.rkt` 的缺陷挂起**（Phase 4 #2-#5 不修）。后端不再走直出汇编文本模式，未来接入 rktasm。Phase 4 只保留 parser（#1、#6）、liveness/regalloc（#7、#8）、interp（#9）项。
3. **pass 框架保留**（Phase 3.3 选项 A）：`run-pass` 校验 `requires`，java 与 aarch64-asm pipeline 接入 `pass.rkt`。
4. **新开发方向**：GCC 插件导出 AST/GIMPLE，接入当前框架作为新前端（对齐细节见 Phase 6，待确认）。
5. **ordered-map 不保留**，按键类型换成 `racket/intmap` / 不可变 hash；typed-id 键用 `idmap`（intmap 存储，见 Phase 0.2）。标准库已有的 pvector 函数优先于自有版本（`pvector-map`/`pvector-filter` 等按标准库参数顺序：pvector 在前）。
6. **GCC 前端走 GENERIC**（`PLUGIN_PRE_GENERICIZE`，逐函数），追求保留尽可能多的原始 AST 信息；GIMPLE 导出不做。模块名 `gccast`。
7. **只支持 GCC 15**（本机 Homebrew gcc-15 15.2.0）。
8. **插件独立目录、独立开发、独立许可证（GPLv3 兼容）**，只负责序列化中间表示，不含任何分析。
9. 类型信息放 `Cfg-info` 中 `VarId → type` 的 intmap。
10. 内存状态边：AST 无虚拟操作数；在 lowering 之后线程化内存状态，再由 cfg-to-rvsdg 建状态边。Java 链路目前也没有内存状态边（已验证 `cfg-to-rvsdg.rkt` 无 state 处理，仅有 `current-exn-out`），该工作对两个前端共用。
11. **GCC 插件源码由 Racket 生成器按版本生成**（细节见 6.1），本机以 gcc-15 为主、gcc@12 作第二版本做兼容验证。
12. **GENERIC → GIMPLE 降级按 GCC trunk 算法逐函数改写**（6.3），用 gcc-15 的 `-fdump-tree-*` 作对齐基准；在 `alpha` 分支开发。
14. **优化做在 GIMPLE 层，逐 pass 对齐 GCC -O1 的 dump**（2026-11-06，用户在 RVSDG 层优化 / GIMPLE 层对齐 / 先只做兼容层三案中选定）。Java 链路暂不推进；RVSDG 侧只保证能承接。见 Phase 7。
13. **GIMPLE 层改为持久化数据 + 纯函数式状态线程，并泛化 SSA / CFG→RVSDG 算法**（2026-11-06，设计见 `doc/gimple-functional-ir.md`）。可变的 `src/component/gccast/` 是移植期权宜，按阶段 S1–S6 替换为 `src/kernel/ir/gimple/` + `src/component/gimple/`，每阶段以现有 dump 对齐测试验收。用户在"持久化值 + 命令式算法"与"纯函数式状态线程"两案中选了后者。
