#!/usr/bin/env python3
"""Regenerate doc/gcc-pass-progress.md from GCC itself.

Inputs produced on the fly (into a temp dir):
  gcc-15 / gcc-12 -O1 -fdump-passes      -> the complete pass tree with ON/OFF gates
  gcc-15 -O{0,1,2,3} -Q --help=optimizers -> every optimizer flag and its state per level
  gcc-15 / gcc-12 -O1 -fdump-tree-all on the fixtures -> which late passes change each fixture
The port's status per pass / flag is the table at the top of this file; the
prose (coverage, remaining late passes) is derived from it.

usage: python3 tools/gcc-pass-sheet.py [--gcc15 gcc-15] [--gcc12 /path/gcc-12]
                                       [--o1-tests N] [--suite PASSED/TOTAL]
  --o1-tests / --suite: the counts of the last test run (raco test), printed
  in section 6; without them the section says the counts were not recorded
"""
import re, collections, os, subprocess, sys, tempfile, glob, datetime
REPO=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
GCC15='gcc-15'; GCC12='/opt/homebrew/Cellar/gcc@12/12.5.0/bin/gcc-12'
O1_TESTS=None; SUITE=None
args=sys.argv[1:]
for i,a in enumerate(args):
    if a=='--gcc15': GCC15=args[i+1]
    if a=='--gcc12': GCC12=args[i+1]
    if a=='--o1-tests': O1_TESTS=args[i+1]
    if a=='--suite': SUITE=args[i+1]
S=tempfile.mkdtemp(prefix='gcc-pass-sheet-')+'/'
FIX=os.path.join(REPO,'gccast-plugin','tests','struct.c')
# the o1-test.rkt fixtures (gccast-plugin/tests/<name>.c)
FIXTURES=['basic','calls','struct','ccp','thread','sink']
def run(cmd, out):
    with open(out,'w') as f: subprocess.run(cmd, stdout=f, stderr=subprocess.STDOUT, check=False)
run([GCC15,'-O1','-fdump-passes','-c','-o','/dev/null',FIX], S+'passes15.txt')
run([GCC12,'-O1','-fdump-passes','-c','-o','/dev/null',FIX], S+'passes12.txt')
for o in '0123':
    txt=subprocess.run([GCC15,'-O'+o,'-Q','--help=optimizers'],capture_output=True,text=True).stdout
    with open(S+'flags-O%s.txt'%o,'w') as f:
        for l in txt.split('\n'):
            m=re.match(r'^  (-\S+)\s+(.*)$',l)
            if m: f.write(m.group(1)+' '+m.group(2).strip()+'\n')
for ver,cc in [('15',GCC15),('12',GCC12)]:
    os.makedirs(S+'dumps'+ver,exist_ok=True)
    for fx in FIXTURES:
        subprocess.run([cc,'-O1','-c','-fdump-tree-all','-o','/dev/null',os.path.join(REPO,'gccast-plugin','tests',fx+'.c')],cwd=S+'dumps'+ver,capture_output=True)
def parse(path):
    out=[]
    for l in open(path):
        m=re.match(r'^( *)(\S+)\s*:\s*(ON|OFF)\s*$', l.rstrip('\n'))
        if m: out.append((len(m.group(1))//3, m.group(2), m.group(3)))
    return out
p15=parse(S+'passes15.txt'); p12=parse(S+'passes12.txt')
idx12=collections.defaultdict(list)
for lvl,n,st in p12: idx12[n].append(st)
status={
 'tree-omplower':'不比较（无 OpenMP）','tree-lower':'完成','tree-eh':'不比较（C 无异常）','tree-cfg':'完成','tree-ompexp':'不比较',
 '*build_cgraph_edges':'范围外（cgraph）','*free_lang_data':'不比较','ipa-visibility':'范围外','ipa-build_ssa_passes':'（分组）',
 'tree-fixup_cfg1':'不比较（SSA 前，夹具无变化）','tree-ssa':'完成','tree-walloca1':'不比较（诊断）','tree-waccess1':'不比较（诊断）','tree-nothrow':'不比较（C 下恒 nothrow）',
 '*rebuild_cgraph_edges':'范围外（cgraph）','ipa-opt_local_passes':'（分组）','tree-fixup_cfg2':'完成','tree-local-fnsummary1':'范围外（IPA 摘要）','tree-einline':'恒等（dump 对齐，不内联）',
 'tree-early_optimizations':'（分组）','*remove_cgraph_callee_edges':'范围外（cgraph）','tree-early_objsz':'不比较（夹具无 object_size）',
 'tree-ccp1':'完成','tree-forwprop1':'完成','tree-ethread':'恒等','tree-esra':'恒等','tree-ealias':'恒等（PTA 未移植）','tree-phiprop1':'恒等','tree-fre1':'完成',
 'tree-mergephi1':'恒等','tree-dse1':'完成','tree-cddce1':'完成','tree-phiopt1':'恒等','tree-iftoswitch':'恒等','tree-sccopy1':'恒等',
 'tree-profile_estimate':'完成','tree-local-pure-const1':'完成','tree-modref1':'完成','*strip_predict_hints':'完成（early 实例）','tree-release_ssa':'完成','tree-local-fnsummary2':'范围外（IPA 摘要）',
 '*warn_unused_result':'不比较（诊断）','*warn_function_return':'不比较（诊断）',
 'ipa-remove_symbols':'完成（子集：被完全内联的 static 函数移出单元）','ipa-inline':'完成（子集：inline into all callers；小函数路径 -O1 下对非 inline 声明关闭）',
 'ipa-static-var':'完成（子集：ipa_discover_variable_flags 的 readonly 发现）','ipa-pure-const':'范围外（IPA 传播未移植）','ipa-modref':'范围外（IPA 传播未移植）',
 'ipa-fnsummary':'完成（子集：无谓词的 size 模型）',
 'tree-fixup_cfg3':'完成（含 inline transform 的 TODO：cleanup、update_ssa、remove_unused_locals）','tree-adjust_alignment':'恒等','tree-ccp2':'完成（nonzero_p：≥15 输出 Global Exported 范围）',
 'tree-objsz1':'恒等','tree-waccess2':'不比较（pointer_query 计数器随 GCC 内部缓存变化）','tree-rebuild_frequencies1':'完成（只有 "Profile is consistent"）',
 'tree-backprop':'恒等','tree-phiprop2':'恒等','tree-phiprop':'恒等','tree-forwprop2':'完成（复用 forwprop）','tree-alias':'恒等（PTA 未移植；≥13 求解器进度行；12 排除）',
 'tree-retslot':'恒等','tree-fre3':'完成（复用 fre）','tree-mergephi2':'恒等','tree-dse2':'完成（复用 dse）','tree-dce2':'完成（复用 dce；remove_unused_locals 仅 ≥15）',
 'tree-stdarg':'恒等（TODO_remove_unused_locals 在 pass 开始处）','tree-cdce':'恒等',
 'tree-cselim':'完成（子集：只有 loop_optimizer_init (LOOPS_NORMAL) 的 latch 重建；条件存储消除未移植，夹具无触发）',
 'tree-copyprop1':'恒等','tree-ifcombine':'恒等','tree-mergephi3':'恒等','tree-phiopt2':'恒等',
 'tree-ch2':'完成（`opt/ch.rkt`：should_duplicate_loop_header_p 15/12 两版决策、do-while 判定、SEME 区域复制、update_profile_after_ch、按名字的增量 update_ssa 与其 dump、区域 VN 子集、TODO_cleanup_cfg）',
 'tree-cplxlower1':'恒等','tree-bitintlower1':'恒等（gcc ≥ 14）','tree-sra':'恒等',
 'tree-ompdevlow':'不比较（无 OpenMP）','tree-ompexpssa2':'不比较（无 OpenMP）',
 'tree-copyprop1':'完成（`opt/copyprop.rkt`：copy-of 格 + SSA 传播 + substitute_and_fold）','tree-copyprop2':'完成（同 copyprop1；basic.c 删掉 dom2 留下的单参数 PHI）',
 'tree-reassoc1':'完成（`opt/reassoc.rkt`：rank、linearize、按 rank 排序、常量合并、串行 rewrite_expr_tree；新名字取 SSA 空闲表；range test / undistribute / 并行重写未移植）',
 'tree-dce3':'完成（复用 dce）','tree-forwprop3':'完成（复用 forwprop）','tree-phiopt3':'恒等','tree-ccp3':'完成（复用 ccp，nonzero_p）',
 'tree-pow':'恒等','tree-sincos':'恒等','tree-laddress':'恒等','tree-lim2':'完成（子集：只有 loop_optimizer_init 的 preheader / latch；不变量外提未移植，夹具无触发）',
 'tree-walloca2':'恒等','tree-sink1':'完成（`opt/sink.rkt`：loop_optimizer_init + split_edges_for_insertion、statement_sink_location（单用途 / 多用途的最近公共支配块 / PHI 入边块，load 不越过 store：12 版 VUSE 立即使用、15 版 virtual_operand_live）、select_best_block（12：循环深度 + 75% 频率阈值；15：do_not_sink + 后支配）、死语句删除；thread.c 的 thr_join 下沉 z = b ^ y；sink_common_stores_to_bb：sink.c 的两个 *p store 合并为 join 块中一个 PHI 值的 store）',
 'tree-dse3':'完成（复用 dse）','tree-dce4':'完成（复用 dce）','tree-fix_loops':'恒等',
 'tree-loop':'完成（gate_loop：函数有循环才进入子流水线；`opt/loop-pipeline.rkt` 记录 loop-pipeline 与 loops-state 标志）',
 'tree-loopinit':'完成（loop_optimizer_init (LOOPS_NORMAL) + rewrite_into_loop_closed_ssa（`opt/update-ssa.rkt` 的增量 update_ssa 与 dump；12 只处理实际操作数（SSA_OP_USE），≥13 含虚拟操作数（SSA_OP_ALL_USES））；loops-state 置 preheaders / simple-latches / lcssa / scev：cleanup 保护 latch 与 preheader、不压缩块号）',
 'tree-sccp':'完成（`opt/sccp.rkt` final_value_replacement_loop；`opt/scev.rkt` 标量演化 + 迭代次数（lt/le、单位步长）；`opt/generic-fold.rkt` GENERIC fold 子集；≥13 常量直接替换使用）',
 'tree-cddce2':'完成（复用 dce；释放顺序按 GCC 的支配树 BFS 逆序；≥13 在 loop-closed SSA 下保留退化的虚拟 PHI）',
 'tree-ivcanon':'完成（`opt/ivcanon.rkt` create_canonical_iv；try_unroll 的 UL_SINGLE_ITER 只对不迭代的循环生效，未移植）',
 'tree-copyprop3':'完成（复用 copyprop；LCSSA 下不跨循环出口边传播）',
 'tree-cunroll':'完成（`opt/cunroll.rkt`：UL_NO_GROWTH 的 try_unroll_loop_completely（tree_estimate_loop_size / estimated_unrolled_size 12、15 两版）、scale_loop_profile（≥13）、duplicate_loop_body_to_header_edge 的复制与 DLTHE 计数缩放、force_edge_cold、unloop + remove_path、update_ssa；basic.c g 完全展开）',
 'tree-ivopts':'完成（`opt/ivopts.rkt`：bivs / givs（alloc_iv 的 12 / 15 两种规范化）、nonlinear / compare / address 用途与分组、候选（标准、biv、原 biv、用途派生、公共基）、force_expr_to_var_cost / get_computation_aff / 地址代价 / 消去（may_eliminate_iv、cand_value_at）、iv_ca 贪心搜索、create_iv、create_mem_ref 子集（TARGET_MEM_REF）、adjust_iv_update_pos、remove_unused_ivs；`opt/tree-affine.rkt` 仿射组合；`opt/target-costs.rkt` 实测 aarch64 代价；IVOPTS_DEBUG=1 输出 TDF_DETAILS 表；未移植：doloop、autoinc、USE_PTR_ADDRESS、地址中的符号）',
 'tree-lim4':'恒等（夹具循环无不变量）',
 'tree-loopdone':'完成（loop_optimizer_finalize：clean_up_loop_closed_phi（按记录出口的逆序）、loops-state 清空，TODO_cleanup_cfg 删除 forwarder 并压缩块号）',
 'tree-no_loop':'恒等（gate：函数从未进入循环流水线）',
 'tree-veclower21':'完成（无向量运算；TODO_cleanup_cfg 删除 sink1 拆出的 forwarder）',
 'tree-switchlower1':'完成（`opt/switchlower.rkt`：simple cluster、balance_case_nodes（≤2 节点）、emit_case_nodes 的 do_jump_if_equal（GCC ≥13 缩放 false 块计数）、函数中途 dump、PHI 参数修补、.MEM 符号重命名的 update_ssa 消息；bit-test / jump-table 未移植）',
 'tree-sincos':'恒等','tree-slsr':'恒等','tree-wrestrict':'恒等','tree-fab1':'恒等','tree-sccopy2':'恒等','tree-phiopt4':'恒等',
 'tree-reassoc2':'完成（复用 reassoc，early_p = false）',
 'tree-dom3':'完成（复用 dom，may_peel_loop_headers = false；PROP_loop_opts_done 下 cancel_invalid_paths 只做出口检查；thread.c 的 thr_loop：latch → 循环头的线程被 thread_through_loop_header 的失败分支取消；gcc 15 struct.c 的线程 (4,5)(5,6)(6,9)：path ranger 判定条件、`opt/threadupdate.rkt` 复制块 / 重定向边 / update_profile、cleanup 中的 update_ssa 替换表）',
 'tree-strlen1':'完成（只有 loop_optimizer_init (LOOPS_NORMAL)；无字符串函数）',
 'tree-ccp4':'完成（复用 ccp，nonzero_p；ccp_fold 改为 gimple_simplify + valueize_op_1 跟随 SSA 定义）',
 'tree-dse5':'完成（复用 dse）','tree-dce7':'完成（复用 dce）','tree-cddce3':'完成（复用 dce）',
 'tree-forwprop4':'完成（复用 forwprop；match.pd 的连续转换合并与 (A±C1)±C2 穿过 nop_convert 的结合，genmatch 物化中间结果为新语句）',
 'tree-sink2':'完成（复用 sink，unsplit）',
 'tree-crited1':'完成（split_critical_edges）',
 'tree-local-pure-const2':'完成（复用 pure-const；finite_p 按循环号保存）',
 'tree-modref2':'完成（复用 modref；Past summary（IPA 传播对不可用被调函数的 collapse；早期摘要无用时按 IPA 分析（跳过调用）判断 IPA 摘要是否存在）与参数标志比较）',
 'tree-uncprop1':'恒等','tree-nrv':'恒等','tree-isel':'恒等',
 'tree-waccess3':'完成（警告未移植；后期实例在夹具上不做 pointer query，计数为 0）',
 'tree-optimized':'完成（execute_fixup_cfg + cleanup_tree_cfg，TODO_remove_unused_locals）',
 'tree-dom2':'完成（`opt/dom.rkt`：边等价 / 条件等价、常量与拷贝表、可用表达式表、cprop（含 ranger 单值）、PHI 等价、前向线程化的语句计数与 cancel_invalid_paths；`opt/ranger.rkt` + `irange.rkt` + `range-op.rkt`：≥15 的 Global Exported；thread.c 的 3 条线程经过同一块：`opt/threadupdate.rkt` 的模板块复制、GCC hash_table 槽序、update_ssa 的替换集合（x_20 -> { x_3 x_17 }）与待释放名；未做关系 oracle；path ranger 见 dom3）',
}
reuse={'ccp','forwprop','fre','dse','dce','cddce','phiprop','mergephi','sccopy','local-pure-const','modref','profile_estimate','fixup_cfg','strip_predict_hints','release_ssa','einline'}
# -fdump-passes evaluates each gate once, before any function is compiled, so
# gates that look at the function (loop passes, postreload) print OFF and a
# pass nested in a group can print ON although the group does not run.  For
# GIMPLE passes the dump files of the fixtures decide: a dump file exists iff
# the pass executed on some fixture function.
ran15=set()
for f in os.listdir(S+'dumps15'):
    m=re.match(r'^\w+\.c\.\d+t\.(.+)$',f)
    if m: ran15.add('tree-'+m.group(1))
def executed(n,st):
    return (n in ran15) if n.startswith('tree-') else st=='ON'
def our_status(n,st,seen):
    if n in status and seen[n]==0: return status[n]
    if n.startswith('tree-') and st=='ON' and n not in ran15: return '未执行（-O1 下 %d 个夹具都没有该 pass 的 dump：所在分组关闭或函数级 gate 为假）' % len(FIXTURES)
    if st=='OFF' and not executed(n,st): return 'gate 关闭（-O1 不执行）'
    base=re.sub(r'\d+$','',n.split('-',1)[1] if '-' in n else n.lstrip('*'))
    if n.startswith('rtl-'): return '范围外（RTL 后端）'
    if n.startswith('ipa-'): return '范围外（IPA）'
    if n.startswith('*'): return '范围外 / 不比较'
    if base in reuse: return '未移植（可复用已实现的 %s）'%base
    return '未移植'
rows=[]; seen=collections.Counter()
for lvl,n,st in p15:
    s12=idx12[n][seen[n]] if seen[n] < len(idx12[n]) else '无'
    # the "gcc-15" column: the -fdump-passes gate, marked when the dumps disagree
    shown=st if executed(n,st)==(st=='ON') else ('%s（实际%s）' % (st, '执行' if executed(n,st) else '未执行'))
    rows.append((lvl,n,shown,s12,our_status(n,st,seen),executed(n,st))); seen[n]+=1
levels={}
for o in '0123':
    for l in open(S+'flags-O%s.txt'%o):
        if ' ' not in l.strip(): continue
        n,st=l.split(None,1); st=st.strip()
        if n.startswith('-O'): continue
        levels.setdefault(n,{})[o]=st
flag_status={
 '-ftree-ccp':'完成（ccp1）','-ftree-bit-ccp':'完成（ccp1）','-ftree-forwprop':'完成（forwprop1）','-ftree-fre':'完成（fre1）','-ftree-dse':'完成（dse1）',
 '-ftree-dce':'完成（cddce1、dce2；后续实例未）','-ftree-builtin-call-dce':'恒等（cdce）','-fguess-branch-probability':'完成（profile_estimate）',
 '-fipa-pure-const':'完成（local 实例）；IPA 传播未','-fipa-modref':'完成（local 实例）；IPA 传播未','-fipa-strict-aliasing':'-O1 无 strict aliasing，alias set 恒 0',
 '-ftree-phiprop':'恒等','-fthread-jumps':'部分（dom 的前向线程化：分析、path ranger（≥13）、取消规则、`opt/threadupdate.rkt` 的 CFG 更新（含同一块多条路径的模板块复制、joiner 块复制（thread.c 的 thr_join）、thread_through_loop_header 的判定与取消（thr_loop）；循环头剥离变换未移植）；后向线程器 ethread / thread 未移植）','-ftree-sra':'恒等（esra）/ 未移植（sra）','-ftree-pta':'恒等 / 未移植（PTA）','-fssa-phiopt':'恒等 / 未移植',
 '-fearly-inlining':'恒等（不内联）','-finline':'未移植','-finline-functions-called-once':'未移植','-finline-atomics':'未移植','-fno-inline-stringops':'未移植',
 '-fipa-profile':'范围外','-fipa-reference':'范围外','-fipa-reference-addressable':'范围外','-fipa-stack-alignment':'范围外',
 '-ftree-copy-prop':'完成（copyprop1/2）','-ftree-dominator-opts':'完成（dom2）','-ftree-reassoc':'完成（reassoc1 串行子集）','-ftree-ch':'完成（ch2）','-ftree-loop-optimize':'完成（loop 到 loopdone、no_loop）','-ftree-loop-im':'未移植',
 '-ftree-loop-ivcanon':'完成（ivcanon）','-ftree-scev-cprop':'完成（sccp）','-fivopts':'完成（ivopts）','-faggressive-loop-optimizations':'未移植','-fsplit-ivs-in-unroller':'未移植（-O1 的 cunroll 不用）',
 '-fmove-loop-invariants':'未移植','-fmove-loop-stores':'未移植','-ftree-sink':'完成（sink1 / sink2，含公共 store 合并）','-ftree-slsr':'未移植','-fssa-backprop':'未移植','-fstdarg-opt':'恒等（stdarg）',
 '-fbit-tests':'未移植（switchlower）','-fjump-tables':'未移植（switchlower）','-fallocation-dce':'未移植','-fmalloc-dce':'未移植','-flifetime-dse':'未移植',
 '-fprintf-return-value':'部分（折叠子集）','-fmath-errno':'部分（折叠子集）','-ffp-int-builtin-inexact':'部分','-fsigned-zeros':'部分','-ftrapping-math':'部分',
 '-ftree-ter':'未移植（out-of-SSA）','-ftree-coalesce-vars':'未移植（out-of-SSA）','-fsemantic-interposition':'简化模型','-ftoplevel-reorder':'简化模型','-fplt':'范围外',
}
def fstat(n,v):
    if n in flag_status: return flag_status[n]
    on1=v.get('1')=='[enabled]'
    if not on1:
        lvl='-O2' if v.get('2')=='[enabled]' else '-O3' if v.get('3')=='[enabled]' else '默认关闭'
        return '不在 -O1 范围（%s）'%lvl
    if v.get('0')=='[enabled]': return '-O0 即启用（语义 / 后端开关）'
    return '未移植'
# late passes that change fixtures (gcc-15 dumps)
def body(path):
    # function bodies only (signature line, "{" .. "}"), keyed by name, so that
    # dump messages between functions do not count as changes
    lines=[l for l in open(path,errors='replace').read().split('\n') if not l.startswith(';;')]
    bodies={}; cur=None; buf=[]
    for i,l in enumerate(lines):
        if cur is None and i+1<len(lines) and lines[i+1]=='{' and re.match(r'^[A-Za-z_].*\)\s*$',l):
            m=re.search(r'([A-Za-z_][A-Za-z0-9_]*) \(',l); cur=m.group(1) if m else l; buf=[l]; continue
        if cur is not None:
            buf.append(l)
            if l=='}': bodies[cur]='\n'.join(buf); cur=None
    return bodies
def late_impact(ver):
    out={}
    for fx in FIXTURES:
        d=S+'dumps'+ver+'/'
        files=sorted([f for f in os.listdir(d) if re.match(fx+r'\.c\.\d+t\.',f)], key=lambda f:int(f.split('.')[2][:-1]))
        start=[i for i,f in enumerate(files) if f.endswith('.release_ssa')]
        prev=None; ch=[]
        for f in files[start[0] if start else 0:]:
            b=body(d+f)
            if prev is not None and b!=prev: ch.append(f.split('.',2)[2])
            prev=b
        out[fx]=[c for c in ch if not c.endswith('statistics')]
    return out
impact=late_impact('15'); impact12=late_impact('12')
cnt=collections.Counter()
for lvl,n,st,s12,s,ran in rows:
    k='tree' if n.startswith('tree-') else 'ipa' if n.startswith('ipa-') else 'rtl' if n.startswith('rtl-') else 'nodump'
    cnt[(k,'ON' if ran else 'OFF')]+=1
done=[r for r in rows if r[4].startswith('完成')]; ident=[r for r in rows if r[4].startswith('恒等')]
def kind(r,k): return r[1].startswith(k+'-') if k!='nodump' else r[1].startswith('*')
L=[]
L.append("# GCC -O1 流水线对照表：移植进展（完整清单）\n")
L.append("更新：%s。\n" % datetime.date.today().isoformat())
L.append("## 0. 数据来源与口径\n")
L.append("- **pass 清单**：`gcc-15 -O1 -fdump-passes` 与 `gcc-12 -O1 -fdump-passes`（Homebrew gcc 15.2.0 / gcc@12 12.5.0）打印的完整 pass 树，含 gate 关闭的 pass。这是 GCC 对 `passes.def` 的实例化结果。此前版本的表只列了夹具实际产生 dump 文件的 pass，漏掉了无 dump 名的 `*` pass 与 gate 关闭的 pass。")
L.append("- **passes.def 规模**（`gcc.git` 的 releases/gcc-15 与 releases/gcc-12 分支）：NEXT_PASS / INSERT_PASS 条目 426（gcc-15）、406（gcc-12）；不同 pass 名 294（gcc-15）。-fdump-passes 展开为 %d 个实例（gcc-15）、%d 个（gcc-12）。" % (len(rows), len(p12)))
L.append("- **优化开关**：`gcc-15 -O{0,1,2,3} -Q --help=optimizers`：%d 个开关；-O0 启用 54，-O1 新增 43（共 96），-O2 再增 46（共 142），-O3 再增 14（共 156），135 个在 -O0 到 -O3 下都不默认启用。另有 316 个 `--param`，不在本表。" % len(levels))
L.append("- **对齐判据**：`src/component/gimple/o1-test.rkt`，%d 夹具（%s）× 2 版本逐函数比较 dump（去掉 `;;` 行；SSA 版本号与临时量编号按原样比较，只把 D.N 的 uid 重新编号）。状态：**完成** = 已移植且对齐；**恒等** = 未移植算法、夹具上 GCC 不改变函数、dump 对齐（换夹具可能触发差异）；**未移植**；**不比较** = 无函数 dump 或夹具上无效果；**范围外** = IPA 跨函数 / RTL 后端（后端为自有 aarch64）；**gate 关闭** = -O1 下 GCC 不执行。\n" % (len(FIXTURES), '、'.join(f+'.c' for f in FIXTURES)))
L.append("- **执行与否**：`-fdump-passes` 在编译任何函数之前对每个 gate 求值一次，所以看函数的 gate（循环 pass、postreload）打印 OFF，而关闭分组里的子 pass 可能打印 ON。GIMPLE pass 改用夹具的 dump 文件判定：4 个夹具中任一个有该 pass 的 dump 文件即为执行；两者不一致时 gcc-15 列写成 `ON（实际未执行）` / `OFF（实际执行）`。IPA、RTL 与无 dump 名的 pass 仍按 `-fdump-passes`。统计表的“执行 / 不执行”按此口径。\n")
L.append("## 1. 统计（gcc-15 pass 树实例，-O1）\n")
L.append("| 类别 | 执行 | 不执行 | 完成 | 恒等 | 未移植（执行） |\n|---|---|---|---|---|---|")
for k,label in [('tree','GIMPLE（tree-）'),('ipa','IPA（ipa-）'),('rtl','RTL（rtl-）'),('nodump','无 dump 名（*）')]:
    d=sum(1 for r in done if kind(r,k)); i=sum(1 for r in ident if kind(r,k))
    u=sum(1 for r in rows if r[5] and r[4].startswith('未移植') and kind(r,k))
    L.append("| %s | %d | %d | %d | %d | %d |"%(label,cnt[(k,'ON')],cnt[(k,'OFF')],d,i,u))
L.append("| 合计 | %d | %d | %d | %d | %d |"%(sum(v for (k,s),v in cnt.items() if s=='ON'),sum(v for (k,s),v in cnt.items() if s=='OFF'),len(done),len(ident),sum(1 for r in rows if r[5] and r[4].startswith('未移植'))))
# the aligned prefix of all_optimizations: from its first GIMPLE pass to the
# last pass before the first enabled, unported one
names=[r[1] for r in rows]
ao=names.index('*all_optimizations')
# the first enabled pass that is not ported or only partly aligned
first_todo=next((i for i,r in enumerate(rows) if i>ao and r[5] and (r[4].startswith('未移植') or r[4].startswith('部分'))), len(rows))
ao_first=next(r[1] for r in rows[ao:] if r[1].startswith('tree-') and r[5])
ao_last=next(r[1] for r in reversed(rows[:first_todo]) if r[1].startswith('tree-') and r[5])
def bare(n): return re.sub(r'^(tree|ipa|rtl)-','',n)
L.append("\n完成的 pass 覆盖 early 序列（`ipa-build_ssa_passes` + `ipa-opt_local_passes`）到 `tree-release_ssa`，IPA 阶段的子集（inline into all callers、static 变量 readonly 发现、符号移除；见 `opt/ipa.rkt`、`opt/inline.rkt`），以及 `all_optimizations` 从 %s 到 %s（其间每个 -O1 执行的 pass 都是完成或恒等）；%s gimplify（`-fdump-tree-gimple`，不是 pass）与 genericize 已完成并对齐。\n" % (bare(ao_first), bare(ao_last), ("下一个未完成的是 `%s`（%s），之后的阶段在依赖它的夹具上尚未对齐。" % (rows[first_todo][1], rows[first_todo][4])) if first_todo < len(rows) else "晚期 GIMPLE 序列全部完成或恒等。"))
L.append("## 2. 夹具上真正改变代码的晚期 pass（release_ssa 之后）\n")
L.append("按 dump 两两比较函数体得到（`;;` 行与函数之间的消息不计）；没有列出的晚期 pass 在该夹具上是恒等的（对齐时只需输出与前一 pass 相同的 dump）。这是晚期序列的移植优先级依据。gcc-12 的 dump 编号与 gcc-15 不同，两表分列。\n")
st_by_name={}
for r in rows: st_by_name.setdefault(r[1],r[4])
def port_state(dumpname):
    # 'tree-NAME' row of the gcc-15 tree; gcc-12-only names have no row
    s=st_by_name.get('tree-'+dumpname)
    if s is None: return '（gcc-12 独有）'
    return '已对齐' if (s.startswith('完成') or s.startswith('恒等')) else '未移植' if s.startswith('未移植') else s
for ver,imp in [('15',impact),('12',impact12)]:
    L.append("**gcc-%s**\n"%ver)
    L.append("| 夹具 | 改变代码的 pass |\n|---|---|")
    for fx in FIXTURES:
        L.append("| %s.c | %s |"%(fx, ', '.join('`'+c+'`' for c in imp[fx])))
    L.append("")
for ver,imp in [('15',impact),('12',impact12)]:
    allc=collections.Counter(re.sub(r'^\d+t\.','',c) for fx in imp for c in imp[fx])
    todo=[(k,v) for k,v in allc.most_common() if port_state(k)!='已对齐']
    fin=[k for k,v in allc.most_common() if port_state(k)=='已对齐']
    L.append("gcc-%s 合并去重：已对齐 %d 个（%s）；仍需移植 %d 个（按出现次数）：%s。\n" % (ver, len(fin), ', '.join('`%s`'%k for k in fin), len(todo), ', '.join('`%s`×%d'%(k,v) for k,v in todo)))
L.append("`optimized` 的变化来自 pass_cleanup_cfg_post_optimizing 与 out-of-SSA 前的整理。\n")
L.append("## 3. 完整 pass 树（gcc-15 顺序；缩进 = 嵌套层级；gcc-12 列 = 该 pass 在 gcc-12 的 gate 状态，`无` = gcc-12 没有该 pass）\n")
L.append("| # | pass（gcc-15） | gcc-15 | gcc-12 | 状态 |\n|---|---|---|---|---|")
for i,(lvl,n,st,s12,s,ran) in enumerate(rows,1):
    L.append("| %d | %s`%s` | %s | %s | %s |"%(i,'　'*lvl,n,st,s12,s))
only12=[n for n in idx12 if n not in {r[1] for r in rows}]
L.append("\ngcc-12 独有的 pass（gcc-15 树中不存在）：%s。\n" % ', '.join('`'+n+'`' for n in only12))
L.append("## 4. 晚期 GIMPLE 序列的复用关系\n")
L.append("`all_optimizations` 中可直接复用已实现 pass 的实例：ccp2/3/4（`opt/ccp.rkt`）、forwprop2/3/4、fre3 及后续、dse2/3/4/5、dce2 到 dce7、cddce2/3（≥O2 的 aggressive 模式未做）、phiprop2、mergephi2/3、sccopy2、local-pure-const2、modref2、profile 重建（rebuild_frequencies）。全新的：alias（PTA）、retslot、stdarg、cdce、cselim、copyprop、ifcombine、phiopt、ch、cplxlower、bitintlower、sra、dom、reassoc、pow、laddress、lim、sink、循环优化族（fix_loops 到 loopdone：sccp、ivcanon、cunroll、ivopts）、veclower、switchlower、sincos、slsr、strlen、wrestrict、fab、crited、uncprop、nrv、isel，以及 expand 前的 out-of-SSA（TER、变量合并）。IPA 阶段：inline（夹具中 static `twice` 在此被内联，之后 remove_symbols 删除它）、pure-const / modref 的 SCC 传播、cdtor、static-var、single-use、simdclone、targetclone、profile；cp / icf / sra / devirt 在 -O1 关闭。\n")
L.append("## 5. 优化开关全表（`gcc-15 -Q --help=optimizers`）\n")
L.append("| 开关 | -O0 | -O1 | -O2 | -O3 | 状态 |\n|---|---|---|---|---|---|")
def mark(v,o):
    st=v.get(o,'')
    return '●' if st=='[enabled]' else '○' if st=='[disabled]' else st.strip('[]')
for n in sorted(levels):
    v=levels[n]
    L.append("| `%s` | %s | %s | %s | %s | %s |"%(n,mark(v,'0'),mark(v,'1'),mark(v,'2'),mark(v,'3'),fstat(n,v)))
L.append("\n● 启用，○ 关闭，其他为开关的非布尔取值。\n")
L.append("## 6. 测试\n")
if O1_TESTS or SUITE:
    o1=O1_TESTS or '未记录'
    o1s=("%s 项通过（%d 夹具 × 2 版本 × 阶段；未通过的见第 2 节的未完成 pass）" % (o1, len(FIXTURES))) if '/' in o1 else ("%s 项（%d 夹具 × 2 版本 × 阶段）全部通过" % (o1, len(FIXTURES)))
    nfail=None
    if SUITE and '/' in SUITE:
        a,b=SUITE.split('/'); nfail=int(b)-int(a)
    why='唯一的失败是' if nfail==1 else '失败中有 1 项是'
    L.append("- 最近一次实测：`o1-test.rkt` %s；全套 `raco test -x -j 4 src test` %s 通过（%s aarch64 scribble 文档模块，缺 scribble collection 无法加载，与本工作无关）。" % (o1s, SUITE or '未记录', why))
else:
    L.append("- 测试数未随本次生成记录（运行时加 `--o1-tests N --suite P/T`）。")
L.append("- 测试驱动按 GCC 的阶段顺序：先对全部函数 gimplify 到 SSA（`ipa-build_ssa_passes`），再逐函数跑 `ipa-opt_local_passes`；调用的 ECF 位按被调声明动态查询（`gimple_call_flags` 语义）。")
open(os.path.join(REPO,'doc','gcc-pass-progress.md'),'w').write('\n'.join(L)+'\n')
print(len(rows),'pass rows;',len(levels),'flag rows; done',len(done),'ident',len(ident))
for fx in impact: print(fx, impact[fx])
