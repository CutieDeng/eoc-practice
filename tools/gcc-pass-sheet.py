#!/usr/bin/env python3
"""Regenerate doc/gcc-pass-progress.md from GCC itself.

Inputs produced on the fly (into a temp dir):
  gcc-15 / gcc-12 -O1 -fdump-passes      -> the complete pass tree with ON/OFF gates
  gcc-15 -O{0,1,2,3} -Q --help=optimizers -> every optimizer flag and its state per level
  gcc-15 -O1 -fdump-tree-all on the fixtures -> which late passes change each fixture
The port's status per pass / flag is the table at the top of this file.

usage: python3 tools/gcc-pass-sheet.py [--gcc15 gcc-15] [--gcc12 /path/gcc-12]
"""
import re, collections, os, subprocess, sys, tempfile, glob
REPO=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
GCC15='gcc-15'; GCC12='/opt/homebrew/Cellar/gcc@12/12.5.0/bin/gcc-12'
args=sys.argv[1:]
for i,a in enumerate(args):
    if a=='--gcc15': GCC15=args[i+1]
    if a=='--gcc12': GCC12=args[i+1]
S=tempfile.mkdtemp(prefix='gcc-pass-sheet-')+'/'
FIX=os.path.join(REPO,'gccast-plugin','tests','struct.c')
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
os.makedirs(S+'dumps15',exist_ok=True)
for fx in ['basic','calls','struct','ccp']:
    subprocess.run([GCC15,'-O1','-c','-fdump-tree-all','-o','/dev/null',os.path.join(REPO,'gccast-plugin','tests',fx+'.c')],cwd=S+'dumps15',capture_output=True)
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
}
reuse={'ccp','forwprop','fre','dse','dce','cddce','phiprop','mergephi','sccopy','local-pure-const','modref','profile_estimate','fixup_cfg','strip_predict_hints','release_ssa','einline'}
def our_status(n,st,seen):
    if n in status and seen[n]==0: return status[n]
    if st=='OFF': return 'gate 关闭（-O1 不执行）'
    base=re.sub(r'\d+$','',n.split('-',1)[1] if '-' in n else n.lstrip('*'))
    if n.startswith('rtl-'): return '范围外（RTL 后端）'
    if n.startswith('ipa-'): return '范围外（IPA）'
    if n.startswith('*'): return '范围外 / 不比较'
    if base in reuse: return '未移植（可复用已实现的 %s）'%base
    return '未移植'
rows=[]; seen=collections.Counter()
for lvl,n,st in p15:
    s12=idx12[n][seen[n]] if seen[n] < len(idx12[n]) else '无'
    rows.append((lvl,n,st,s12,our_status(n,st,seen))); seen[n]+=1
levels={}
for o in '0123':
    for l in open(S+'flags-O%s.txt'%o):
        if ' ' not in l.strip(): continue
        n,st=l.split(None,1); st=st.strip()
        if n.startswith('-O'): continue
        levels.setdefault(n,{})[o]=st
flag_status={
 '-ftree-ccp':'完成（ccp1）','-ftree-bit-ccp':'完成（ccp1）','-ftree-forwprop':'完成（forwprop1）','-ftree-fre':'完成（fre1）','-ftree-dse':'完成（dse1）',
 '-ftree-dce':'完成（cddce1；dce2+ 未）','-ftree-builtin-call-dce':'未移植（cdce）','-fguess-branch-probability':'完成（profile_estimate）',
 '-fipa-pure-const':'完成（local 实例）；IPA 传播未','-fipa-modref':'完成（local 实例）；IPA 传播未','-fipa-strict-aliasing':'-O1 无 strict aliasing，alias set 恒 0',
 '-ftree-phiprop':'恒等','-fthread-jumps':'恒等（threader 未移植）','-ftree-sra':'恒等（esra）/ 未移植（sra）','-ftree-pta':'恒等 / 未移植（PTA）','-fssa-phiopt':'恒等 / 未移植',
 '-fearly-inlining':'恒等（不内联）','-finline':'未移植','-finline-functions-called-once':'未移植','-finline-atomics':'未移植','-fno-inline-stringops':'未移植',
 '-fipa-profile':'范围外','-fipa-reference':'范围外','-fipa-reference-addressable':'范围外','-fipa-stack-alignment':'范围外',
 '-ftree-copy-prop':'未移植','-ftree-dominator-opts':'未移植','-ftree-reassoc':'未移植','-ftree-ch':'未移植','-ftree-loop-optimize':'未移植','-ftree-loop-im':'未移植',
 '-ftree-loop-ivcanon':'未移植','-ftree-scev-cprop':'未移植','-fivopts':'未移植','-faggressive-loop-optimizations':'未移植','-fsplit-ivs-in-unroller':'未移植',
 '-fmove-loop-invariants':'未移植','-fmove-loop-stores':'未移植','-ftree-sink':'未移植','-ftree-slsr':'未移植','-fssa-backprop':'未移植','-fstdarg-opt':'未移植',
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
impact={}
for fx in ['basic','calls','struct','ccp']:
    files=sorted([f for f in os.listdir(S+'dumps15') if re.match(fx+r'\.c\.\d+t\.',f)], key=lambda f:int(f.split('.')[2][:-1]))
    prev=None; ch=[]
    for f in files:
        n=int(f.split('.')[2][:-1])
        if n<57: continue
        b=body(S+'dumps15/'+f)
        if prev is not None and b!=prev: ch.append(f.split('.',2)[2])
        prev=b
    impact[fx]=[c for c in ch if not c.endswith('statistics')]
cnt=collections.Counter()
for lvl,n,st,s12,s in rows:
    k='tree' if n.startswith('tree-') else 'ipa' if n.startswith('ipa-') else 'rtl' if n.startswith('rtl-') else 'nodump'
    cnt[(k,st)]+=1
done=[r for r in rows if r[4].startswith('完成')]; ident=[r for r in rows if r[4].startswith('恒等')]
def kind(r,k): return r[1].startswith(k+'-') if k!='nodump' else r[1].startswith('*')
L=[]
L.append("# GCC -O1 流水线对照表：移植进展（完整清单）\n")
L.append("更新：2026-11-08。\n")
L.append("## 0. 数据来源与口径\n")
L.append("- **pass 清单**：`gcc-15 -O1 -fdump-passes` 与 `gcc-12 -O1 -fdump-passes`（Homebrew gcc 15.2.0 / gcc@12 12.5.0）打印的完整 pass 树，含 gate 关闭的 pass。这是 GCC 对 `passes.def` 的实例化结果。此前版本的表只列了夹具实际产生 dump 文件的 pass，漏掉了无 dump 名的 `*` pass 与 gate 关闭的 pass。")
L.append("- **passes.def 规模**（`gcc.git` 的 releases/gcc-15 与 releases/gcc-12 分支）：NEXT_PASS / INSERT_PASS 条目 426（gcc-15）、406（gcc-12）；不同 pass 名 294（gcc-15）。-fdump-passes 展开为 %d 个实例（gcc-15）、%d 个（gcc-12）。" % (len(rows), len(p12)))
L.append("- **优化开关**：`gcc-15 -O{0,1,2,3} -Q --help=optimizers`：%d 个开关；-O0 启用 54，-O1 新增 43（共 96），-O2 再增 46（共 142），-O3 再增 14（共 156），135 个在 -O0 到 -O3 下都不默认启用。另有 316 个 `--param`，不在本表。" % len(levels))
L.append("- **对齐判据**：`src/component/gimple/o1-test.rkt`，4 夹具（basic.c、calls.c、struct.c、ccp.c）× 2 版本逐函数比较归一化 dump。状态：**完成** = 已移植且对齐；**恒等** = 未移植算法、夹具上 GCC 不改变函数、dump 对齐（换夹具可能触发差异）；**未移植**；**不比较** = 无函数 dump 或夹具上无效果；**范围外** = IPA 跨函数 / RTL 后端（后端为自有 aarch64）；**gate 关闭** = -O1 下 GCC 不执行。\n")
L.append("## 1. 统计（gcc-15 pass 树实例，-O1）\n")
L.append("| 类别 | ON | OFF | 完成 | 恒等 | 未移植（ON） |\n|---|---|---|---|---|---|")
for k,label in [('tree','GIMPLE（tree-）'),('ipa','IPA（ipa-）'),('rtl','RTL（rtl-）'),('nodump','无 dump 名（*）')]:
    d=sum(1 for r in done if kind(r,k)); i=sum(1 for r in ident if kind(r,k))
    u=sum(1 for r in rows if r[2]=='ON' and r[4].startswith('未移植') and kind(r,k))
    L.append("| %s | %d | %d | %d | %d | %d |"%(label,cnt[(k,'ON')],cnt[(k,'OFF')],d,i,u))
L.append("| 合计 | %d | %d | %d | %d | %d |"%(sum(v for (k,s),v in cnt.items() if s=='ON'),sum(v for (k,s),v in cnt.items() if s=='OFF'),len(done),len(ident),sum(1 for r in rows if r[2]=='ON' and r[4].startswith('未移植'))))
L.append("\n完成的 pass 覆盖 early 序列（`ipa-build_ssa_passes` + `ipa-opt_local_passes`）到 `tree-release_ssa`，IPA 阶段的子集（inline into all callers、static 变量 readonly 发现、符号移除；见 `opt/ipa.rkt`、`opt/inline.rkt`），以及 `all_optimizations` 从 fixup_cfg3 到 dce2；之后的晚期序列（含循环优化、向量化降级、out-of-SSA）未移植。gimplify（`-fdump-tree-gimple`，不是 pass）与 genericize 已完成并对齐。\n")
L.append("## 2. 夹具上真正改变代码的晚期 pass（gcc-15，release_ssa 之后）\n")
L.append("按 dump 两两比较函数体得到；没有列出的晚期 pass 在该夹具上是恒等的（对齐时只需输出与前一 pass 相同的 dump）。这是晚期序列的移植优先级依据。\n")
L.append("| 夹具 | 改变代码的 pass |\n|---|---|")
for fx in ['basic','calls','struct','ccp']:
    L.append("| %s.c | %s |"%(fx, ', '.join('`'+c+'`' for c in impact[fx])))
allc=collections.Counter(re.sub(r'^\d+t\.','',c) for fx in impact for c in impact[fx])
L.append("\n合并去重后需要移植的晚期 pass（按出现次数）：%s。`optimized` 的变化来自 pass_cleanup_cfg_post_optimizing 与 out-of-SSA 前的整理。\n" % ', '.join('`%s`×%d'%(k,v) for k,v in allc.most_common()))
L.append("## 3. 完整 pass 树（gcc-15 顺序；缩进 = 嵌套层级；gcc-12 列 = 该 pass 在 gcc-12 的 gate 状态，`无` = gcc-12 没有该 pass）\n")
L.append("| # | pass（gcc-15） | gcc-15 | gcc-12 | 状态 |\n|---|---|---|---|---|")
for i,(lvl,n,st,s12,s) in enumerate(rows,1):
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
L.append("- `o1-test.rkt` 276 项（4 夹具 × 2 版本 × 阶段），全套通过（aarch64 scribble 文档模块因缺 scribble collection 加载失败，与本工作无关）。")
L.append("- 测试驱动按 GCC 的阶段顺序：先对全部函数 gimplify 到 SSA（`ipa-build_ssa_passes`），再逐函数跑 `ipa-opt_local_passes`；调用的 ECF 位按被调声明动态查询（`gimple_call_flags` 语义）。")
open(os.path.join(REPO,'doc','gcc-pass-progress.md'),'w').write('\n'.join(L)+'\n')
print(len(rows),'pass rows;',len(levels),'flag rows; done',len(done),'ident',len(ident))
for fx in impact: print(fx, impact[fx])
