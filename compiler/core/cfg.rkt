#lang racket/base

;; ============================================================
;; Core: CFG 数据结构定义
;; ============================================================
;;
;; 控制流图 (Control Flow Graph) 的核心数据结构
;; 所有 ID 使用从零开始的整数，元信息通过查表获取
;; ============================================================

;; === ID 类型定义 ===

;; 基本块 ID
(struct BlockId (id) #:prefab)

;; 变量 ID（值流形式中的变量）
(struct VarId (id) #:prefab)

;; 指令 ID（全局唯一，稳定标识符）
(struct InsnId (id) #:prefab)

;; 指令索引（块内，用于遍历，非稳定）
(struct InsnIdx (id) #:prefab)

(provide (struct-out BlockId))
(provide (struct-out VarId))
(provide (struct-out InsnId))
(provide (struct-out InsnIdx))

;; === ID 操作 ===

(define (block-id-offset base n)
  (BlockId (+ (BlockId-id base) n)))

(define (var-id-offset base n)
  (VarId (+ (VarId-id base) n)))

(define (insn-id-offset base n)
  (InsnId (+ (InsnId-id base) n)))

(provide block-id-offset var-id-offset insn-id-offset)

;; === CFG 结构 ===

;; 控制流图
(struct Cfg (
  block-cnt       ; 下一个可分配的 BlockId (整数)
  var-cnt         ; 下一个可分配的 VarId (整数)
  insn-cnt        ; 下一个可分配的 InsnId (整数)
  entry           ; BlockId - 入口块
  blocks          ; ordl: BlockId → CfgBlock
  info            ; ordl: symbol → any
) #:prefab)

(provide (struct-out Cfg))

;; 基本块
(struct CfgBlock (
  id              ; BlockId
  phis            ; (Listof PhiInsn) - SSA φ 节点（可为空）
  insns           ; (Listof VfInsn) - 值流指令
  terminator      ; Terminator - 终结指令
) #:prefab)

(provide (struct-out CfgBlock))

;; === 值流指令 ===

;; 值流指令（栈消除后的形式）
;; id 字段可选：
;;   - #f: 未分配 ID（轻量模式）
;;   - InsnId: 已分配稳定 ID（用于 SSA 分析）
(struct VfInsn (
  op              ; Symbol - 操作符
  inputs          ; (Listof Any) - 输入（可含 VarId 和其他数据）
  outputs         ; (Listof VarId) - 输出变量
  info            ; 附加信息（可选）
  id              ; InsnId or #f - 稳定标识符（可选）
) #:prefab)

(provide (struct-out VfInsn))

;; φ 节点（SSA 形式）
(struct PhiInsn (
  output          ; VarId - 输出变量
  sources         ; (Listof (Pairof BlockId VarId)) - 来源映射
) #:prefab)

(provide (struct-out PhiInsn))

;; === 终结指令 ===

;; 无条件跳转
(struct TermJump (target) #:prefab)  ; target: BlockId

;; 条件分支
(struct TermBranch (
  cond            ; VarId - 条件变量
  then-target     ; BlockId
  else-target     ; BlockId
) #:prefab)

;; Switch 分支
(struct TermSwitch (
  value           ; VarId - 判断值
  cases           ; (Listof (Pairof Integer BlockId)) - case 映射
  default         ; BlockId - 默认分支
) #:prefab)

;; 返回
(struct TermReturn (
  values          ; (Listof VarId) - 返回值
) #:prefab)

;; 抛出异常
(struct TermThrow (
  exception       ; VarId - 异常对象
) #:prefab)

;; 不可达（用于标记 unreachable 代码）
(struct TermUnreachable () #:prefab)

(provide (struct-out TermJump))
(provide (struct-out TermBranch))
(provide (struct-out TermSwitch))
(provide (struct-out TermReturn))
(provide (struct-out TermThrow))
(provide (struct-out TermUnreachable))

;; === 辅助谓词 ===

(define (terminator? x)
  (or (TermJump? x)
      (TermBranch? x)
      (TermSwitch? x)
      (TermReturn? x)
      (TermThrow? x)
      (TermUnreachable? x)))

(provide terminator?)

;; === info 键名约定 ===
;;
;; --- 基础映射 ---
;; 'insn->block   : ordl: InsnId → BlockId                ; 指令所属块（双向映射之一）
;; 'block->insns  : ordl: BlockId → (Listof InsnId)       ; 块内指令（双向映射之一）
;;
;; --- Use-Def 链（可选，按需构建）---
;; 'var->def      : ordl: VarId → InsnId                  ; 变量定义指令
;; 'var->uses     : ordl: VarId → (Listof (cons InsnId ArgIdx))  ; 变量使用位置
;;
;; --- 类型信息 ---
;; 'var->type     : ordl: VarId → Type                    ; 变量类型
;;
;; --- 控制流分析 ---
;; 'block->preds  : ordl: BlockId → (Listof BlockId)      ; 前驱块
;; 'block->succs  : ordl: BlockId → (Listof BlockId)      ; 后继块
;; 'block->dom    : ordl: BlockId → BlockId               ; 直接支配者
;; 'block->idom   : ordl: BlockId → (Listof BlockId)      ; 被支配者
;;
;; --- 调试信息 ---
;; 'source-map    : ordl: InsnId → SourceLoc              ; 源码位置映射
