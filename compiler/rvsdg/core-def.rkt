#lang racket/base

;; ============================================================
;; RVSDG Core Definitions
;; ============================================================
;;
;; 设计原则：
;; 1. Region 是核心容器，包含 info 字段用于挂载扩展数据
;; 2. 节点类型是纯结构，不继承 NodeInfo
;; 3. info 字段类型为 ordl (cutie-ftree 平衡树)
;;
;; info 字段推荐 key：
;;   'types        -> (ordl NodeId/WireId Type)  类型标注
;;   'effects      -> (ordl NodeId EffectRow)    效果标注
;;   'source-map   -> (ordl NodeId SrcLoc)       源码位置
;;   'names        -> (ordl NodeId/WireId Symbol) 调试名称
;;   'tensor-shapes -> (ordl WireId Shape)       ML编译器用
;; ============================================================

;; === 标识符类型 ===

(struct NodeId (id) #:transparent)
(struct InputId (id) #:transparent)
(struct OutputId (id) #:transparent)
(struct WireId (id) #:transparent)

;; === Region: 核心容器 ===

(struct Region (
  info            ; ordl: 扩展元信息 (key -> value)

  ;; 边的连接映射
  wire->input     ; ordl: WireId -> InputId
  wire->output    ; ordl: WireId -> OutputId

  ;; 端口的反向映射
  input->wire     ; ordl: InputId -> WireId
  input->node     ; ordl: InputId -> NodeId
  output->wire    ; ordl: OutputId -> WireId
  output->node    ; ordl: OutputId -> NodeId

  ;; 节点的端口映射
  node->input     ; ordl: NodeId -> (cons InputId cnt)
  node->output    ; ordl: NodeId -> (cons OutputId cnt)
  node->value     ; ordl: NodeId -> NodeValue

  ;; ID 分配计数器
  wire-cnt        ; Natural
  input-cnt       ; Natural
  output-cnt      ; Natural
  node-cnt        ; Natural
) #:transparent)

;; === 节点类型 ===

;; 简单节点：原始操作
(struct Simple (op) #:transparent)

;; 控制流节点
(struct Gamma (region*) #:transparent)    ; 条件分支，region* 是分支列表
(struct Theta (region) #:transparent)     ; 循环

;; 函数相关节点
(struct Lambda (region) #:transparent)    ; 函数定义
(struct Delta (region) #:transparent)     ; 全局/可变变量
(struct Phi (region*) #:transparent)      ; 相互递归函数组

;; 程序根节点
(struct Omega (region) #:transparent)     ; 程序入口

;; === 效果系统节点 ===

;; 限定续体 (delimited continuation)
(struct Psi (tag region) #:transparent)   ; prompt/reset 边界
(struct Shift (tag) #:transparent)        ; 续体捕获 (shift/control)

;; 代数效果 (algebraic effects)
(struct EffHandler (ops handler-region return-region) #:transparent)
(struct EffPerform (op-name) #:transparent)

;; === 异常处理节点（可选，可用效果系统替代）===

(struct Kappa (try-region handler*) #:transparent)  ; try-catch
(struct Throw (exn-tag) #:transparent)              ; throw

;; === 导出 ===

(provide (struct-out Region))

(provide (struct-out NodeId))
(provide (struct-out InputId))
(provide (struct-out OutputId))
(provide (struct-out WireId))

;; 基本节点
(provide (struct-out Simple))
(provide (struct-out Gamma))
(provide (struct-out Theta))
(provide (struct-out Lambda))
(provide (struct-out Delta))
(provide (struct-out Phi))
(provide (struct-out Omega))

;; 效果系统节点
(provide (struct-out Psi))
(provide (struct-out Shift))
(provide (struct-out EffHandler))
(provide (struct-out EffPerform))

;; 异常节点
(provide (struct-out Kappa))
(provide (struct-out Throw))
