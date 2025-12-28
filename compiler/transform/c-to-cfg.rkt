#lang racket/base

;; ============================================================
;; Transform: C IR → CFG
;; ============================================================
;;
;; 将旧的 CProgram (基于 ral 的控制流图) 转换为新的 Cfg 结构
;; 使转换后的 CFG 可以复用 JVM 优化管道
;; ============================================================

(require racket/match racket/list racket/dict)
(require "../ftree.rkt")
(require "../core/cfg.rkt")
(require "../core/core-types.rkt")
(require "../core/p-types.rkt")
(require "../cfg/raw.rkt")

;; ============================================================
;; 主入口
;; ============================================================

;; 将 CProgram 转换为 Cfg
(define (c-program->cfg cprog)
  (match cprog
    [(CProgram info blocks)
     (define cfg0 (cfg-empty))

     ;; 第一遍：为每个旧 block ID 分配新的 BlockId
     (define-values (id-mapping cfg1)
       (for/fold ([mapping (ordl-make-empty integer-compare)]
                  [cfg cfg0])
                 ([(old-id _) (in-dict blocks)])
         (define-values (new-id cfg^) (cfg-alloc-block-id cfg))
         (values (dict-set mapping old-id new-id) cfg^)))

     ;; 第二遍：转换每个块
     (define cfg2
       (for/fold ([cfg cfg1])
                 ([(old-id stmts) (in-dict blocks)])
         (define new-id (dict-ref id-mapping old-id))
         (convert-block cfg new-id stmts id-mapping)))

     ;; 设置入口块 (block 2 是标准入口)
     (define entry-id (dict-ref id-mapping 2 (BlockId 0)))
     (cfg-set-entry cfg2 entry-id)]))

(provide c-program->cfg)

;; ============================================================
;; 块转换
;; ============================================================

;; 将一个块的语句列表转换为 CfgBlock
(define (convert-block cfg block-id stmts id-mapping)
  ;; 分离普通指令和终结器
  (define-values (insns terminator cfg^)
    (convert-statements cfg stmts id-mapping))

  ;; 创建并设置块
  (define block (CfgBlock block-id '() insns terminator))
  (cfg-set-block cfg^ block))

;; 转换语句列表，返回 (values insns terminator cfg)
(define (convert-statements cfg stmts id-mapping)
  (define stmt-list (ral->list stmts))

  (let loop ([stmts stmt-list]
             [insns '()]
             [cfg cfg])
    (match stmts
      ['()
       ;; 没有显式终结器，添加 unreachable
       (values (reverse insns) (TermUnreachable) cfg)]

      [(list (Goto label))
       ;; 无条件跳转
       (define target (dict-ref id-mapping label (BlockId label)))
       (values (reverse insns) (TermJump target) cfg)]

      [(list (Return arg))
       ;; 返回语句
       (define-values (ret-var insn cfg^) (convert-return-arg cfg arg))
       (define insns^ (if insn (cons insn insns) insns))
       (values (reverse insns^) (TermReturn (if ret-var (list ret-var) '())) cfg^)]

      [(list (IfStmt cnd thn els))
       ;; 条件分支
       (define-values (cond-var cond-insns cfg^) (convert-condition cfg cnd))
       (define then-target (extract-goto-target thn id-mapping))
       (define else-target (extract-goto-target els id-mapping))
       (define all-insns (append (reverse insns) cond-insns))
       (values all-insns (TermBranch cond-var then-target else-target) cfg^)]

      [(cons stmt rest)
       ;; 普通语句
       (define-values (new-insns cfg^) (convert-statement cfg stmt))
       (loop rest (append (reverse new-insns) insns) cfg^)])))

;; ============================================================
;; 语句转换
;; ============================================================

;; 转换单个语句，返回 (values insns cfg)
(define (convert-statement cfg stmt)
  (match stmt
    [(Assign (Var var-id) rhs)
     (convert-assign cfg var-id rhs)]

    [(Collect size)
     ;; 垃圾回收触发
     (define insn (VfInsn 'gc-collect (list size) '() #f #f))
     (values (list insn) cfg)]

    [(Prim 'vector-set! (list vec (Int idx) val))
     ;; 向量写入
     (define-values (vec-var vec-insns cfg1) (convert-atom cfg vec))
     (define-values (val-var val-insns cfg2) (convert-atom cfg1 val))
     (define insn (VfInsn 'vector-set! (list vec-var idx val-var) '() #f #f))
     (values (append vec-insns val-insns (list insn)) cfg2)]

    [_
     ;; 未知语句，忽略
     (values '() cfg)]))

;; 转换赋值语句
(define (convert-assign cfg var-id rhs)
  (define output (VarId var-id))

  (match rhs
    ;; 整数常量
    [(Int n)
     (define insn (VfInsn 'const (list n) (list output) #f #f))
     (values (list insn) cfg)]

    ;; 布尔常量
    [(Bool b)
     (define insn (VfInsn 'const (list b) (list output) #f #f))
     (values (list insn) cfg)]

    ;; 空值
    [(Void)
     (define insn (VfInsn 'const (list 'void) (list output) #f #f))
     (values (list insn) cfg)]

    ;; 变量引用
    [(Var src-id)
     (define insn (VfInsn 'copy (list (VarId src-id)) (list output) #f #f))
     (values (list insn) cfg)]

    ;; 原语操作
    [(Prim op args)
     (define-values (arg-vars arg-insns cfg^) (convert-atoms cfg args))
     (define insn (VfInsn (prim-op->cfg-op op) arg-vars (list output) #f #f))
     (values (append arg-insns (list insn)) cfg^)]

    ;; 全局值
    [(GlobalValue name)
     (define insn (VfInsn 'global-ref (list name) (list output) #f #f))
     (values (list insn) cfg)]

    ;; 内存分配
    [(Allocate amount type)
     (define insn (VfInsn 'allocate (list amount type) (list output) #f #f))
     (values (list insn) cfg)]

    ;; 函数引用
    [(FunRef name arity)
     (define insn (VfInsn 'fun-ref (list name arity) (list output) #f #f))
     (values (list insn) cfg)]

    ;; 函数调用
    [(Call func args)
     (define-values (func-var func-insns cfg1) (convert-atom cfg func))
     (define-values (arg-vars arg-insns cfg2) (convert-atoms cfg1 args))
     (define insn (VfInsn 'call (cons func-var arg-vars) (list output) #f #f))
     (values (append func-insns arg-insns (list insn)) cfg2)]

    [_
     ;; 其他情况，生成占位指令
     (define insn (VfInsn 'unknown (list rhs) (list output) #f #f))
     (values (list insn) cfg)]))

;; ============================================================
;; 原子表达式转换
;; ============================================================

;; 转换单个原子表达式，返回 (values var-id insns cfg)
(define (convert-atom cfg atom)
  (match atom
    [(Int n)
     (define-values (vid cfg^) (cfg-alloc-var-id cfg))
     (define insn (VfInsn 'const (list n) (list vid) #f #f))
     (values vid (list insn) cfg^)]

    [(Bool b)
     (define-values (vid cfg^) (cfg-alloc-var-id cfg))
     (define insn (VfInsn 'const (list b) (list vid) #f #f))
     (values vid (list insn) cfg^)]

    [(Var id)
     (values (VarId id) '() cfg)]

    [(GlobalValue name)
     (define-values (vid cfg^) (cfg-alloc-var-id cfg))
     (define insn (VfInsn 'global-ref (list name) (list vid) #f #f))
     (values vid (list insn) cfg^)]

    [_
     ;; 未知原子，创建占位
     (define-values (vid cfg^) (cfg-alloc-var-id cfg))
     (define insn (VfInsn 'unknown-atom (list atom) (list vid) #f #f))
     (values vid (list insn) cfg^)]))

;; 转换多个原子表达式
(define (convert-atoms cfg atoms)
  (for/fold ([vars '()]
             [insns '()]
             [cfg cfg])
            ([atom atoms])
    (define-values (var new-insns cfg^) (convert-atom cfg atom))
    (values (append vars (list var))
            (append insns new-insns)
            cfg^)))

;; ============================================================
;; 条件表达式转换
;; ============================================================

;; 转换条件表达式，返回 (values cond-var insns cfg)
(define (convert-condition cfg cnd)
  (match cnd
    [(Prim op (list a b))
     #:when (memq op '(eq? < > <= >= and or))
     (define-values (a-var a-insns cfg1) (convert-atom cfg a))
     (define-values (b-var b-insns cfg2) (convert-atom cfg1 b))
     (define-values (cond-vid cfg3) (cfg-alloc-var-id cfg2))
     (define cmp-insn (VfInsn (prim-op->cfg-op op)
                               (list a-var b-var)
                               (list cond-vid) #f #f))
     (values cond-vid (append a-insns b-insns (list cmp-insn)) cfg3)]

    [(Prim 'not (list e))
     (define-values (e-var e-insns cfg1) (convert-condition cfg e))
     (define-values (not-vid cfg2) (cfg-alloc-var-id cfg1))
     (define not-insn (VfInsn 'not (list e-var) (list not-vid) #f #f))
     (values not-vid (append e-insns (list not-insn)) cfg2)]

    [(Var id)
     (values (VarId id) '() cfg)]

    [(Bool #t)
     (define-values (vid cfg^) (cfg-alloc-var-id cfg))
     (define insn (VfInsn 'const (list #t) (list vid) #f #f))
     (values vid (list insn) cfg^)]

    [(Bool #f)
     (define-values (vid cfg^) (cfg-alloc-var-id cfg))
     (define insn (VfInsn 'const (list #f) (list vid) #f #f))
     (values vid (list insn) cfg^)]

    [_
     ;; 未知条件
     (define-values (vid cfg^) (cfg-alloc-var-id cfg))
     (define insn (VfInsn 'unknown-cond (list cnd) (list vid) #f #f))
     (values vid (list insn) cfg^)]))

;; ============================================================
;; 辅助函数
;; ============================================================

;; 从 Goto 或 ral 中提取目标块
(define (extract-goto-target target id-mapping)
  (match target
    [(Goto label)
     (dict-ref id-mapping label (BlockId label))]
    [(? ral?)
     (match (ral->list target)
       [(list (Goto label)) (dict-ref id-mapping label (BlockId label))]
       [_ (BlockId 0)])]
    [(? integer?)
     (dict-ref id-mapping target (BlockId target))]
    [_ (BlockId 0)]))

;; 转换返回参数
(define (convert-return-arg cfg arg)
  (match arg
    [(Void) (values #f #f cfg)]
    [(Var id) (values (VarId id) #f cfg)]
    [(Int n)
     (define-values (vid cfg^) (cfg-alloc-var-id cfg))
     (define insn (VfInsn 'const (list n) (list vid) #f #f))
     (values vid insn cfg^)]
    [(Bool b)
     (define-values (vid cfg^) (cfg-alloc-var-id cfg))
     (define insn (VfInsn 'const (list b) (list vid) #f #f))
     (values vid insn cfg^)]
    [_
     (define-values (vid cfg^) (cfg-alloc-var-id cfg))
     (define insn (VfInsn 'unknown (list arg) (list vid) #f #f))
     (values vid insn cfg^)]))

;; 将 Prim 操作符映射到 CFG 操作符
(define (prim-op->cfg-op op)
  (case op
    [(+) 'add]
    [(-) 'sub]
    [(*) 'mul]
    [(/) 'div]
    [(remainder) 'rem]
    [(eq?) 'eq]
    [(<) 'lt]
    [(>) 'gt]
    [(<=) 'le]
    [(>=) 'ge]
    [(and) 'and]
    [(or) 'or]
    [(not) 'not]
    [(vector-ref) 'vector-ref]
    [(vector-set!) 'vector-set!]
    [(vector-length) 'vector-length]
    [(read) 'read]
    [(print) 'print]
    [else op]))

;; 将 ral 转换为 list (如果是 ral)
(define (ral->list x)
  (if (ral? x)
      (for/list ([elem (in-ral0 x)]) elem)
      (if (list? x) x (list x))))
