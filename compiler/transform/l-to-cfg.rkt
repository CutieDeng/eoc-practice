#lang racket/base

;; ============================================================
;; Transform: L IR → CFG
;; ============================================================
;;
;; 直接将 L-language (高级函数式表达式) 转换为 CFG
;; 跳过 C IR 中间步骤，生成优化友好的值流形式
;; ============================================================

(require racket/match racket/list racket/dict)
(require "../ftree.rkt")
(require "../core/cfg.rkt")
(require "../core/p-types.rkt")
(require "../cfg/raw.rkt")

;; ============================================================
;; 编译上下文
;; ============================================================

;; 编译状态：包含 CFG、当前块、变量环境
(struct CompileCtx (
  cfg           ; Cfg - 正在构建的 CFG
  current-block ; BlockId - 当前块
  env           ; ordl: Symbol -> VarId - 符号变量环境
  id-env        ; ordl: Integer -> VarId - ID 变量映射
) #:transparent)

(define (ctx-cfg ctx) (CompileCtx-cfg ctx))
(define (ctx-block ctx) (CompileCtx-current-block ctx))
(define (ctx-env ctx) (CompileCtx-env ctx))
(define (ctx-id-env ctx) (CompileCtx-id-env ctx))

(define (ctx-set-cfg ctx cfg)
  (struct-copy CompileCtx ctx [cfg cfg]))

(define (ctx-set-block ctx block-id)
  (struct-copy CompileCtx ctx [current-block block-id]))

;; 绑定符号名到 VarId
(define (ctx-bind ctx name var-id)
  (struct-copy CompileCtx ctx
    [env (dict-set (ctx-env ctx) name var-id)]))

;; 绑定整数 ID 到 VarId
(define (ctx-bind-id ctx id var-id)
  (struct-copy CompileCtx ctx
    [id-env (dict-set (ctx-id-env ctx) id var-id)]))

;; 查找符号名
(define (ctx-lookup ctx name)
  (dict-ref (ctx-env ctx) name #f))

;; 查找整数 ID
(define (ctx-lookup-id ctx id)
  (dict-ref (ctx-id-env ctx) id #f))

;; ============================================================
;; 主入口
;; ============================================================

;; 将 Program 转换为 Cfg
(define (l-program->cfg prog)
  (match prog
    [(Program info body)
     (define cfg0 (cfg-empty))

     ;; 创建入口块
     (define-values (entry-id cfg1) (cfg-create-block cfg0))
     (define cfg2 (cfg-set-entry cfg1 entry-id))

     ;; 初始化上下文
     (define ctx0 (CompileCtx cfg2 entry-id
                              (ordl-make-empty symbol-compare)
                              (ordl-make-empty integer-compare)))

     ;; 编译表达式并生成返回
     (define-values (result-var ctx1) (compile-expr ctx0 body))

     ;; 添加返回终结器
     (define cfg3 (ctx-cfg ctx1))
     (define final-block (ctx-block ctx1))
     (define cfg4 (cfg-block-set-terminator cfg3 final-block
                    (TermReturn (if result-var (list result-var) '()))))

     cfg4]))

(provide l-program->cfg)

;; ============================================================
;; 表达式编译
;; ============================================================

;; 编译表达式，返回 (values result-var ctx)
;; result-var 可能是 #f (对于 void 表达式)
(define (compile-expr ctx expr)
  (match expr
    ;; === 字面量 ===
    [(Int n)
     (emit-const ctx n)]

    [(Bool b)
     (emit-const ctx b)]

    [(Void)
     (values #f ctx)]

    ;; === 变量引用 ===
    [(Var id)
     ;; 整数 ID (来自编号后的程序)
     ;; 首先查找 ID 映射，如果没有才直接使用 VarId
     (define mapped-var (ctx-lookup-id ctx id))
     (values (or mapped-var (VarId id)) ctx)]

    [(Var:r name)
     ;; 符号名 (编号前的程序)
     (define var-id (ctx-lookup ctx name))
     (if var-id
         (values var-id ctx)
         (error 'compile-expr "Unbound variable: ~a" name))]

    ;; === Let 绑定 ===
    ;; 注意：不直接使用 Let 的 ID，因为中间临时变量可能已占用
    ;; 使用 rhs-var 作为绑定变量，在环境中建立映射
    [(Let x rhs body)
     (define-values (rhs-var ctx1) (compile-expr ctx rhs))
     ;; 如果 rhs 是 void，分配新变量
     (define-values (bound-var ctx2)
       (if rhs-var
           (values rhs-var ctx1)
           (let-values ([(vid cfg^) (cfg-alloc-var-id (ctx-cfg ctx1))])
             (values vid (ctx-set-cfg ctx1 cfg^)))))
     ;; 在环境中建立映射（用于符号名）或 ID 映射
     (define ctx3
       (cond
         [(symbol? x) (ctx-bind ctx2 x bound-var)]
         [(integer? x) (ctx-bind-id ctx2 x bound-var)]
         [else ctx2]))
     (compile-expr ctx3 body)]

    ;; === If 条件 ===
    [(If cnd thn els)
     (compile-if ctx cnd thn els)]

    ;; === While 循环 ===
    [(WhileLoop cnd body)
     (compile-while ctx cnd body)]

    ;; === Begin 序列 ===
    [(Begin es body)
     (define ctx1
       (for/fold ([ctx ctx])
                 ([e (in-ral0 es)])
         (define-values (_var ctx^) (compile-expr ctx e))
         ctx^))
     (compile-expr ctx1 body)]

    ;; === SetBang 赋值 ===
    [(SetBang var rhs)
     (define-values (rhs-var ctx1) (compile-expr ctx rhs))
     (define target-var
       (cond
         [(integer? var) (VarId var)]
         [(symbol? var) (ctx-lookup ctx1 var)]
         [else (error 'compile-expr "Invalid set! target: ~a" var)]))
     (when (not target-var)
       (error 'compile-expr "Unbound variable in set!: ~a" var))
     (define ctx2 (emit-copy ctx1 rhs-var target-var))
     (values #f ctx2)]

    ;; === GetBang 读取 ===
    [(GetBang var)
     (define var-id
       (cond
         [(integer? var) (VarId var)]
         [(symbol? var) (ctx-lookup ctx var)]
         [else (error 'compile-expr "Invalid get! target: ~a" var)]))
     (values var-id ctx)]

    ;; === 原语操作 ===
    [(Prim op args)
     (compile-prim ctx op args)]

    ;; === 全局值 ===
    [(GlobalValue name)
     (define-values (vid cfg^) (cfg-alloc-var-id (ctx-cfg ctx)))
     (define insn (VfInsn 'global-ref (list name) (list vid) #f #f))
     (define cfg^^ (cfg-block-append-insn cfg^ (ctx-block ctx) insn))
     (values vid (ctx-set-cfg ctx cfg^^))]

    ;; === 内存分配 ===
    [(Allocate amount type)
     (define-values (vid cfg^) (cfg-alloc-var-id (ctx-cfg ctx)))
     (define insn (VfInsn 'allocate (list amount type) (list vid) #f #f))
     (define cfg^^ (cfg-block-append-insn cfg^ (ctx-block ctx) insn))
     (values vid (ctx-set-cfg ctx cfg^^))]

    ;; === 函数引用 ===
    [(FunRef name arity)
     (define-values (vid cfg^) (cfg-alloc-var-id (ctx-cfg ctx)))
     (define insn (VfInsn 'fun-ref (list name arity) (list vid) #f #f))
     (define cfg^^ (cfg-block-append-insn cfg^ (ctx-block ctx) insn))
     (values vid (ctx-set-cfg ctx cfg^^))]

    ;; === 函数调用 ===
    [(Apply func arg-list)
     (define-values (func-var ctx1) (compile-expr ctx func))
     ;; arg-list 可能是 ral 或 list
     (define args (if (ral? arg-list) (ral->list arg-list) arg-list))
     (define-values (arg-vars ctx2)
       (for/fold ([vars '()] [ctx ctx1])
                 ([arg args])
         (define-values (var ctx^) (compile-expr ctx arg))
         (values (append vars (list var)) ctx^)))
     (define-values (vid cfg^) (cfg-alloc-var-id (ctx-cfg ctx2)))
     (define insn (VfInsn 'call (cons func-var arg-vars) (list vid) #f #f))
     (define cfg^^ (cfg-block-append-insn cfg^ (ctx-block ctx2) insn))
     (values vid (ctx-set-cfg ctx2 cfg^^))]

    ;; === Call (直接调用) ===
    [(Call func args)
     (define-values (func-var ctx1) (compile-expr ctx func))
     (define-values (arg-vars ctx2)
       (for/fold ([vars '()] [ctx ctx1])
                 ([arg args])
         (define-values (var ctx^) (compile-expr ctx arg))
         (values (append vars (list var)) ctx^)))
     (define-values (vid cfg^) (cfg-alloc-var-id (ctx-cfg ctx2)))
     (define insn (VfInsn 'call (cons func-var arg-vars) (list vid) #f #f))
     (define cfg^^ (cfg-block-append-insn cfg^ (ctx-block ctx2) insn))
     (values vid (ctx-set-cfg ctx2 cfg^^))]

    ;; === 类型注解 (透传) ===
    [(HasType e _type)
     (compile-expr ctx e)]

    ;; === 垃圾回收 ===
    [(Collect size)
     (define insn (VfInsn 'gc-collect (list size) '() #f #f))
     (define cfg^ (cfg-block-append-insn (ctx-cfg ctx) (ctx-block ctx) insn))
     (values #f (ctx-set-cfg ctx cfg^))]

    [_
     (error 'compile-expr "Unknown expression: ~a" expr)]))

;; ============================================================
;; If 编译
;; ============================================================

(define (compile-if ctx cnd thn els)
  (define cfg0 (ctx-cfg ctx))
  (define current-block (ctx-block ctx))

  ;; 编译条件
  (define-values (cond-var ctx1) (compile-condition ctx cnd))

  ;; 创建 then、else、merge 块
  (define-values (then-id cfg1^) (cfg-create-block (ctx-cfg ctx1)))
  (define-values (else-id cfg2) (cfg-create-block cfg1^))
  (define-values (merge-id cfg3) (cfg-create-block cfg2))

  ;; 设置条件分支
  (define cfg4 (cfg-block-set-terminator cfg3 (ctx-block ctx1)
                 (TermBranch cond-var then-id else-id)))

  ;; 编译 then 分支
  (define ctx-then (ctx-set-cfg (ctx-set-block ctx1 then-id) cfg4))
  (define-values (then-var ctx-then^) (compile-expr ctx-then thn))
  (define cfg5 (cfg-block-set-terminator (ctx-cfg ctx-then^) then-id
                 (TermJump merge-id)))

  ;; 编译 else 分支
  (define ctx-else (ctx-set-cfg (ctx-set-block ctx1 else-id) cfg5))
  (define-values (else-var ctx-else^) (compile-expr ctx-else els))
  (define cfg6 (cfg-block-set-terminator (ctx-cfg ctx-else^) else-id
                 (TermJump merge-id)))

  ;; 如果需要，在 merge 块添加 PHI 节点
  (define-values (result-var cfg7)
    (if (and then-var else-var)
        (let-values ([(vid cfg^) (cfg-alloc-var-id cfg6)])
          (define phi (PhiInsn vid (list (cons then-id then-var)
                                          (cons else-id else-var))))
          (values vid (cfg-block-add-phi cfg^ merge-id phi)))
        (values (or then-var else-var) cfg6)))

  (values result-var (ctx-set-cfg (ctx-set-block ctx1 merge-id) cfg7)))

;; ============================================================
;; While 编译
;; ============================================================

(define (compile-while ctx cnd body)
  (define cfg0 (ctx-cfg ctx))
  (define current-block (ctx-block ctx))

  ;; 创建 header、body、exit 块
  (define-values (header-id cfg1) (cfg-create-block cfg0))
  (define-values (body-id cfg2) (cfg-create-block cfg1))
  (define-values (exit-id cfg3) (cfg-create-block cfg2))

  ;; 从当前块跳转到 header
  (define cfg4 (cfg-block-set-terminator cfg3 current-block
                 (TermJump header-id)))

  ;; 在 header 编译条件
  (define ctx-header (ctx-set-cfg (ctx-set-block ctx header-id) cfg4))
  (define-values (cond-var ctx-header^) (compile-condition ctx-header cnd))

  ;; 设置条件分支
  (define cfg5 (cfg-block-set-terminator (ctx-cfg ctx-header^) header-id
                 (TermBranch cond-var body-id exit-id)))

  ;; 编译循环体
  (define ctx-body (ctx-set-cfg (ctx-set-block ctx-header^ body-id) cfg5))
  (define-values (_body-var ctx-body^) (compile-expr ctx-body body))

  ;; 跳回 header
  (define cfg6 (cfg-block-set-terminator (ctx-cfg ctx-body^) body-id
                 (TermJump header-id)))

  ;; 返回 void，继续在 exit 块
  (values #f (ctx-set-cfg (ctx-set-block ctx exit-id) cfg6)))

;; ============================================================
;; 条件编译
;; ============================================================

(define (compile-condition ctx cnd)
  (match cnd
    [(Prim op (list a b))
     #:when (memq op '(eq? < > <= >=))
     (define-values (a-var ctx1) (compile-expr ctx a))
     (define-values (b-var ctx2) (compile-expr ctx1 b))
     (define-values (vid cfg^) (cfg-alloc-var-id (ctx-cfg ctx2)))
     (define insn (VfInsn (prim-op->cfg-op op) (list a-var b-var) (list vid) #f #f))
     (define cfg^^ (cfg-block-append-insn cfg^ (ctx-block ctx2) insn))
     (values vid (ctx-set-cfg ctx2 cfg^^))]

    [(Prim 'not (list e))
     (define-values (e-var ctx1) (compile-condition ctx e))
     (define-values (vid cfg^) (cfg-alloc-var-id (ctx-cfg ctx1)))
     (define insn (VfInsn 'not (list e-var) (list vid) #f #f))
     (define cfg^^ (cfg-block-append-insn cfg^ (ctx-block ctx1) insn))
     (values vid (ctx-set-cfg ctx1 cfg^^))]

    [(Bool b)
     (emit-const ctx b)]

    [(Var id)
     ;; 首先查找 ID 映射
     (define mapped-var (ctx-lookup-id ctx id))
     (values (or mapped-var (VarId id)) ctx)]

    [(Var:r name)
     (define var-id (ctx-lookup ctx name))
     (if var-id
         (values var-id ctx)
         (error 'compile-condition "Unbound variable: ~a" name))]

    [(GetBang var)
     (define var-id
       (cond
         [(integer? var) (VarId var)]
         [(symbol? var) (ctx-lookup ctx var)]
         [else (error 'compile-condition "Invalid get! target: ~a" var)]))
     (values var-id ctx)]

    [_
     ;; 其他表达式：编译后与 true 比较
     (define-values (val-var ctx1) (compile-expr ctx cnd))
     (if val-var
         (let ()
           (define-values (true-var ctx2) (emit-const ctx1 #t))
           (define-values (vid cfg^) (cfg-alloc-var-id (ctx-cfg ctx2)))
           (define insn (VfInsn 'eq (list val-var true-var) (list vid) #f #f))
           (define cfg^^ (cfg-block-append-insn cfg^ (ctx-block ctx2) insn))
           (values vid (ctx-set-cfg ctx2 cfg^^)))
         ;; void 值作为 false
         (emit-const ctx1 #f))]))

;; ============================================================
;; 原语编译
;; ============================================================

(define (compile-prim ctx op args)
  ;; 编译所有参数
  (define-values (arg-vars ctx1)
    (for/fold ([vars '()] [ctx ctx])
              ([arg args])
      (define-values (var ctx^) (compile-expr ctx arg))
      (values (append vars (list var)) ctx^)))

  ;; 生成操作指令
  (define cfg-op (prim-op->cfg-op op))

  (cond
    ;; 无返回值操作
    [(memq op '(vector-set! print))
     (define insn (VfInsn cfg-op arg-vars '() #f #f))
     (define cfg^ (cfg-block-append-insn (ctx-cfg ctx1) (ctx-block ctx1) insn))
     (values #f (ctx-set-cfg ctx1 cfg^))]

    ;; 有返回值操作
    [else
     (define-values (vid cfg^) (cfg-alloc-var-id (ctx-cfg ctx1)))
     (define insn (VfInsn cfg-op arg-vars (list vid) #f #f))
     (define cfg^^ (cfg-block-append-insn cfg^ (ctx-block ctx1) insn))
     (values vid (ctx-set-cfg ctx1 cfg^^))]))

;; ============================================================
;; 辅助函数
;; ============================================================

;; 发射常量
(define (emit-const ctx value)
  (define-values (vid cfg^) (cfg-alloc-var-id (ctx-cfg ctx)))
  (define insn (VfInsn 'const (list value) (list vid) #f #f))
  (define cfg^^ (cfg-block-append-insn cfg^ (ctx-block ctx) insn))
  (values vid (ctx-set-cfg ctx cfg^^)))

;; 发射复制
(define (emit-copy ctx src-var dst-var)
  (define insn (VfInsn 'copy (list src-var) (list dst-var) #f #f))
  (define cfg^ (cfg-block-append-insn (ctx-cfg ctx) (ctx-block ctx) insn))
  (ctx-set-cfg ctx cfg^))

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
    [(make-vector) 'make-vector]
    [(read) 'read]
    [(print) 'print]
    [else op]))

;; 将 ral 转换为 list
(define (ral->list x)
  (if (ral? x)
      (for/list ([elem (in-ral0 x)]) elem)
      (if (list? x) x (list x))))
