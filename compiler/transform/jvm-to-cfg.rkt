#lang racket/base

;; ============================================================
;; Transform: JVM → CFG
;; ============================================================
;;
;; 将 JVM 字节码转换为 CFG
;; 包含栈模拟，生成值流形式的 CFG
;; ============================================================

(require racket/match racket/list racket/dict racket/set)
(require cutie-ftree)
(require "../core/jvm.rkt")
(require "../core/cfg.rkt")
(require "../cfg/raw.rkt")

;; === 主入口 ===

;; 将 JvmMethod 转换为 CFG
(define (jvm-method->cfg method)
  (define insns (JvmMethod-insns method))

  ;; 第一遍：识别基本块边界，建立标签映射
  (define-values (label->block-id block-boundaries cfg0)
    (identify-blocks insns))

  ;; 第二遍：填充每个块的指令，进行栈模拟
  (define cfg1
    (populate-blocks insns label->block-id block-boundaries cfg0))

  ;; 设置入口
  (define entry-block
    (if (null? (dict-keys label->block-id))
        (BlockId 0)
        (first-block-id block-boundaries)))

  (cfg-set-entry cfg1 entry-block))

(provide jvm-method->cfg)

;; === 第一遍：识别基本块 ===

(define (identify-blocks insns)
  (define cfg0 (cfg-empty))

  ;; 收集所有标签和跳转目标
  (define labels (collect-labels insns))
  (define jump-targets (collect-jump-targets insns))

  ;; 块边界 = 标签位置 ∪ 跳转后位置
  (define boundaries (set-union labels jump-targets))

  ;; 为每个标签分配 BlockId
  (define-values (label->block-id cfg1)
    (for/fold ([mapping (ordl-make-empty string-compare)]
               [cfg cfg0])
              ([label (in-set labels)])
      (define-values (bid cfg^) (cfg-alloc-block-id cfg))
      (values (dict-set mapping label bid) cfg^)))

  ;; 为非标签边界也分配块（跳转后的下一条指令）
  ;; 这些通过指令索引标识
  (define-values (idx->block-id cfg2)
    (for/fold ([mapping (ordl-make-empty integer-compare)]
               [cfg cfg1])
              ([idx (in-set jump-targets)]
               #:unless (label-at-index? insns idx))
      (define-values (bid cfg^) (cfg-alloc-block-id cfg))
      (values (dict-set mapping idx bid) cfg^)))

  (values label->block-id boundaries cfg2))

;; 收集所有标签
(define (collect-labels insns)
  (for/fold ([labels (set)])
            ([insn insns])
    (if (and (JvmInsn? insn)
             (eq? (JvmInsn-opcode insn) 'CUTIEDENG-LABEL))
        (match (JvmInsn-operands insn)
          [`(,label) (set-add labels label)]
          [_ labels])
        labels)))

;; 收集所有跳转目标
(define (collect-jump-targets insns)
  (for/fold ([targets (set)])
            ([insn insns]
             [idx (in-naturals)])
    (if (JvmInsn? insn)
        (match (JvmInsn-opcode insn)
          ;; 条件跳转后面还会继续执行
          [(or 'IFEQ 'IFNE 'IFLT 'IFGE 'IFGT 'IFLE
               'IF_ICMPEQ 'IF_ICMPNE 'IF_ICMPLT 'IF_ICMPGE 'IF_ICMPGT 'IF_ICMPLE
               'IF_ACMPEQ 'IF_ACMPNE 'IFNULL 'IFNONNULL)
           (set-add targets (+ idx 1))]
          ;; 无条件跳转后是新块边界（如果有后续指令）
          [(or 'GOTO 'GOTO_W)
           (if (< (+ idx 1) (length insns))
               (set-add targets (+ idx 1))
               targets)]
          ;; 返回/抛出后是新块边界
          [(or 'IRETURN 'LRETURN 'FRETURN 'DRETURN 'ARETURN 'RETURN 'ATHROW)
           (if (< (+ idx 1) (length insns))
               (set-add targets (+ idx 1))
               targets)]
          [_ targets])
        targets)))

;; 检查某索引处是否有标签
(define (label-at-index? insns idx)
  (and (< idx (length insns))
       (let ([insn (list-ref insns idx)])
         (and (JvmInsn? insn)
              (eq? (JvmInsn-opcode insn) 'CUTIEDENG-LABEL)))))

;; 获取第一个块的 ID
(define (first-block-id boundaries)
  (BlockId 0))

;; === 第二遍：填充块内容（含栈模拟）===

(define (populate-blocks insns label->block-id boundaries cfg)
  ;; 状态：当前块、栈状态、变量计数器
  (define-values (cfg^ _current-block _stack)
    (for/fold ([cfg cfg]
               [current-block #f]
               [stack '()])  ; 栈是 VarId 列表，栈顶在前
              ([insn insns]
               [idx (in-naturals)])
      (cond
        ;; 遇到标签：开始新块
        [(and (JvmInsn? insn)
              (eq? (JvmInsn-opcode insn) 'CUTIEDENG-LABEL))
         (match (JvmInsn-operands insn)
           [`(,label)
            (define new-block-id (dict-ref label->block-id label))
            ;; 如果之前有块且未终结，添加跳转
            (define cfg^
              (if (and current-block
                       (TermUnreachable? (CfgBlock-terminator
                                           (cfg-get-block cfg current-block))))
                  (cfg-block-set-terminator cfg current-block (TermJump new-block-id))
                  cfg))
            ;; 创建新块
            (define cfg^^
              (if (cfg-has-block? cfg^ new-block-id)
                  cfg^
                  (let-values ([(_ c) (cfg-create-block cfg^)])
                    ;; 实际上需要用已分配的 ID
                    (cfg-set-block c (CfgBlock new-block-id '() '() (TermUnreachable))))))
            (values cfg^^ new-block-id stack)])]

        ;; 普通指令：处理并更新栈
        [(JvmInsn? insn)
         (if current-block
             (process-insn cfg current-block stack insn label->block-id)
             ;; 没有当前块，创建一个
             (let-values ([(bid cfg^) (cfg-create-block cfg)])
               (process-insn cfg^ bid stack insn label->block-id)))]

        [else (values cfg current-block stack)])))

  cfg^)

;; === 指令处理（含栈模拟）===

(define (process-insn cfg block-id stack insn label->block-id)
  (define opcode (JvmInsn-opcode insn))
  (define operands (JvmInsn-operands insn))

  (match opcode
    ;; === 常量加载 ===
    ['ACONST_NULL
     (push-const cfg block-id stack 'null)]
    [(or 'ICONST_M1 'ICONST_0 'ICONST_1 'ICONST_2 'ICONST_3 'ICONST_4 'ICONST_5)
     (push-const cfg block-id stack (iconst->value opcode))]
    [(or 'LCONST_0 'LCONST_1)
     (push-const cfg block-id stack (lconst->value opcode))]
    [(or 'FCONST_0 'FCONST_1 'FCONST_2)
     (push-const cfg block-id stack (fconst->value opcode))]
    [(or 'DCONST_0 'DCONST_1)
     (push-const cfg block-id stack (dconst->value opcode))]
    ['BIPUSH
     (push-const cfg block-id stack (first operands))]
    ['SIPUSH
     (push-const cfg block-id stack (first operands))]
    ['LDC
     (push-const cfg block-id stack (first operands))]

    ;; === 局部变量加载 ===
    [(or 'ILOAD 'LLOAD 'FLOAD 'DLOAD 'ALOAD)
     (load-local cfg block-id stack (first operands))]
    [(or 'ILOAD_0 'ILOAD_1 'ILOAD_2 'ILOAD_3)
     (load-local cfg block-id stack (iload-n->index opcode))]
    [(or 'LLOAD_0 'LLOAD_1 'LLOAD_2 'LLOAD_3)
     (load-local cfg block-id stack (lload-n->index opcode))]
    [(or 'ALOAD_0 'ALOAD_1 'ALOAD_2 'ALOAD_3)
     (load-local cfg block-id stack (aload-n->index opcode))]

    ;; === 局部变量存储 ===
    [(or 'ISTORE 'LSTORE 'FSTORE 'DSTORE 'ASTORE)
     (store-local cfg block-id stack (first operands))]
    [(or 'ISTORE_0 'ISTORE_1 'ISTORE_2 'ISTORE_3)
     (store-local cfg block-id stack (istore-n->index opcode))]
    [(or 'ASTORE_0 'ASTORE_1 'ASTORE_2 'ASTORE_3)
     (store-local cfg block-id stack (astore-n->index opcode))]

    ;; === 算术运算 ===
    [(or 'IADD 'LADD 'FADD 'DADD)
     (binary-op cfg block-id stack 'add)]
    [(or 'ISUB 'LSUB 'FSUB 'DSUB)
     (binary-op cfg block-id stack 'sub)]
    [(or 'IMUL 'LMUL 'FMUL 'DMUL)
     (binary-op cfg block-id stack 'mul)]
    [(or 'IDIV 'LDIV 'FDIV 'DDIV)
     (binary-op cfg block-id stack 'div)]
    [(or 'IREM 'LREM 'FREM 'DREM)
     (binary-op cfg block-id stack 'rem)]
    [(or 'INEG 'LNEG 'FNEG 'DNEG)
     (unary-op cfg block-id stack 'neg)]

    ;; === 位运算 ===
    [(or 'ISHL 'LSHL) (binary-op cfg block-id stack 'shl)]
    [(or 'ISHR 'LSHR) (binary-op cfg block-id stack 'shr)]
    [(or 'IUSHR 'LUSHR) (binary-op cfg block-id stack 'ushr)]
    [(or 'IAND 'LAND) (binary-op cfg block-id stack 'and)]
    [(or 'IOR 'LOR) (binary-op cfg block-id stack 'or)]
    [(or 'IXOR 'LXOR) (binary-op cfg block-id stack 'xor)]

    ;; === 比较 ===
    ['LCMP (binary-op cfg block-id stack 'lcmp)]
    [(or 'FCMPL 'DCMPL) (binary-op cfg block-id stack 'cmpl)]
    [(or 'FCMPG 'DCMPG) (binary-op cfg block-id stack 'cmpg)]

    ;; === 类型转换 ===
    ['I2L (unary-op cfg block-id stack 'i2l)]
    ['I2F (unary-op cfg block-id stack 'i2f)]
    ['I2D (unary-op cfg block-id stack 'i2d)]
    ['L2I (unary-op cfg block-id stack 'l2i)]
    ['L2F (unary-op cfg block-id stack 'l2f)]
    ['L2D (unary-op cfg block-id stack 'l2d)]
    ['F2I (unary-op cfg block-id stack 'f2i)]
    ['F2L (unary-op cfg block-id stack 'f2l)]
    ['F2D (unary-op cfg block-id stack 'f2d)]
    ['D2I (unary-op cfg block-id stack 'd2i)]
    ['D2L (unary-op cfg block-id stack 'd2l)]
    ['D2F (unary-op cfg block-id stack 'd2f)]
    ['I2B (unary-op cfg block-id stack 'i2b)]
    ['I2C (unary-op cfg block-id stack 'i2c)]
    ['I2S (unary-op cfg block-id stack 'i2s)]

    ;; === 栈操作 ===
    ['POP
     (values cfg block-id (cdr stack))]
    ['POP2
     (values cfg block-id (cddr stack))]
    ['DUP
     (values cfg block-id (cons (car stack) stack))]
    ['DUP_X1
     (let ([v1 (car stack)] [v2 (cadr stack)])
       (values cfg block-id (list* v1 v2 v1 (cddr stack))))]
    ['DUP_X2
     (let ([v1 (car stack)] [v2 (cadr stack)] [v3 (caddr stack)])
       (values cfg block-id (list* v1 v2 v3 v1 (cdddr stack))))]
    ['DUP2
     (let ([v1 (car stack)] [v2 (cadr stack)])
       (values cfg block-id (list* v1 v2 v1 v2 (cddr stack))))]
    ['SWAP
     (let ([v1 (car stack)] [v2 (cadr stack)])
       (values cfg block-id (list* v2 v1 (cddr stack))))]

    ;; === 条件跳转 ===
    [(or 'IFEQ 'IFNE 'IFLT 'IFGE 'IFGT 'IFLE)
     (conditional-jump-unary cfg block-id stack opcode (first operands) label->block-id)]
    [(or 'IF_ICMPEQ 'IF_ICMPNE 'IF_ICMPLT 'IF_ICMPGE 'IF_ICMPGT 'IF_ICMPLE)
     (conditional-jump-binary cfg block-id stack opcode (first operands) label->block-id)]
    [(or 'IF_ACMPEQ 'IF_ACMPNE)
     (conditional-jump-binary cfg block-id stack opcode (first operands) label->block-id)]
    [(or 'IFNULL 'IFNONNULL)
     (conditional-jump-unary cfg block-id stack opcode (first operands) label->block-id)]

    ;; === 无条件跳转 ===
    [(or 'GOTO 'GOTO_W)
     (define target-label (first operands))
     (define target-block (dict-ref label->block-id target-label))
     (define cfg^ (cfg-block-set-terminator cfg block-id (TermJump target-block)))
     (values cfg^ block-id '())]  ; 跳转后栈状态重置

    ;; === 返回 ===
    [(or 'IRETURN 'LRETURN 'FRETURN 'DRETURN 'ARETURN)
     (define ret-val (car stack))
     (define cfg^ (cfg-block-set-terminator cfg block-id (TermReturn (list ret-val))))
     (values cfg^ block-id '())]
    ['RETURN
     (define cfg^ (cfg-block-set-terminator cfg block-id (TermReturn '())))
     (values cfg^ block-id '())]

    ;; === 方法调用 ===
    [(or 'INVOKEVIRTUAL 'INVOKESPECIAL 'INVOKESTATIC 'INVOKEINTERFACE)
     (invoke-method cfg block-id stack opcode operands)]

    ;; === 字段访问 ===
    [(or 'GETFIELD 'GETSTATIC)
     (get-field cfg block-id stack opcode operands)]
    [(or 'PUTFIELD 'PUTSTATIC)
     (put-field cfg block-id stack opcode operands)]

    ;; === 对象创建 ===
    ['NEW
     (define-values (vid cfg^) (cfg-alloc-var-id cfg))
     (define insn^ (VfInsn 'new (list (first operands)) (list vid) #f))
     (define cfg^^ (cfg-block-append-insn cfg^ block-id insn^))
     (values cfg^^ block-id (cons vid stack))]

    ;; === 数组操作 ===
    ['NEWARRAY
     (define-values (vid cfg^) (cfg-alloc-var-id cfg))
     (define size (car stack))
     (define insn^ (VfInsn 'newarray (list size (first operands)) (list vid) #f))
     (define cfg^^ (cfg-block-append-insn cfg^ block-id insn^))
     (values cfg^^ block-id (cons vid (cdr stack)))]
    ['ANEWARRAY
     (define-values (vid cfg^) (cfg-alloc-var-id cfg))
     (define size (car stack))
     (define insn^ (VfInsn 'anewarray (list size (first operands)) (list vid) #f))
     (define cfg^^ (cfg-block-append-insn cfg^ block-id insn^))
     (values cfg^^ block-id (cons vid (cdr stack)))]
    ['ARRAYLENGTH
     (define-values (vid cfg^) (cfg-alloc-var-id cfg))
     (define arr (car stack))
     (define insn^ (VfInsn 'arraylength (list arr) (list vid) #f))
     (define cfg^^ (cfg-block-append-insn cfg^ block-id insn^))
     (values cfg^^ block-id (cons vid (cdr stack)))]
    [(or 'IALOAD 'LALOAD 'FALOAD 'DALOAD 'AALOAD 'BALOAD 'CALOAD 'SALOAD)
     (array-load cfg block-id stack)]
    [(or 'IASTORE 'LASTORE 'FASTORE 'DASTORE 'AASTORE 'BASTORE 'CASTORE 'SASTORE)
     (array-store cfg block-id stack)]

    ;; === 异常 ===
    ['ATHROW
     (define exc (car stack))
     (define cfg^ (cfg-block-set-terminator cfg block-id (TermThrow exc)))
     (values cfg^ block-id '())]

    ;; === 其他（暂时忽略）===
    ['IINC
     ;; IINC 直接修改局部变量，需要特殊处理
     (values cfg block-id stack)]

    ['TRY-CATCH-BLOCK
     ;; 异常表条目，在构建时另外处理
     (values cfg block-id stack)]

    ['CHECKCAST
     ;; 类型检查不改变栈
     (values cfg block-id stack)]

    ['INSTANCEOF
     (define-values (vid cfg^) (cfg-alloc-var-id cfg))
     (define obj (car stack))
     (define insn^ (VfInsn 'instanceof (list obj (first operands)) (list vid) #f))
     (define cfg^^ (cfg-block-append-insn cfg^ block-id insn^))
     (values cfg^^ block-id (cons vid (cdr stack)))]

    ;; 默认：未处理的操作码
    [_
     (eprintf "Warning: unhandled opcode ~a~n" opcode)
     (values cfg block-id stack)]))

;; === 辅助函数 ===

(define (push-const cfg block-id stack value)
  (define-values (vid cfg^) (cfg-alloc-var-id cfg))
  (define insn (VfInsn 'const (list value) (list vid) #f))
  (define cfg^^ (cfg-block-append-insn cfg^ block-id insn))
  (values cfg^^ block-id (cons vid stack)))

(define (load-local cfg block-id stack index)
  (define-values (vid cfg^) (cfg-alloc-var-id cfg))
  (define insn (VfInsn 'load-local (list index) (list vid) #f))
  (define cfg^^ (cfg-block-append-insn cfg^ block-id insn))
  (values cfg^^ block-id (cons vid stack)))

(define (store-local cfg block-id stack index)
  (define val (car stack))
  (define insn (VfInsn 'store-local (list val index) '() #f))
  (define cfg^ (cfg-block-append-insn cfg block-id insn))
  (values cfg^ block-id (cdr stack)))

(define (binary-op cfg block-id stack op)
  (define-values (vid cfg^) (cfg-alloc-var-id cfg))
  (define v2 (car stack))
  (define v1 (cadr stack))
  (define insn (VfInsn op (list v1 v2) (list vid) #f))
  (define cfg^^ (cfg-block-append-insn cfg^ block-id insn))
  (values cfg^^ block-id (cons vid (cddr stack))))

(define (unary-op cfg block-id stack op)
  (define-values (vid cfg^) (cfg-alloc-var-id cfg))
  (define v (car stack))
  (define insn (VfInsn op (list v) (list vid) #f))
  (define cfg^^ (cfg-block-append-insn cfg^ block-id insn))
  (values cfg^^ block-id (cons vid (cdr stack))))

(define (conditional-jump-unary cfg block-id stack opcode target-label label->block-id)
  ;; 比较栈顶与 0
  (define-values (cmp-vid cfg^) (cfg-alloc-var-id cfg))
  (define val (car stack))
  (define cmp-op (case opcode
                   [(IFEQ IFNULL) 'eq0]
                   [(IFNE IFNONNULL) 'ne0]
                   [(IFLT) 'lt0]
                   [(IFGE) 'ge0]
                   [(IFGT) 'gt0]
                   [(IFLE) 'le0]))
  (define cmp-insn (VfInsn cmp-op (list val) (list cmp-vid) #f))
  (define cfg^^ (cfg-block-append-insn cfg^ block-id cmp-insn))

  ;; 创建分支终结器
  (define then-block (dict-ref label->block-id target-label))
  ;; else-block 将是下一个块（需要后续处理）
  ;; 暂时用占位符
  (define cfg^^^ (cfg-set-info cfg^^ 'pending-branch
                   (list block-id cmp-vid then-block)))
  (values cfg^^^ block-id (cdr stack)))

(define (conditional-jump-binary cfg block-id stack opcode target-label label->block-id)
  (define-values (cmp-vid cfg^) (cfg-alloc-var-id cfg))
  (define v2 (car stack))
  (define v1 (cadr stack))
  (define cmp-op (case opcode
                   [(IF_ICMPEQ IF_ACMPEQ) 'eq]
                   [(IF_ICMPNE IF_ACMPNE) 'ne]
                   [(IF_ICMPLT) 'lt]
                   [(IF_ICMPGE) 'ge]
                   [(IF_ICMPGT) 'gt]
                   [(IF_ICMPLE) 'le]))
  (define cmp-insn (VfInsn cmp-op (list v1 v2) (list cmp-vid) #f))
  (define cfg^^ (cfg-block-append-insn cfg^ block-id cmp-insn))

  (define then-block (dict-ref label->block-id target-label))
  (define cfg^^^ (cfg-set-info cfg^^ 'pending-branch
                   (list block-id cmp-vid then-block)))
  (values cfg^^^ block-id (cddr stack)))

(define (invoke-method cfg block-id stack opcode operands)
  (match operands
    [`(,owner ,name ,desc . ,_rest)
     ;; 解析描述符确定参数数量和返回类型
     (define param-count (count-method-params desc))
     (define has-return (method-has-return? desc))
     (define is-static (eq? opcode 'INVOKESTATIC))

     ;; 收集参数（包括 this 如果非静态）
     (define total-args (if is-static param-count (+ param-count 1)))
     (define args (take stack total-args))
     (define rest-stack (drop stack total-args))

     ;; 创建调用指令
     (if has-return
         (let-values ([(vid cfg^) (cfg-alloc-var-id cfg)])
           (define insn (VfInsn 'invoke
                          (list opcode owner name desc (reverse args))
                          (list vid) #f))
           (define cfg^^ (cfg-block-append-insn cfg^ block-id insn))
           (values cfg^^ block-id (cons vid rest-stack)))
         (let ()
           (define insn (VfInsn 'invoke
                          (list opcode owner name desc (reverse args))
                          '() #f))
           (define cfg^ (cfg-block-append-insn cfg block-id insn))
           (values cfg^ block-id rest-stack)))]))

(define (get-field cfg block-id stack opcode operands)
  (match operands
    [`(,owner ,name ,desc)
     (define-values (vid cfg^) (cfg-alloc-var-id cfg))
     (if (eq? opcode 'GETSTATIC)
         (let ()
           (define insn (VfInsn 'get-static (list owner name desc) (list vid) #f))
           (define cfg^^ (cfg-block-append-insn cfg^ block-id insn))
           (values cfg^^ block-id (cons vid stack)))
         (let ()
           (define obj (car stack))
           (define insn (VfInsn 'get-field (list obj owner name desc) (list vid) #f))
           (define cfg^^ (cfg-block-append-insn cfg^ block-id insn))
           (values cfg^^ block-id (cons vid (cdr stack)))))]))

(define (put-field cfg block-id stack opcode operands)
  (match operands
    [`(,owner ,name ,desc)
     (if (eq? opcode 'PUTSTATIC)
         (let ()
           (define val (car stack))
           (define insn (VfInsn 'put-static (list val owner name desc) '() #f))
           (define cfg^ (cfg-block-append-insn cfg block-id insn))
           (values cfg^ block-id (cdr stack)))
         (let ()
           (define val (car stack))
           (define obj (cadr stack))
           (define insn (VfInsn 'put-field (list obj val owner name desc) '() #f))
           (define cfg^ (cfg-block-append-insn cfg block-id insn))
           (values cfg^ block-id (cddr stack))))]))

(define (array-load cfg block-id stack)
  (define-values (vid cfg^) (cfg-alloc-var-id cfg))
  (define idx (car stack))
  (define arr (cadr stack))
  (define insn (VfInsn 'aload (list arr idx) (list vid) #f))
  (define cfg^^ (cfg-block-append-insn cfg^ block-id insn))
  (values cfg^^ block-id (cons vid (cddr stack))))

(define (array-store cfg block-id stack)
  (define val (car stack))
  (define idx (cadr stack))
  (define arr (caddr stack))
  (define insn (VfInsn 'astore (list arr idx val) '() #f))
  (define cfg^ (cfg-block-append-insn cfg block-id insn))
  (values cfg^ block-id (cdddr stack)))

;; === 操作码辅助 ===

(define (iconst->value opcode)
  (case opcode
    [(ICONST_M1) -1]
    [(ICONST_0) 0]
    [(ICONST_1) 1]
    [(ICONST_2) 2]
    [(ICONST_3) 3]
    [(ICONST_4) 4]
    [(ICONST_5) 5]))

(define (lconst->value opcode)
  (case opcode
    [(LCONST_0) 0]
    [(LCONST_1) 1]))

(define (fconst->value opcode)
  (case opcode
    [(FCONST_0) 0.0]
    [(FCONST_1) 1.0]
    [(FCONST_2) 2.0]))

(define (dconst->value opcode)
  (case opcode
    [(DCONST_0) 0.0]
    [(DCONST_1) 1.0]))

(define (iload-n->index opcode)
  (case opcode
    [(ILOAD_0) 0] [(ILOAD_1) 1] [(ILOAD_2) 2] [(ILOAD_3) 3]))

(define (lload-n->index opcode)
  (case opcode
    [(LLOAD_0) 0] [(LLOAD_1) 1] [(LLOAD_2) 2] [(LLOAD_3) 3]))

(define (aload-n->index opcode)
  (case opcode
    [(ALOAD_0) 0] [(ALOAD_1) 1] [(ALOAD_2) 2] [(ALOAD_3) 3]))

(define (istore-n->index opcode)
  (case opcode
    [(ISTORE_0) 0] [(ISTORE_1) 1] [(ISTORE_2) 2] [(ISTORE_3) 3]))

(define (astore-n->index opcode)
  (case opcode
    [(ASTORE_0) 0] [(ASTORE_1) 1] [(ASTORE_2) 2] [(ASTORE_3) 3]))

;; === 方法描述符解析 ===

(define (count-method-params desc)
  ;; 简单解析：计算 '(' 和 ')' 之间的参数
  (define param-part (car (regexp-match #rx"\\(([^)]*)\\)" desc)))
  (define params (substring param-part 1 (- (string-length param-part) 1)))
  (count-type-descriptors params))

(define (count-type-descriptors str)
  (if (string=? str "")
      0
      (let ([len (type-descriptor-length str)])
        (+ 1 (count-type-descriptors (substring str len))))))

(define (type-descriptor-length str)
  (case (string-ref str 0)
    [(#\B #\C #\D #\F #\I #\J #\S #\Z) 1]
    [(#\L) (+ 1 (string-length (car (regexp-match #rx"[^;]*;" str))))]
    [(#\[) (+ 1 (type-descriptor-length (substring str 1)))]
    [else 1]))

(define (method-has-return? desc)
  (define ret (cadr (regexp-match #rx"\\)(.+)$" desc)))
  (not (string=? ret "V")))
