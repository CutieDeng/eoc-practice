#lang racket/base

;; ============================================================
;; CFG Contracts - 类型契约定义
;; ============================================================
;;
;; 为 CFG 模块提供运行时类型检查和契约保证
;; 增强代码健壮性，提供更好的错误信息
;;
;; 使用方式：
;;   (require "compiler/ir/cfg/contracts.rkt")
;;   ; 使用带契约的导出函数
;; ============================================================

(require racket/contract)
(require "types.rkt")
(require "raw.rkt")

;; ============================================================
;; 基础类型契约
;; ============================================================

;; ID 类型谓词
(define block-id/c (struct/c BlockId exact-nonnegative-integer?))
(define var-id/c (struct/c VarId exact-nonnegative-integer?))
(define insn-id/c (struct/c InsnId exact-nonnegative-integer?))
(define insn-idx/c (struct/c InsnIdx exact-nonnegative-integer?))

(provide block-id/c var-id/c insn-id/c insn-idx/c)

;; ============================================================
;; 终止器契约
;; ============================================================

(define terminator/c
  (or/c TermJump?
        TermBranch?
        TermSwitch?
        TermReturn?
        TermThrow?
        TermUnreachable?))

(provide terminator/c)

;; ============================================================
;; 指令契约
;; ============================================================

;; 值流指令契约
(define vf-insn/c
  (struct/c VfInsn
    symbol?                    ; op
    list?                      ; inputs
    (listof VarId?)           ; outputs
    any/c                      ; info
    (or/c InsnId? #f)))       ; id

;; PHI 指令契约
(define phi-insn/c
  (struct/c PhiInsn
    VarId?                     ; output
    (listof (cons/c BlockId? VarId?))))  ; sources

(provide vf-insn/c phi-insn/c)

;; ============================================================
;; 基本块契约
;; ============================================================

(define cfg-block/c
  (struct/c CfgBlock
    BlockId?                   ; id
    (listof PhiInsn?)         ; phis
    (listof VfInsn?)          ; insns
    terminator/c))            ; terminator

(provide cfg-block/c)

;; ============================================================
;; CFG 契约
;; ============================================================

;; 完整 CFG 契约
(define cfg/c
  (and/c Cfg?
         (lambda (cfg)
           (and (exact-nonnegative-integer? (Cfg-block-cnt cfg))
                (exact-nonnegative-integer? (Cfg-var-cnt cfg))
                (exact-nonnegative-integer? (Cfg-insn-cnt cfg))
                (or (not (Cfg-entry cfg)) (BlockId? (Cfg-entry cfg)))
                (or (not (Cfg-exit cfg)) (BlockId? (Cfg-exit cfg)))))))

(provide cfg/c)

;; ============================================================
;; 带契约的函数导出
;; ============================================================

;; CFG 构造
(provide/contract
  [cfg-empty (-> cfg/c)]
  [cfg-create-block (-> cfg/c (values block-id/c cfg/c))]
  [cfg-create-block-with-terminator (-> cfg/c terminator/c (values block-id/c cfg/c))])

;; ID 分配
(provide/contract
  [cfg-alloc-block-id (-> cfg/c (values block-id/c cfg/c))]
  [cfg-alloc-block-ids (-> cfg/c exact-nonnegative-integer? (values block-id/c cfg/c))]
  [cfg-alloc-var-id (-> cfg/c (values var-id/c cfg/c))]
  [cfg-alloc-var-ids (-> cfg/c exact-nonnegative-integer? (values var-id/c cfg/c))]
  [cfg-alloc-insn-id (-> cfg/c (values insn-id/c cfg/c))]
  [cfg-alloc-insn-ids (-> cfg/c exact-nonnegative-integer? (values insn-id/c cfg/c))])

;; 块操作
(provide/contract
  [cfg-get-block (-> cfg/c block-id/c (or/c cfg-block/c #f))]
  [cfg-set-block (-> cfg/c cfg-block/c cfg/c)]
  [cfg-remove-block (-> cfg/c block-id/c cfg/c)]
  [cfg-has-block? (-> cfg/c block-id/c boolean?)])

;; 入口/出口
(provide/contract
  [cfg-set-entry (-> cfg/c block-id/c cfg/c)]
  [cfg-get-entry (-> cfg/c (or/c block-id/c #f))]
  [cfg-set-exit (-> cfg/c block-id/c cfg/c)]
  [cfg-get-exit (-> cfg/c (or/c block-id/c #f))])

;; 信息操作
(provide/contract
  [cfg-get-info (-> cfg/c symbol? any/c)]
  [cfg-set-info (-> cfg/c symbol? any/c cfg/c)]
  [cfg-remove-info (-> cfg/c symbol? cfg/c)]
  [cfg-update-info (-> cfg/c symbol? (-> any/c any/c) any/c cfg/c)])

;; 块内容操作
(provide/contract
  [cfg-block-append-insn (-> cfg/c block-id/c vf-insn/c cfg/c)]
  [cfg-block-set-terminator (-> cfg/c block-id/c terminator/c cfg/c)]
  [cfg-block-add-phi (-> cfg/c block-id/c phi-insn/c cfg/c)])

;; 遍历
(provide/contract
  [cfg-all-block-ids (-> cfg/c (listof block-id/c))]
  [cfg-block-count (-> cfg/c exact-nonnegative-integer?)])

;; 终止器分析
(provide/contract
  [terminator-successors (-> terminator/c (listof block-id/c))]
  [terminator-uses (-> terminator/c (listof var-id/c))])

;; ID 比较
(provide/contract
  [block-id-compare (-> block-id/c block-id/c (or/c -1 0 1))]
  [var-id-compare (-> var-id/c var-id/c (or/c -1 0 1))]
  [insn-id-compare (-> insn-id/c insn-id/c (or/c -1 0 1))])

;; ID 偏移
(provide/contract
  [block-id-offset (-> block-id/c exact-integer? block-id/c)]
  [var-id-offset (-> var-id/c exact-integer? var-id/c)]
  [insn-id-offset (-> insn-id/c exact-integer? insn-id/c)])

;; ============================================================
;; 不变量检查
;; ============================================================

;; 检查 CFG 是否满足基本不变量
(define (cfg-valid? cfg)
  (and
    ;; 有入口块
    (Cfg-entry cfg)
    ;; 入口块存在
    (cfg-has-block? cfg (Cfg-entry cfg))
    ;; 所有块的终止器目标都存在
    (for/and ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (and block
           (for/and ([succ (terminator-successors (CfgBlock-terminator block))])
             (cfg-has-block? cfg succ))))
    ;; 出口块如果存在则有效
    (or (not (Cfg-exit cfg))
        (cfg-has-block? cfg (Cfg-exit cfg)))))

;; 检查 CFG 是否为 SSA 形式
(define (cfg-ssa? cfg)
  ;; 每个变量只定义一次
  (define defs (make-hash))
  (for*/and ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (and block
         ;; PHI 输出
         (for/and ([phi (CfgBlock-phis block)])
           (define out (PhiInsn-output phi))
           (if (hash-has-key? defs out)
               #f
               (begin (hash-set! defs out #t) #t)))
         ;; 指令输出
         (for/and ([insn (CfgBlock-insns block)])
           (for/and ([out (VfInsn-outputs insn)])
             (if (hash-has-key? defs out)
                 #f
                 (begin (hash-set! defs out #t) #t)))))))

(provide cfg-valid? cfg-ssa?)

;; ============================================================
;; 调试辅助
;; ============================================================

;; 带验证的 CFG 打印
(define (cfg-debug-print cfg [port (current-output-port)])
  (fprintf port "CFG (~a blocks, entry=~a, exit=~a)\n"
           (cfg-block-count cfg)
           (Cfg-entry cfg)
           (Cfg-exit cfg))
  (fprintf port "Valid: ~a, SSA: ~a\n"
           (cfg-valid? cfg)
           (cfg-ssa? cfg))
  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (fprintf port "  Block ~a:\n" (BlockId-id bid))
      (for ([phi (CfgBlock-phis block)])
        (fprintf port "    PHI ~a <- ~a\n"
                 (PhiInsn-output phi)
                 (PhiInsn-sources phi)))
      (for ([insn (CfgBlock-insns block)])
        (fprintf port "    ~a ~a -> ~a\n"
                 (VfInsn-op insn)
                 (VfInsn-inputs insn)
                 (VfInsn-outputs insn)))
      (fprintf port "    ~a\n" (CfgBlock-terminator block)))))

(provide cfg-debug-print)
