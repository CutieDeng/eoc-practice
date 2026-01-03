#lang racket/base

;; ============================================================
;; CFG SSA Layer: Use-Def 链与 SSA 分析
;; ============================================================
;;
;; 提供 use-def 链的构建、查询和增量维护
;; 所有数据存储在 Cfg.info 中
;; ============================================================

(require racket/match racket/list racket/dict racket/set)
(require (except-in "../kernel/data/main.rkt" symbol-compare))
(require "../kernel/ir/cfg/types.rkt")
(require "../component/cfg/utils/graph-ops.rkt")

;; === 辅助函数：提取嵌套结构中的所有 VarId ===

;; 递归提取列表中的所有 VarId（处理嵌套情况）
(define (extract-var-ids datum)
  (cond
    [(VarId? datum) (list datum)]
    [(list? datum) (append-map extract-var-ids datum)]
    [else '()]))

;; === 位置类型 ===

;; 定义位置
(struct DefLoc (block-id insn-idx) #:prefab)
;; 参数定义（特殊位置）
(struct ParamDef (index) #:prefab)
;; 使用位置
(struct UseLoc (block-id insn-idx arg-idx) #:prefab)

(provide (struct-out DefLoc))
(provide (struct-out ParamDef))
(provide (struct-out UseLoc))

;; === 比较函数 ===

(define (defloc-compare a b)
  (cond
    [(and (DefLoc? a) (DefLoc? b))
     (define cmp1 (block-id-compare (DefLoc-block-id a) (DefLoc-block-id b)))
     (if (eq? cmp1 '=)
         (integer-compare (DefLoc-insn-idx a) (DefLoc-insn-idx b))
         cmp1)]
    [(and (ParamDef? a) (ParamDef? b))
     (integer-compare (ParamDef-index a) (ParamDef-index b))]
    [(ParamDef? a) '<]
    [(ParamDef? b) '>]
    [else '=]))

(define (useloc-compare a b)
  (define cmp1 (block-id-compare (UseLoc-block-id a) (UseLoc-block-id b)))
  (if (eq? cmp1 '=)
      (let ([cmp2 (integer-compare (UseLoc-insn-idx a) (UseLoc-insn-idx b))])
        (if (eq? cmp2 '=)
            (integer-compare (UseLoc-arg-idx a) (UseLoc-arg-idx b))
            cmp2))
      cmp1))

(provide defloc-compare useloc-compare)

;; === 构建 Use-Def 链 ===

;; 从 CFG 构建完整的 use-def 链
;; 返回更新后的 CFG（链存储在 info 中）
;; 同时构建 insn->block 和 block->insns 双向映射（如果指令有 InsnId）
(define (cfg-build-use-def cfg)
  (define var->def (ordered-map-empty var-id-compare))
  (define var->uses (ordered-map-empty var-id-compare))
  (define insn->block (ordered-map-empty insn-id-compare))
  (define block->insns (ordered-map-empty block-id-compare))

  ;; 遍历所有块和指令
  (define-values (v->d v->u i->b b->i)
    (for/fold ([v->d var->def]
               [v->u var->uses]
               [i->b insn->block]
               [b->i block->insns])
              ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (if (not block)
          (values v->d v->u i->b b->i)
          ;; 处理每条指令
          (for/fold ([v->d v->d]
                     [v->u v->u]
                     [i->b i->b]
                     [b->i b->i])
                    ([(insn idx) (in-indexed (CfgBlock-insns block))])
            (cond
              [(VfInsn? insn)
               (define dloc (DefLoc bid idx))
               (define insn-id (VfInsn-id insn))
               ;; 记录 defs
               (define v->d*
                 (for/fold ([acc v->d])
                           ([out (VfInsn-outputs insn)]
                            #:when (VarId? out))
                   (dict-set acc out dloc)))
               ;; 记录 uses（使用 extract-var-ids 处理嵌套）
               (define input-vars (extract-var-ids (VfInsn-inputs insn)))
               (define v->u*
                 (for/fold ([acc v->u])
                           ([(inp arg-idx) (in-indexed input-vars)])
                   (define uloc (UseLoc bid idx arg-idx))
                   (dict-update acc inp (λ (lst) (cons uloc lst)) '())))
               ;; 记录 insn->block 映射（如果有 InsnId）
               (define-values (i->b* b->i*)
                 (if insn-id
                     (values (dict-set i->b insn-id bid)
                             (dict-update b->i bid
                               (λ (lst) (append lst (list insn-id)))
                               '()))
                     (values i->b b->i)))
               (values v->d* v->u* i->b* b->i*)]
              [else (values v->d v->u i->b b->i)])))))

  ;; 处理 terminator 中的 uses
  (define v->u-final
    (for/fold ([v->u v->u])
              ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (if (not block)
          v->u
          (let ([term (CfgBlock-terminator block)])
            (define term-uses (terminator-uses term))
            (for/fold ([acc v->u])
                      ([(var idx) (in-indexed term-uses)]
                       #:when (VarId? var))
              ;; 使用 -1 作为 insn-idx 表示 terminator
              (define uloc (UseLoc bid -1 idx))
              (dict-update acc var (λ (lst) (cons uloc lst)) '()))))))

  ;; 存储到 info
  (define cfg* (cfg-set-info (cfg-set-info cfg 'var->def v->d) 'var->uses v->u-final))
  (define cfg** (cfg-set-info (cfg-set-info cfg* 'insn->block i->b) 'block->insns b->i))
  cfg**)

(provide cfg-build-use-def)

;; === 查询接口 ===

;; 获取变量的定义位置
(define (cfg-get-def cfg var-id)
  (define var->def (cfg-get-info cfg 'var->def))
  (and var->def (dict-ref var->def var-id #f)))

;; 获取变量的所有使用位置
(define (cfg-get-uses cfg var-id)
  (define var->uses (cfg-get-info cfg 'var->uses))
  (if var->uses
      (dict-ref var->uses var-id '())
      '()))

;; 检查 use-def 链是否已构建
(define (cfg-has-use-def? cfg)
  (and (cfg-get-info cfg 'var->def) #t))

;; 获取指令所属的块（需要 InsnId）
(define (cfg-get-insn-block cfg insn-id)
  (define insn->block (cfg-get-info cfg 'insn->block))
  (and insn->block (dict-ref insn->block insn-id #f)))

;; 获取块内的所有指令 ID（按顺序）
(define (cfg-get-block-insns cfg block-id)
  (define block->insns (cfg-get-info cfg 'block->insns))
  (if block->insns
      (dict-ref block->insns block-id '())
      '()))

(provide cfg-get-def cfg-get-uses cfg-has-use-def?)
(provide cfg-get-insn-block cfg-get-block-insns)

;; === 增量更新 ===

;; 添加指令时更新 use-def 链
(define (cfg-update-chains-add-insn cfg block-id insn-idx insn)
  (unless (cfg-has-use-def? cfg)
    (error 'cfg-update-chains-add-insn "use-def chains not built"))

  (define dloc (DefLoc block-id insn-idx))

  ;; 更新 def 链
  (define cfg*
    (for/fold ([c cfg])
              ([out (VfInsn-outputs insn)]
               #:when (VarId? out))
      (cfg-update-info c 'var->def
        (λ (d) (dict-set d out dloc))
        (ordered-map-empty var-id-compare))))

  ;; 更新 use 链（使用 extract-var-ids 处理嵌套）
  (define input-vars (extract-var-ids (VfInsn-inputs insn)))
  (for/fold ([c cfg*])
            ([(inp idx) (in-indexed input-vars)])
    (define uloc (UseLoc block-id insn-idx idx))
    (cfg-update-info c 'var->uses
      (λ (u) (dict-update u inp (λ (lst) (cons uloc lst)) '()))
      (ordered-map-empty var-id-compare))))

;; 删除指令时更新 use-def 链
(define (cfg-update-chains-remove-insn cfg block-id insn-idx insn)
  (unless (cfg-has-use-def? cfg)
    (error 'cfg-update-chains-remove-insn "use-def chains not built"))

  (define dloc (DefLoc block-id insn-idx))

  ;; 移除 def
  (define cfg*
    (for/fold ([c cfg])
              ([out (VfInsn-outputs insn)]
               #:when (VarId? out))
      (cfg-update-info c 'var->def
        (λ (d) (dict-remove d out))
        (ordered-map-empty var-id-compare))))

  ;; 移除 uses（使用 extract-var-ids 处理嵌套）
  (define input-vars (extract-var-ids (VfInsn-inputs insn)))
  (for/fold ([c cfg*])
            ([(inp idx) (in-indexed input-vars)])
    (define uloc (UseLoc block-id insn-idx idx))
    (cfg-update-info c 'var->uses
      (λ (u) (dict-update u inp
               (λ (lst) (remove uloc lst))
               '()))
      (ordered-map-empty var-id-compare))))

(provide cfg-update-chains-add-insn cfg-update-chains-remove-insn)

;; ============================================================
;; Safe Layer: 自动维护 use-def 链的操作
;; ============================================================
;;
;; 这一层封装了 CFG 修改操作，自动维护 use-def 链
;; 用户使用这些函数时无需手动更新链
;; ============================================================

;; 安全地添加指令（自动更新链）
(define (cfg/add-insn cfg block-id insn)
  (define block (cfg-get-block cfg block-id))
  (unless block
    (error 'cfg/add-insn "Block not found: ~a" block-id))

  (define insn-idx (length (CfgBlock-insns block)))
  (define cfg* (cfg-block-append-insn cfg block-id insn))

  ;; 如果有 use-def 链，自动更新
  (if (cfg-has-use-def? cfg)
      (cfg-update-chains-add-insn cfg* block-id insn-idx insn)
      cfg*))

;; 安全地替换指令（自动更新链）
(define (cfg/replace-insn cfg block-id insn-idx new-insn)
  (define block (cfg-get-block cfg block-id))
  (unless block
    (error 'cfg/replace-insn "Block not found: ~a" block-id))

  (define insns (CfgBlock-insns block))
  (unless (< insn-idx (length insns))
    (error 'cfg/replace-insn "Insn index out of range: ~a" insn-idx))

  (define old-insn (list-ref insns insn-idx))
  (define new-insns (list-set insns insn-idx new-insn))
  (define new-block (struct-copy CfgBlock block [insns new-insns]))
  (define cfg* (cfg-set-block cfg new-block))

  ;; 如果有 use-def 链，先移除旧的，再添加新的
  (if (cfg-has-use-def? cfg)
      (let* ([cfg** (cfg-update-chains-remove-insn cfg* block-id insn-idx old-insn)]
             [cfg*** (cfg-update-chains-add-insn cfg** block-id insn-idx new-insn)])
        cfg***)
      cfg*))

;; 安全地删除指令（自动更新链）
(define (cfg/remove-insn cfg block-id insn-idx)
  (define block (cfg-get-block cfg block-id))
  (unless block
    (error 'cfg/remove-insn "Block not found: ~a" block-id))

  (define insns (CfgBlock-insns block))
  (unless (< insn-idx (length insns))
    (error 'cfg/remove-insn "Insn index out of range: ~a" insn-idx))

  (define old-insn (list-ref insns insn-idx))

  ;; 注意：删除指令后，后续指令的索引会改变
  ;; 这会导致 use-def 链中的 UseLoc 失效
  ;; 解决方案：重建链，或使用稳定的指令 ID

  ;; 简化实现：删除后标记链为需要重建
  (define new-insns (append (take insns insn-idx) (drop insns (+ 1 insn-idx))))
  (define new-block (struct-copy CfgBlock block [insns new-insns]))
  (define cfg* (cfg-set-block cfg new-block))

  ;; 标记链失效，下次查询时重建
  (if (cfg-has-use-def? cfg)
      (cfg-set-info (cfg-set-info cfg* 'var->def #f) 'var->uses #f)
      cfg*))

(provide cfg/add-insn cfg/replace-insn cfg/remove-insn)

;; ============================================================
;; 惰性重建机制
;; ============================================================

;; 确保 use-def 链有效（惰性重建）
(define (cfg-ensure-use-def cfg)
  (if (cfg-has-use-def? cfg)
      cfg
      (cfg-build-use-def cfg)))

;; 带自动重建的查询
(define (cfg-def cfg var-id)
  (cfg-get-def (cfg-ensure-use-def cfg) var-id))

(define (cfg-uses cfg var-id)
  (cfg-get-uses (cfg-ensure-use-def cfg) var-id))

(provide cfg-ensure-use-def cfg-def cfg-uses)

;; ============================================================
;; 优化 Pass 框架
;; ============================================================

;; 运行优化 pass，自动管理 use-def 链
;; pass-fn: (Cfg -> Cfg) 优化函数
;; options:
;;   'preserve-chains - pass 保证自己维护链（使用 cfg/ 函数）
;;   'rebuild-chains  - pass 可能破坏链，运行后重建
;;   'no-chains       - pass 不需要链
(define (cfg-run-pass cfg pass-fn [option 'rebuild-chains])
  (case option
    [(preserve-chains)
     ;; 确保链存在，pass 负责维护
     (pass-fn (cfg-ensure-use-def cfg))]
    [(rebuild-chains)
     ;; 运行 pass 后重建链
     (define cfg* (pass-fn cfg))
     (if (cfg-has-use-def? cfg)
         (cfg-build-use-def cfg*)
         cfg*)]
    [(no-chains)
     ;; 不管链
     (pass-fn cfg)]
    [else
     (error 'cfg-run-pass "Unknown option: ~a" option)]))

(provide cfg-run-pass)

;; ============================================================
;; 辅助分析
;; ============================================================

;; 获取所有已定义的变量
(define (cfg-defined-vars cfg)
  (define cfg* (cfg-ensure-use-def cfg))
  (define var->def (cfg-get-info cfg* 'var->def))
  (if var->def
      (for/list ([(k v) (in-dict var->def)]) k)
      '()))

;; 获取死变量（定义了但从未使用）
(define (cfg-dead-vars cfg)
  (define cfg* (cfg-ensure-use-def cfg))
  (for/list ([var (cfg-defined-vars cfg*)]
             #:when (null? (cfg-get-uses cfg* var)))
    var))

;; 获取变量的 def-use 链（定义位置 + 所有使用位置）
(define (cfg-def-use-chain cfg var-id)
  (define cfg* (cfg-ensure-use-def cfg))
  (cons (cfg-get-def cfg* var-id)
        (cfg-get-uses cfg* var-id)))

(provide cfg-defined-vars cfg-dead-vars cfg-def-use-chain)
