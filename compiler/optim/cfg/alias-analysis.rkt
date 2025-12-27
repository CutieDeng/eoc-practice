#lang racket/base

;; ============================================================
;; CFG Analysis: Alias Analysis
;; ============================================================
;;
;; 别名分析：确定内存引用是否可能指向同一位置
;;
;; 概念：
;; - Must-alias: 两个引用一定指向相同位置
;; - May-alias: 两个引用可能指向相同位置
;; - No-alias: 两个引用一定不指向相同位置
;;
;; 内存操作：
;; - load: 从内存读取
;; - store: 写入内存
;; - vector-ref: 向量读取
;; - vector-set!: 向量写入
;;
;; 参考：GCC tree-ssa-alias.cc
;; ============================================================

(require racket/match racket/list racket/set racket/hash)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")

;; ============================================================
;; Memory Location Representation
;; ============================================================

;; 内存位置表示
;; 用于追踪指向内存的引用
(struct MemLoc (
  base     ; VarId 或 'global 或 'unknown - 基地址
  offset   ; Integer 或 'unknown - 偏移量
  size     ; Integer 或 'unknown - 大小
) #:prefab)

;; 特殊内存位置
(define mem-unknown (MemLoc 'unknown 'unknown 'unknown))

(provide (struct-out MemLoc) mem-unknown)

;; ============================================================
;; Alias Query Results
;; ============================================================

;; 别名查询结果
(define alias-no 'no-alias)        ; 一定不别名
(define alias-may 'may-alias)      ; 可能别名
(define alias-must 'must-alias)    ; 一定别名

(provide alias-no alias-may alias-must)

;; ============================================================
;; Memory Operation Classification
;; ============================================================

;; 检查操作是否为内存读取
(define (memory-read-op? op)
  (and (memq op '(load load-local vector-ref
                  aload iaload laload faload daload
                  aaload baload caload saload
                  getfield getstatic))
       #t))

;; 检查操作是否为内存写入
(define (memory-write-op? op)
  (and (memq op '(store store-local vector-set!
                  astore istore lstore fstore dstore
                  aastore iastore lastore fastore dastore
                  bastore castore sastore
                  putfield putstatic))
       #t))

;; 检查操作是否有副作用（可能修改内存）
(define (has-side-effect? op)
  (or (memory-write-op? op)
      (and (memq op '(call invoke invokevirtual invokestatic
                      invokeinterface invokespecial
                      new newarray anewarray multianewarray
                      athrow monitorenter monitorexit))
           #t)))

(provide memory-read-op? memory-write-op? has-side-effect?)

;; ============================================================
;; Extract Memory Location from Instruction
;; ============================================================

;; 从指令中提取内存位置
(define (insn-memory-loc insn)
  (match insn
    [(VfInsn 'load-local (list idx) _ _ _)
     (MemLoc 'local idx 1)]

    [(VfInsn 'store-local (list _ idx) _ _ _)
     (MemLoc 'local idx 1)]

    [(VfInsn 'vector-ref (list base-var (? integer? idx)) _ _ _)
     (MemLoc base-var idx 1)]

    [(VfInsn 'vector-set! (list base-var (? integer? idx) _) _ _ _)
     (MemLoc base-var idx 1)]

    [(VfInsn (? memory-read-op? op) inputs _ _ _)
     ;; 通用内存读取 - 尝试提取基地址
     (if (and (pair? inputs) (VarId? (car inputs)))
         (MemLoc (car inputs) 'unknown 'unknown)
         mem-unknown)]

    [(VfInsn (? memory-write-op? op) inputs _ _ _)
     ;; 通用内存写入
     (if (and (pair? inputs) (VarId? (car inputs)))
         (MemLoc (car inputs) 'unknown 'unknown)
         mem-unknown)]

    [_ #f]))

(provide insn-memory-loc)

;; ============================================================
;; Alias Query
;; ============================================================

;; 判断两个内存位置是否可能别名
(define (may-alias? loc1 loc2)
  (define base1 (MemLoc-base loc1))
  (define base2 (MemLoc-base loc2))

  (cond
    ;; 任一为 unknown，保守返回 may-alias
    [(or (equal? base1 'unknown)
         (equal? base2 'unknown))
     alias-may]

    ;; 都是局部变量 ('local)，比较偏移量
    [(and (equal? base1 'local) (equal? base2 'local))
     (define off1 (MemLoc-offset loc1))
     (define off2 (MemLoc-offset loc2))
     (cond
       [(or (equal? off1 'unknown) (equal? off2 'unknown)) alias-may]
       [(= off1 off2) alias-must]
       [else alias-no])]

    ;; 不同的基地址（非 'local）
    [(not (equal? base1 base2))
     ;; 不同变量基地址，保守返回 may-alias
     ;; （它们可能指向同一堆对象）
     alias-may]

    ;; 相同基地址（VarId）
    [else
     (define off1 (MemLoc-offset loc1))
     (define off2 (MemLoc-offset loc2))
     (define sz1 (MemLoc-size loc1))
     (define sz2 (MemLoc-size loc2))

     (cond
       ;; 偏移量未知
       [(or (equal? off1 'unknown) (equal? off2 'unknown))
        alias-may]

       ;; 相同偏移量
       [(= off1 off2)
        alias-must]

       ;; 检查范围是否重叠
       ;; 不重叠的条件: off1 + sz1 <= off2 OR off2 + sz2 <= off1
       [(and (integer? sz1) (integer? sz2))
        (if (or (<= (+ off1 sz1) off2)
                (<= (+ off2 sz2) off1))
            alias-no
            alias-may)]

       ;; 大小未知，保守处理
       [else alias-may])]))

(provide may-alias?)

;; ============================================================
;; Instruction Alias Query
;; ============================================================

;; 判断两条指令是否可能访问相同内存
(define (insns-may-alias? insn1 insn2)
  (define loc1 (insn-memory-loc insn1))
  (define loc2 (insn-memory-loc insn2))

  (cond
    ;; 任一不是内存操作
    [(or (not loc1) (not loc2)) #f]

    ;; 执行别名分析
    [else
     (define result (may-alias? loc1 loc2))
     (not (equal? result alias-no))]))

(provide insns-may-alias?)

;; ============================================================
;; Kill Analysis for DSE
;; ============================================================

;; 检查 store2 是否完全覆盖 store1
;; 用于死存储消除
(define (store-kills? store1 store2)
  (define loc1 (insn-memory-loc store1))
  (define loc2 (insn-memory-loc store2))

  (cond
    [(or (not loc1) (not loc2)) #f]

    ;; 必须相同基地址和偏移
    [(and (equal? (MemLoc-base loc1) (MemLoc-base loc2))
          (equal? (MemLoc-offset loc1) (MemLoc-offset loc2))
          (integer? (MemLoc-offset loc1)))
     ;; 检查大小
     (define sz1 (MemLoc-size loc1))
     (define sz2 (MemLoc-size loc2))
     (or (equal? sz1 sz2)
         (and (integer? sz1) (integer? sz2) (>= sz2 sz1)))]

    [else #f]))

(provide store-kills?)

;; ============================================================
;; Points-to Analysis (Simplified)
;; ============================================================

;; 简化的指向分析
;; 追踪变量可能指向的内存对象

;; 指向集合
(struct PointsTo (
  map  ; Hash[VarId -> Set[Symbol]] - 变量 -> 可能指向的对象集
) #:prefab)

;; 创建空的指向分析
(define (points-to-empty)
  (PointsTo (hash)))

;; 添加指向关系
(define (points-to-add pt var target)
  (define m (PointsTo-map pt))
  (define current (hash-ref m var (set)))
  (PointsTo (hash-set m var (set-add current target))))

;; 获取变量的指向集
(define (points-to-get pt var)
  (hash-ref (PointsTo-map pt) var (set)))

;; 合并指向分析
(define (points-to-merge pt1 pt2)
  (define m1 (PointsTo-map pt1))
  (define m2 (PointsTo-map pt2))
  (PointsTo
   (for/fold ([m m1])
             ([(k v) (in-hash m2)])
     (define current (hash-ref m k (set)))
     (hash-set m k (set-union current v)))))

(provide (struct-out PointsTo)
         points-to-empty points-to-add points-to-get points-to-merge)

;; ============================================================
;; CFG-level Alias Analysis
;; ============================================================

;; 对整个 CFG 执行别名分析
;; 返回 AliasInfo 供其他优化使用
(struct AliasInfo (
  mem-ops     ; (Listof (cons BlockId VfInsn)) - 所有内存操作
  stores      ; (Listof (cons BlockId VfInsn)) - 所有存储操作
  loads       ; (Listof (cons BlockId VfInsn)) - 所有加载操作
) #:prefab)

(define (analyze-aliases cfg)
  (define mem-ops '())
  (define stores '())
  (define loads '())

  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (for ([insn (CfgBlock-insns block)])
        (when (VfInsn? insn)
          (define op (VfInsn-op insn))
          (when (or (memory-read-op? op) (memory-write-op? op))
            (set! mem-ops (cons (cons bid insn) mem-ops)))
          (when (memory-read-op? op)
            (set! loads (cons (cons bid insn) loads)))
          (when (memory-write-op? op)
            (set! stores (cons (cons bid insn) stores)))))))

  (AliasInfo (reverse mem-ops) (reverse stores) (reverse loads)))

(provide (struct-out AliasInfo) analyze-aliases)
