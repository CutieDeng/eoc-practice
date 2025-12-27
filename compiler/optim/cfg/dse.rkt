#lang racket/base

;; ============================================================
;; CFG Optimization: Dead Store Elimination (DSE)
;; ============================================================
;;
;; 删除死存储：不会被后续读取的内存写入
;;
;; 死存储的条件：
;; 1. 存储后在同一位置有另一个存储，且中间没有读取
;; 2. 存储的值从未被读取
;;
;; 参考：GCC tree-ssa-dse.cc
;; ============================================================

(require racket/match racket/list racket/set racket/hash)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "alias-analysis.rkt")
(require "loop-analysis.rkt")

;; ============================================================
;; Local Dead Store Elimination
;; ============================================================

;; 在单个基本块内消除死存储
;; 后向遍历指令，追踪哪些存储会被后续覆盖
(define (eliminate-dead-stores-in-block insns)
  ;; 后向遍历
  ;; active-stores: 活跃存储集合（位置 -> 存储指令）
  ;; result: 保留的指令（逆序）

  (define-values (result _)
    (for/fold ([kept '()] [active-stores (hash)])
              ([insn (reverse insns)])
      (cond
        ;; 写入操作
        [(and (VfInsn? insn) (memory-write-op? (VfInsn-op insn)))
         (define loc (insn-memory-loc insn))
         (cond
           [(not loc)
            ;; 无法确定位置，保留并使所有存储活跃
            (values (cons insn kept) (hash))]

           [else
            ;; 检查是否有活跃存储覆盖当前存储
            (define killed?
              (for/or ([(active-loc active-insn) (in-hash active-stores)])
                (equal? (may-alias? loc active-loc) alias-must)))
            (if killed?
                ;; 当前存储被覆盖，删除
                (values kept active-stores)
                ;; 保留当前存储并标记为活跃
                (values (cons insn kept)
                        (hash-set active-stores loc insn)))])]

        ;; 读取操作
        [(and (VfInsn? insn) (memory-read-op? (VfInsn-op insn)))
         (define loc (insn-memory-loc insn))
         (cond
           [(not loc)
            ;; 无法确定位置，保留并使所有存储不可消除
            (values (cons insn kept) (hash))]

           [else
            ;; 移除可能被读取的存储（它们不是死存储）
            (define new-active
              (for/hash ([(active-loc active-insn) (in-hash active-stores)]
                         #:unless (not (equal? (may-alias? loc active-loc) alias-no)))
                (values active-loc active-insn)))
            (values (cons insn kept) new-active)])]

        ;; 有副作用的操作（如函数调用）
        [(and (VfInsn? insn) (has-side-effect? (VfInsn-op insn)))
         ;; 保守处理：清除所有活跃存储
         (values (cons insn kept) (hash))]

        ;; 其他操作
        [else
         (values (cons insn kept) active-stores)])))

  result)

;; ============================================================
;; Store-Local Dead Store Elimination
;; ============================================================

;; 专门处理 store-local 操作的死存储消除
;; store-local 存储到局部变量槽
(define (eliminate-dead-store-locals insns)
  ;; 后向遍历，追踪每个局部变量槽的最后一次存储
  (define-values (result _)
    (for/fold ([kept '()] [last-stores (hash)])
              ([insn (reverse insns)])
      (match insn
        ;; store-local: 存储到局部变量
        [(VfInsn 'store-local (list val idx) '() _ _)
         (cond
           ;; 如果这个槽后面有另一个存储，当前存储是死的
           [(hash-has-key? last-stores idx)
            (values kept last-stores)]
           ;; 否则保留并记录
           [else
            (values (cons insn kept)
                    (hash-set last-stores idx insn))])]

        ;; load-local: 读取局部变量
        [(VfInsn 'load-local (list idx) _ _ _)
         ;; 该槽被读取，移除其"最后存储"记录
         (values (cons insn kept)
                 (hash-remove last-stores idx))]

        ;; 函数调用等：保守处理，清除所有记录
        [(VfInsn op _ _ _ _)
         #:when (has-side-effect? op)
         (values (cons insn kept) (hash))]

        ;; 其他
        [_
         (values (cons insn kept) last-stores)])))

  result)

;; ============================================================
;; Global Dead Store Elimination
;; ============================================================

;; 跨基本块的死存储消除
;; 使用活跃变量分析的思路

;; 计算每个块的 GEN 和 KILL 集合（针对存储）
(define (compute-store-gen-kill cfg)
  (define gen-map (make-hash))
  (define kill-map (make-hash))

  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (define gen (mutable-set))
      (define kill (mutable-set))

      ;; 正向遍历块中的指令
      (for ([insn (CfgBlock-insns block)])
        (when (VfInsn? insn)
          (define op (VfInsn-op insn))
          (cond
            ;; 写入操作生成一个存储
            [(memory-write-op? op)
             (define loc (insn-memory-loc insn))
             (when loc
               (set-add! gen loc))]

            ;; 读取操作杀死对应位置的"死存储"属性
            [(memory-read-op? op)
             (define loc (insn-memory-loc insn))
             (when loc
               (set-add! kill loc))])))

      (hash-set! gen-map bid (set-copy gen))
      (hash-set! kill-map bid (set-copy kill))))

  (values gen-map kill-map))

;; ============================================================
;; Main DSE Pass
;; ============================================================

;; 对整个 CFG 执行死存储消除
(define (cfg-dse cfg)
  ;; 对每个块执行局部 DSE
  (for/fold ([cfg cfg])
            ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (cond
      [(not block) cfg]
      [else
       (define new-insns
         (eliminate-dead-store-locals
          (eliminate-dead-stores-in-block (CfgBlock-insns block))))
       (if (equal? new-insns (CfgBlock-insns block))
           cfg
           (cfg-set-block cfg
                          (struct-copy CfgBlock block
                                       [insns new-insns])))])))

(provide cfg-dse)

;; ============================================================
;; With Statistics
;; ============================================================

(define (cfg-dse-with-stats cfg)
  (define eliminated-count 0)

  (define cfg^
    (for/fold ([cfg cfg])
              ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (cond
        [(not block) cfg]
        [else
         (define old-insns (CfgBlock-insns block))
         (define new-insns
           (eliminate-dead-store-locals
            (eliminate-dead-stores-in-block old-insns)))
         (define diff (- (length old-insns) (length new-insns)))
         (set! eliminated-count (+ eliminated-count diff))
         (if (= diff 0)
             cfg
             (cfg-set-block cfg
                            (struct-copy CfgBlock block
                                         [insns new-insns])))])))

  (values cfg^
          `((eliminated-stores . ,eliminated-count))))

(provide cfg-dse-with-stats)
