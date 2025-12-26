#lang racket/base

;; ============================================================
;; CFG Optimization: Dead Code Elimination (DCE)
;; ============================================================
;;
;; 死代码消除
;;
;; 删除不会影响程序输出的指令：
;;   1. 定义的变量从未被使用
;;   2. 没有副作用
;;
;; 算法：
;;   1. 识别 "根" 指令（必须保留）：
;;      - 有副作用的指令（store, call, throw 等）
;;      - 被 terminator 使用的变量的定义
;;   2. 从根反向标记所有被使用的指令
;;   3. 删除未标记的指令
;;
;; 参考：GCC tree-ssa-dce.cc
;; ============================================================

(require racket/match racket/list racket/dict racket/set)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "../../cfg/ssa.rkt")

;; ============================================================
;; 副作用判断
;; ============================================================

;; 判断操作是否有副作用（必须保留）
(define (has-side-effect? op)
  (case op
    ;; 存储操作
    [(store-local put-field put-static astore) #t]
    ;; 方法调用（可能有副作用）
    [(invoke) #t]
    ;; 对象/数组创建（分配内存）
    [(new newarray anewarray) #t]
    ;; 其他纯计算无副作用
    [else #f]))

;; 判断指令是否必须保留（根指令）
(define (is-root-insn? insn)
  (and (VfInsn? insn)
       (has-side-effect? (VfInsn-op insn))))

;; ============================================================
;; 活跃指令标记
;; ============================================================

;; 从指令提取输入中的所有 VarId
(define (insn-input-vars insn)
  (if (VfInsn? insn)
      (filter VarId? (flatten (VfInsn-inputs insn)))
      '()))

;; 从 terminator 提取使用的变量
(define (terminator-input-vars term)
  (match term
    [(TermJump _) '()]
    [(TermBranch cond _ _) (if (VarId? cond) (list cond) '())]
    [(TermSwitch value _ _) (if (VarId? value) (list value) '())]
    [(TermReturn vals) (filter VarId? vals)]
    [(TermThrow exc) (if (VarId? exc) (list exc) '())]
    [(TermUnreachable) '()]))

;; 构建 VarId → 定义该变量的 (BlockId . InsnIdx) 映射
(define (build-def-location-map cfg)
  (for*/fold ([def-map (ordl-make-empty var-id-compare)])
             ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if (not block)
        def-map
        (for/fold ([m def-map])
                  ([(insn idx) (in-indexed (CfgBlock-insns block))])
          (if (VfInsn? insn)
              (for/fold ([m2 m])
                        ([out (VfInsn-outputs insn)])
                (dict-set m2 out (cons bid idx)))
              m)))))

;; 标记活跃指令
;; 返回 (set (cons BlockId InsnIdx) ...) 表示活跃的指令位置
(define (mark-live-insns cfg)
  (define def-map (build-def-location-map cfg))

  ;; 工作集：待处理的变量
  (define worklist (mutable-set))

  ;; 活跃指令集合
  (define live-insns (mutable-set))

  ;; 添加根指令和 terminator 使用的变量
  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      ;; 检查每条指令是否为根
      (for ([(insn idx) (in-indexed (CfgBlock-insns block))])
        (when (is-root-insn? insn)
          ;; 根指令本身活跃
          (set-add! live-insns (cons bid idx))
          ;; 其输入变量需要处理
          (for ([v (insn-input-vars insn)])
            (set-add! worklist v))))

      ;; terminator 使用的变量
      (for ([v (terminator-input-vars (CfgBlock-terminator block))])
        (set-add! worklist v))))

  ;; 反向传播：标记所有被使用的定义
  (let loop ()
    (unless (set-empty? worklist)
      (define var (set-first worklist))
      (set-remove! worklist var)

      ;; 找到定义该变量的指令
      (define def-loc (dict-ref def-map var #f))
      (when (and def-loc (not (set-member? live-insns def-loc)))
        ;; 标记为活跃
        (set-add! live-insns def-loc)

        ;; 获取该指令，将其输入加入工作集
        (match def-loc
          [(cons bid idx)
           (define block (cfg-get-block cfg bid))
           (when block
             (define insn (list-ref (CfgBlock-insns block) idx))
             (for ([v (insn-input-vars insn)])
               (unless (set-member? live-insns (dict-ref def-map v #f))
                 (set-add! worklist v))))]))

      (loop)))

  ;; 返回不可变集合
  (for/set ([loc live-insns]) loc))

;; ============================================================
;; 死代码删除
;; ============================================================

;; 从块中删除死代码
;; 返回 (values new-block removed-count)
(define (remove-dead-from-block block bid live-insns)
  (define-values (new-insns removed)
    (for/fold ([acc '()] [removed 0])
              ([(insn idx) (in-indexed (CfgBlock-insns block))])
      (if (set-member? live-insns (cons bid idx))
          (values (cons insn acc) removed)
          (values acc (+ removed 1)))))
  (values (struct-copy CfgBlock block
                       [insns (reverse new-insns)])
          removed))

;; DCE 主入口
(define (cfg-dce cfg)
  ;; 标记活跃指令
  (define live-insns (mark-live-insns cfg))

  ;; 删除死代码
  (define-values (cfg^ total-removed)
    (for/fold ([cfg cfg] [total 0])
              ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (if (not block)
          (values cfg total)
          (let-values ([(new-block removed) (remove-dead-from-block block bid live-insns)])
            (values (cfg-set-block cfg new-block)
                    (+ total removed))))))

  cfg^)

(provide cfg-dce)

;; ============================================================
;; 带统计版本
;; ============================================================

(define (cfg-dce-with-stats cfg)
  (define before-count
    (for/sum ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (if block (length (CfgBlock-insns block)) 0)))

  (define cfg^ (cfg-dce cfg))

  (define after-count
    (for/sum ([bid (cfg-all-block-ids cfg^)])
      (define block (cfg-get-block cfg^ bid))
      (if block (length (CfgBlock-insns block)) 0)))

  (values cfg^
          (list (cons 'removed (- before-count after-count))
                (cons 'remaining after-count))))

(provide cfg-dce-with-stats)

;; ============================================================
;; 激进 DCE（包括可能有副作用但结果未使用的调用）
;; ============================================================

;; 判断调用是否为纯函数（无副作用）
;; 这是一个保守的判断，可以通过分析扩展
(define (is-pure-call? insn)
  (match insn
    [(VfInsn 'invoke (list op owner name desc args) outputs _ _)
     ;; 目前保守地认为所有调用都有副作用
     ;; 可以添加已知纯函数的白名单
     #f]
    [_ #f]))

;; 激进模式的副作用判断
(define (has-side-effect-aggressive? op insn)
  (case op
    ;; 存储操作总是有副作用
    [(store-local put-field put-static astore) #t]
    ;; invoke 需要特殊检查
    [(invoke) (not (is-pure-call? insn))]
    ;; new/newarray 如果结果未使用且无副作用，可以删除
    ;; 但为安全起见，目前保留
    [(new newarray anewarray) #t]
    [else #f]))

(provide has-side-effect? is-root-insn?)
