#lang racket/base

;; ============================================================
;; CFG Optimization: Loop Normalization (Canonicalization)
;; ============================================================
;;
;; 循环规范化优化
;;
;; 将循环转换为规范形式：
;;   1. 归纳变量从 0 开始
;;   2. 步长为 +1
;;   3. 使用 < (小于) 比较
;;
;; 例如:
;;   for (i = 10; i < 20; i += 2)  →  for (j = 0; j < 5; j++) { i = 10 + j*2 }
;;
;; 优化效果：
;;   - 简化循环分析
;;   - 使迭代次数计算更可靠
;;   - 为向量化等优化做准备
;;
;; 参考：GCC tree-ssa-loop-ivcanon.cc
;; ============================================================

(require racket/match racket/list racket/dict racket/set)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "loop-analysis.rkt")

;; ============================================================
;; 循环特征分析 (复用 loop-unroll 的结构)
;; ============================================================

;; 使用 NormLoopInfo 避免与 loop-unroll.rkt 的 LoopInfo 冲突
(struct NormLoopInfo
  (induction-var   ; VarId - 归纳变量
   init-value      ; integer - 初始值
   bound           ; integer or VarId - 边界
   step            ; integer - 步长
   cmp-op          ; symbol - 比较操作 (lt, le, gt, ge)
   trip-count)     ; integer or #f - 迭代次数
  #:prefab)

;; 分析循环头的 PHI 节点，寻找归纳变量
(define (find-norm-induction-variable cfg loop)
  (define header (Loop-header loop))
  (define block (cfg-get-block cfg header))
  (define body (Loop-body loop))
  (define preds (compute-predecessors cfg))
  (define preheader (Loop-preheader loop))

  (and block preheader
       (for/or ([phi (CfgBlock-phis block)])
         (analyze-induction-phi cfg phi loop preheader preds))))

;; 分析单个 PHI 是否是归纳变量
(define (analyze-induction-phi cfg phi loop preheader preds)
  (define header (Loop-header loop))
  (define body (Loop-body loop))
  (define iv (PhiInsn-output phi))
  (define sources (PhiInsn-sources phi))

  (and (= (length sources) 2)
       (let ()
         (define-values (init-src update-src)
           (let ([s1 (car sources)]
                 [s2 (cadr sources)])
             (if (equal? (car s1) preheader)
                 (values s1 s2)
                 (values s2 s1))))

         (and (equal? (car init-src) preheader)
              (set-member? body (car update-src))
              (analyze-induction-pattern cfg iv init-src update-src loop)))))

;; 分析归纳变量的更新模式
(define (analyze-induction-pattern cfg iv init-src update-src loop)
  (define init-var (cdr init-src))
  (define update-var (cdr update-src))
  (define update-block-id (car update-src))

  (define init-value (get-constant-value cfg init-var))
  (define update-info (analyze-update-expr cfg update-var iv update-block-id))

  (and init-value update-info
       (match update-info
         [(list 'add step)
          (define bound-info (analyze-bound-condition cfg loop iv))
          (and bound-info
               (match bound-info
                 [(list bound-value cmp-op)
                  (define trip-count
                    (compute-trip-count init-value bound-value step cmp-op))
                  (NormLoopInfo iv init-value bound-value step cmp-op trip-count)]))]
         [_ #f])))

;; 获取变量的常量值
(define (get-constant-value cfg var)
  (cond
    [(integer? var) var]
    [(VarId? var)
     (for*/or ([bid (cfg-all-block-ids cfg)]
               [block (in-value (cfg-get-block cfg bid))]
               #:when block
               [insn (CfgBlock-insns block)]
               #:when (and (VfInsn? insn)
                           (member var (VfInsn-outputs insn))
                           (eq? (VfInsn-op insn) 'const)))
       (car (VfInsn-inputs insn)))]
    [else #f]))

;; 分析更新表达式
(define (analyze-update-expr cfg update-var iv update-block-id)
  (define block (cfg-get-block cfg update-block-id))
  (and block
       (for/or ([insn (CfgBlock-insns block)])
         (and (VfInsn? insn)
              (member update-var (VfInsn-outputs insn))
              (match insn
                [(VfInsn 'add (list a b) _ _ _)
                 (cond
                   [(and (equal? a iv) (integer? b)) (list 'add b)]
                   [(and (equal? b iv) (integer? a)) (list 'add a)]
                   [else #f])]
                [_ #f])))))

;; 分析边界条件
(define (analyze-bound-condition cfg loop iv)
  (define header (Loop-header loop))
  (define block (cfg-get-block cfg header))

  (and block
       (match (CfgBlock-terminator block)
         [(TermBranch cond then-bid else-bid)
          (define cmp-insn (find-comparison-insn cfg cond))
          (and cmp-insn
               (analyze-comparison cmp-insn iv loop then-bid else-bid cfg))]
         [_ #f])))

;; 查找比较指令
(define (find-comparison-insn cfg cond-var)
  (for*/or ([bid (cfg-all-block-ids cfg)]
            [block (in-value (cfg-get-block cfg bid))]
            #:when block
            [insn (CfgBlock-insns block)]
            #:when (and (VfInsn? insn)
                        (member cond-var (VfInsn-outputs insn))
                        (memq (VfInsn-op insn) '(lt le gt ge eq ne))))
    insn))

;; 分析比较指令
(define (analyze-comparison insn iv loop then-bid else-bid cfg)
  (define body (Loop-body loop))
  (define loop-continues-on-true (set-member? body then-bid))

  (match insn
    [(VfInsn op (list a b) _ _ _)
     (cond
       [(and (equal? a iv) (or (integer? b) (VarId? b)))
        (define bound-val (if (integer? b) b (get-constant-value cfg b)))
        (and bound-val
             (list bound-val
                   (if loop-continues-on-true
                       (case op [(lt) 'lt] [(le) 'le] [(gt) 'gt] [(ge) 'ge] [else #f])
                       (case op [(lt) 'ge] [(le) 'gt] [(gt) 'le] [(ge) 'lt] [else #f]))))]
       [(and (equal? b iv) (or (integer? a) (VarId? a)))
        (define bound-val (if (integer? a) a (get-constant-value cfg a)))
        (and bound-val
             (list bound-val
                   (if loop-continues-on-true
                       (case op [(lt) 'gt] [(le) 'ge] [(gt) 'lt] [(ge) 'le] [else #f])
                       (case op [(lt) 'le] [(le) 'lt] [(gt) 'ge] [(ge) 'gt] [else #f]))))]
       [else #f])]
    [_ #f]))

;; 计算迭代次数
(define (compute-trip-count init bound step cmp-op)
  (and (integer? init) (integer? bound) (integer? step) (not (= step 0))
       (case cmp-op
         [(lt)
          (if (> step 0)
              (max 0 (quotient (+ (- bound init) (- step 1)) step))
              #f)]
         [(le)
          (if (> step 0)
              (max 0 (+ 1 (quotient (- bound init) step)))
              #f)]
         [(gt)
          (if (< step 0)
              (max 0 (quotient (+ (- init bound) (- (- step) 1)) (- step)))
              #f)]
         [(ge)
          (if (< step 0)
              (max 0 (+ 1 (quotient (- init bound) (- step))))
              #f)]
         [else #f])))

;; ============================================================
;; 规范化检测
;; ============================================================

;; 检查循环是否已经是规范形式
(define (is-canonical? loop-info)
  (and loop-info
       (= (NormLoopInfo-init-value loop-info) 0)
       (= (NormLoopInfo-step loop-info) 1)
       (eq? (NormLoopInfo-cmp-op loop-info) 'lt)))

;; 检查循环是否可以规范化
(define (can-normalize? loop-info)
  (and loop-info
       (NormLoopInfo-trip-count loop-info)   ; 需要已知迭代次数
       (> (NormLoopInfo-trip-count loop-info) 0)
       (not (is-canonical? loop-info)))) ; 不是已经规范的

;; ============================================================
;; 变量ID生成
;; ============================================================

;; 找到 CFG 中最大的变量 ID
(define (cfg-max-var-id cfg)
  (define max-id 0)

  (define (update-max v)
    (when (VarId? v)
      (set! max-id (max max-id (VarId-id v)))))

  (define (scan-datum d)
    (cond
      [(VarId? d) (update-max d)]
      [(list? d) (for-each scan-datum d)]))

  (for* ([bid (cfg-all-block-ids cfg)]
         [block (in-value (cfg-get-block cfg bid))]
         #:when block)
    ;; 扫描 PHI
    (for ([phi (CfgBlock-phis block)])
      (update-max (PhiInsn-output phi))
      (for ([src (PhiInsn-sources phi)])
        (scan-datum (cdr src))))
    ;; 扫描指令
    (for ([insn (CfgBlock-insns block)])
      (when (VfInsn? insn)
        (scan-datum (VfInsn-inputs insn))
        (scan-datum (VfInsn-outputs insn))))
    ;; 扫描 terminator
    (match (CfgBlock-terminator block)
      [(TermBranch cond _ _) (scan-datum cond)]
      [(TermReturn vals) (scan-datum vals)]
      [(TermSwitch val _ _) (scan-datum val)]
      [_ (void)]))

  max-id)

;; ============================================================
;; 循环规范化变换
;; ============================================================

;; 规范化单个循环
;; 创建新的规范归纳变量，并用它表达原有归纳变量
(define (normalize-loop cfg loop loop-info)
  (define header (Loop-header loop))
  (define preheader (Loop-preheader loop))
  (define body (Loop-body loop))

  (define old-iv (NormLoopInfo-induction-var loop-info))
  (define init (NormLoopInfo-init-value loop-info))
  (define step (NormLoopInfo-step loop-info))
  (define trip-count (NormLoopInfo-trip-count loop-info))

  ;; 生成新变量 ID
  (define base-id (+ 1 (cfg-max-var-id cfg)))
  (define new-iv (VarId base-id))           ; 规范归纳变量
  (define new-iv-next (VarId (+ base-id 1))) ; 规范归纳变量的更新值
  (define new-cmp (VarId (+ base-id 2)))    ; 新比较结果
  (define new-init (VarId (+ base-id 3)))   ; 新初始值 (0)
  (define new-bound (VarId (+ base-id 4)))  ; 新边界 (trip-count)
  (define old-iv-computed (VarId (+ base-id 5))) ; 计算得到的旧 IV 值

  ;; 1. 修改 preheader：添加初始化 new-iv = 0
  (define cfg1
    (cfg-block-append-insn cfg preheader
      (VfInsn 'const '(0) (list new-init) #f #f)))

  (define cfg2
    (cfg-block-append-insn cfg1 preheader
      (VfInsn 'const (list trip-count) (list new-bound) #f #f)))

  ;; 2. 修改 header：
  ;;    - 添加新的 PHI: new-iv = phi(new-init, new-iv-next)
  ;;    - 添加计算旧 IV: old-iv = init + new-iv * step
  ;;    - 修改比较: new-cmp = new-iv < trip-count

  (define header-block (cfg-get-block cfg2 header))

  ;; 找到更新块（body 中跳回 header 的块）
  (define update-block-id
    (for/first ([be (Loop-back-edges loop)])
      (car be)))

  ;; 添加新的 PHI
  (define new-phi
    (PhiInsn new-iv (list (cons preheader new-init)
                          (cons update-block-id new-iv-next))))

  (define cfg3
    (cfg-block-add-phi cfg2 header new-phi))

  ;; 在 header 的指令开头添加旧 IV 的计算
  ;; old_iv = init + new_iv * step
  ;; 分解为: tmp = new_iv * step; old_iv = init + tmp
  (define header-block3 (cfg-get-block cfg3 header))

  ;; 如果 step = 1，简化为 old_iv = init + new_iv
  ;; 如果 init = 0 且 step = 1，可以直接复用 new_iv
  (define cfg4
    (if (and (= init 0) (= step 1))
        ;; 可以直接用新 IV 替代旧 IV - 不需要额外计算
        cfg3
        ;; 需要计算 old_iv = init + new_iv * step
        (if (= step 1)
            ;; old_iv = init + new_iv
            (let ()
              (define add-insn
                (VfInsn 'add (list new-iv init) (list old-iv-computed) #f #f))
              (cfg-block-prepend-insn cfg3 header add-insn))
            ;; old_iv = init + new_iv * step
            (let ()
              (define tmp-var (VarId (+ base-id 6)))
              (define mul-insn
                (VfInsn 'mul (list new-iv step) (list tmp-var) #f #f))
              (define add-insn
                (VfInsn 'add (list tmp-var init) (list old-iv-computed) #f #f))
              (define cfg-a (cfg-block-prepend-insn cfg3 header mul-insn))
              (cfg-block-prepend-insn cfg-a header add-insn)))))

  ;; 3. 找到并修改比较指令
  (define header-block4 (cfg-get-block cfg4 header))
  (define old-cmp-var
    (match (CfgBlock-terminator header-block4)
      [(TermBranch cond _ _) cond]
      [_ #f]))

  ;; 添加新的比较指令
  (define new-cmp-insn
    (VfInsn 'lt (list new-iv new-bound) (list new-cmp) #f #f))

  (define cfg5 (cfg-block-append-insn cfg4 header new-cmp-insn))

  ;; 更新 terminator 使用新的比较结果
  (define header-block5 (cfg-get-block cfg5 header))
  (define old-term (CfgBlock-terminator header-block5))
  (define new-term
    (match old-term
      [(TermBranch _ then-bid else-bid)
       ;; 确保循环继续在 then 分支
       (if (set-member? body then-bid)
           (TermBranch new-cmp then-bid else-bid)
           (TermBranch new-cmp else-bid then-bid))]  ; 如果原来是反的，交换
      [t t]))

  (define cfg6
    (cfg-block-set-terminator cfg5 header new-term))

  ;; 4. 修改更新块：添加 new-iv-next = new-iv + 1
  (define new-update-insn
    (VfInsn 'add (list new-iv 1) (list new-iv-next) #f #f))

  (define cfg7
    (cfg-block-append-insn cfg6 update-block-id new-update-insn))

  ;; 5. 替换循环体中对旧 IV 的使用（除了 PHI）
  (define replacement-var
    (if (and (= init 0) (= step 1))
        new-iv
        old-iv-computed))

  (define cfg8
    (if (equal? replacement-var old-iv)
        cfg7  ; 不需要替换
        (replace-var-uses-in-loop cfg7 loop old-iv replacement-var)))

  cfg8)

;; 在块开头添加指令
(define (cfg-block-prepend-insn cfg bid insn)
  (define block (cfg-get-block cfg bid))
  (define new-block
    (struct-copy CfgBlock block
                 [insns (cons insn (CfgBlock-insns block))]))
  (cfg-set-block cfg new-block))

;; 替换循环中变量的使用
(define (replace-var-uses-in-loop cfg loop old-var new-var)
  (define body (Loop-body loop))
  (define header (Loop-header loop))

  (for/fold ([cfg cfg])
            ([bid (in-set body)])
    (define block (cfg-get-block cfg bid))
    (if (not block)
        cfg
        (let ()
          ;; 替换指令中的使用（跳过 header 的 PHI 定义）
          (define new-insns
            (for/list ([insn (CfgBlock-insns block)])
              (if (VfInsn? insn)
                  (struct-copy VfInsn insn
                               [inputs (replace-in-datum (VfInsn-inputs insn)
                                                         old-var new-var)])
                  insn)))

          ;; 替换 terminator 中的使用
          (define new-term
            (replace-in-terminator (CfgBlock-terminator block) old-var new-var))

          (define new-block
            (struct-copy CfgBlock block
                         [insns new-insns]
                         [terminator new-term]))

          (cfg-set-block cfg new-block)))))

;; 在数据中替换变量
(define (replace-in-datum datum old-var new-var)
  (cond
    [(equal? datum old-var) new-var]
    [(list? datum) (map (λ (d) (replace-in-datum d old-var new-var)) datum)]
    [else datum]))

;; 在 terminator 中替换变量
(define (replace-in-terminator term old-var new-var)
  (match term
    [(TermBranch cond then-bid else-bid)
     (TermBranch (replace-in-datum cond old-var new-var) then-bid else-bid)]
    [(TermReturn vals)
     (TermReturn (replace-in-datum vals old-var new-var))]
    [(TermSwitch val cases default-bid)
     (TermSwitch (replace-in-datum val old-var new-var) cases default-bid)]
    [t t]))

;; ============================================================
;; 主入口
;; ============================================================

(define (cfg-loop-normalize cfg)
  (define loops (analyze-loops cfg))

  (for/fold ([cfg cfg])
            ([loop (sort-loops-by-depth loops)])  ; 内层循环优先
    (define loop-info (find-norm-induction-variable cfg loop))
    (cond
      [(can-normalize? loop-info)
       (normalize-loop cfg loop loop-info)]
      [else cfg])))

(provide cfg-loop-normalize)

;; ============================================================
;; 带统计版本
;; ============================================================

(define (cfg-loop-normalize-with-stats cfg)
  (define loops (analyze-loops cfg))
  (define normalizable-count
    (for/sum ([loop loops])
      (define loop-info (find-norm-induction-variable cfg loop))
      (if (can-normalize? loop-info) 1 0)))

  (define cfg^ (cfg-loop-normalize cfg))

  (values cfg^
          `((loops-found . ,(length loops))
            (loops-normalizable . ,normalizable-count))))

(provide cfg-loop-normalize-with-stats)

;; ============================================================
;; 导出分析函数（用于测试）
;; ============================================================

(provide NormLoopInfo NormLoopInfo?
         NormLoopInfo-induction-var NormLoopInfo-init-value
         NormLoopInfo-bound NormLoopInfo-step NormLoopInfo-cmp-op NormLoopInfo-trip-count
         find-norm-induction-variable
         is-canonical? can-normalize?)
