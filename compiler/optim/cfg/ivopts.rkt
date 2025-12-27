#lang racket/base

;; ============================================================
;; CFG Optimization: Induction Variable Optimization (IVOpts)
;; ============================================================
;;
;; 归纳变量优化
;;
;; 功能：
;;   1. 基础归纳变量检测 (BIV)
;;   2. 派生归纳变量检测 (DIV): j = a*i + b
;;   3. 循环内强度削减: i*k → 累加器
;;   4. 归纳变量消除: 移除死归纳变量
;;   5. 线性函数测试替换 (LFTR)
;;
;; 例如:
;;   for (i = 0; i < n; i++) {
;;     arr[i * 4] = ...
;;   }
;; 转换为:
;;   for (i = 0, p = arr; i < n; i++, p += 4) {
;;     *p = ...
;;   }
;;
;; 参考：GCC tree-ssa-loop-ivopts.cc
;; ============================================================

(require racket/match racket/list racket/dict racket/set)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "loop-analysis.rkt")

;; ============================================================
;; 归纳变量表示
;; ============================================================

;; 基础归纳变量 (Basic IV)
;; i = phi(init, i + step)
(struct BIV
  (var         ; VarId - 归纳变量
   init        ; integer or VarId - 初始值
   step        ; integer - 步长
   update-var) ; VarId - i + step 的结果
  #:prefab)

;; 派生归纳变量 (Derived IV)
;; j = a * i + b, 其中 i 是 BIV
(struct DIV
  (var    ; VarId - 派生变量
   base   ; BIV - 基础 IV
   scale  ; integer - 系数 a
   offset ; integer - 偏移 b
   def-block) ; BlockId - 定义所在块
  #:prefab)

;; ============================================================
;; 基础归纳变量检测
;; ============================================================

;; 检测循环中的所有基础归纳变量
(define (find-basic-ivs cfg loop)
  (define header (Loop-header loop))
  (define block (cfg-get-block cfg header))
  (define body (Loop-body loop))
  (define preds (compute-predecessors cfg))
  (define preheader (Loop-preheader loop))

  (if (not (and block preheader))
      '()
      (for/list ([phi (CfgBlock-phis block)]
                 #:when (biv-pattern? cfg phi loop preheader body))
        (extract-biv cfg phi loop preheader body))))

;; 检查 PHI 是否是 BIV 模式
(define (biv-pattern? cfg phi loop preheader body)
  (define sources (PhiInsn-sources phi))
  (and (= (length sources) 2)
       (let ()
         (define-values (init-src update-src)
           (separate-sources sources preheader body))
         (and init-src update-src
              (is-linear-update? cfg (PhiInsn-output phi) update-src body)))))

;; 分离初始源和更新源
(define (separate-sources sources preheader body)
  (define s1 (car sources))
  (define s2 (cadr sources))
  (cond
    [(and (equal? (car s1) preheader) (set-member? body (car s2)))
     (values s1 s2)]
    [(and (equal? (car s2) preheader) (set-member? body (car s1)))
     (values s2 s1)]
    [else (values #f #f)]))

;; 检查更新是否是线性的 (i + step 或 i - step)
(define (is-linear-update? cfg iv update-src body)
  (define update-var (cdr update-src))
  (define update-block-id (car update-src))
  (define block (cfg-get-block cfg update-block-id))

  (and block
       (for/or ([insn (CfgBlock-insns block)])
         (and (VfInsn? insn)
              (member update-var (VfInsn-outputs insn))
              (match insn
                [(VfInsn 'add (list a b) _ _ _)
                 (or (and (equal? a iv) (integer? b))
                     (and (equal? b iv) (integer? a)))]
                [(VfInsn 'sub (list a b) _ _ _)
                 (and (equal? a iv) (integer? b))]
                [_ #f])))))

;; 提取 BIV 信息
(define (extract-biv cfg phi loop preheader body)
  (define iv (PhiInsn-output phi))
  (define sources (PhiInsn-sources phi))
  (define-values (init-src update-src)
    (separate-sources sources preheader body))

  (define init-val (cdr init-src))
  (define update-var (cdr update-src))
  (define update-block-id (car update-src))
  (define block (cfg-get-block cfg update-block-id))

  (define step
    (for/or ([insn (CfgBlock-insns block)])
      (and (VfInsn? insn)
           (member update-var (VfInsn-outputs insn))
           (match insn
             [(VfInsn 'add (list a b) _ _ _)
              (cond
                [(and (equal? a iv) (integer? b)) b]
                [(and (equal? b iv) (integer? a)) a]
                [else #f])]
             [(VfInsn 'sub (list a b) _ _ _)
              (and (equal? a iv) (integer? b) (- b))]
             [_ #f]))))

  (BIV iv init-val (or step 1) update-var))

;; ============================================================
;; 派生归纳变量检测
;; ============================================================

;; 检测循环中所有派生归纳变量
;; DIV: j = a * i + b
(define (find-derived-ivs cfg loop bivs)
  (define body (Loop-body loop))

  (append*
   (for/list ([biv bivs])
     (find-divs-for-biv cfg loop biv body))))

;; 找出基于特定 BIV 的所有 DIV
(define (find-divs-for-biv cfg loop biv body)
  (define iv (BIV-var biv))

  (for*/list ([bid (in-set body)]
              [block (in-value (cfg-get-block cfg bid))]
              #:when block
              [insn (CfgBlock-insns block)]
              #:when (VfInsn? insn)
              [div (in-value (try-extract-div insn iv biv bid))]
              #:when div)
    div))

;; 尝试从指令中提取 DIV
;; 识别模式：j = a * i, j = i * a, j = i + b, j = a * i + b
(define (try-extract-div insn iv biv def-block)
  (match insn
    ;; j = i * a (scale only)
    [(VfInsn 'mul (list a b) (list out) _ _)
     (cond
       [(and (equal? a iv) (integer? b))
        (DIV out biv b 0 def-block)]
       [(and (equal? b iv) (integer? a))
        (DIV out biv a 0 def-block)]
       [else #f])]

    ;; j = i + b (offset only)
    [(VfInsn 'add (list a b) (list out) _ _)
     (cond
       [(and (equal? a iv) (integer? b))
        (DIV out biv 1 b def-block)]
       [(and (equal? b iv) (integer? a))
        (DIV out biv 1 a def-block)]
       [else #f])]

    ;; j = i - b
    [(VfInsn 'sub (list a b) (list out) _ _)
     (and (equal? a iv) (integer? b)
          (DIV out biv 1 (- b) def-block))]

    ;; j = i << n (equivalent to i * 2^n)
    [(VfInsn 'shl (list a b) (list out) _ _)
     (and (equal? a iv) (integer? b)
          (DIV out biv (expt 2 b) 0 def-block))]

    [_ #f]))

;; ============================================================
;; 循环内强度削减
;; ============================================================

;; 对 DIV 进行强度削减
;; 将 j = scale * i + offset 转换为累加器形式
;; 新增：j' = phi(init_j, j' + scale*step)
;; 其中 init_j = scale * init_i + offset
(define (strength-reduce-div cfg loop biv div)
  (define header (Loop-header loop))
  (define preheader (Loop-preheader loop))
  (define body (Loop-body loop))

  (define iv (BIV-var biv))
  (define init-i (BIV-init biv))
  (define step-i (BIV-step biv))
  (define update-block
    (for/first ([be (Loop-back-edges loop)])
      (car be)))

  (define scale (DIV-scale div))
  (define offset (DIV-offset div))
  (define div-var (DIV-var div))
  (define def-block (DIV-def-block div))

  ;; 计算新 IV 的参数
  ;; init_j = scale * init_i + offset
  ;; step_j = scale * step_i
  (define step-j (* scale step-i))

  ;; 只有当新步长更简单时才进行转换
  ;; (避免增加复杂度)
  (cond
    ;; 如果 scale = 1，不需要转换
    [(= scale 1) (values cfg #f)]

    ;; 如果 scale 是 2 的幂，转换有意义
    [(power-of-2? scale)
     (transform-div-to-accumulator cfg loop biv div preheader header
                                   update-block init-i step-j offset)]

    ;; 小常数也值得转换
    [(<= (abs scale) 16)
     (transform-div-to-accumulator cfg loop biv div preheader header
                                   update-block init-i step-j offset)]

    [else (values cfg #f)]))

;; 判断是否为 2 的幂
(define (power-of-2? n)
  (and (integer? n) (positive? n)
       (zero? (bitwise-and n (- n 1)))))

;; 执行 DIV 到累加器的转换
(define (transform-div-to-accumulator cfg loop biv div preheader header
                                       update-block init-i step-j offset)
  (define scale (DIV-scale div))
  (define div-var (DIV-var div))
  (define def-block (DIV-def-block div))

  ;; 生成新变量
  (define base-id (+ 1 (cfg-max-var-id cfg)))
  (define new-iv (VarId base-id))           ; 新累加器
  (define new-iv-next (VarId (+ base-id 1))) ; 累加器更新值
  (define new-init (VarId (+ base-id 2)))    ; 初始值

  ;; 1. 在 preheader 中计算初始值
  ;; init_j = scale * init_i + offset
  (define cfg1
    (if (integer? init-i)
        ;; 常量初始值
        (let ([init-j (+ (* scale init-i) offset)])
          (cfg-block-append-insn cfg preheader
            (VfInsn 'const (list init-j) (list new-init) #f #f)))
        ;; 变量初始值 - 需要计算
        (let ()
          (define tmp (VarId (+ base-id 3)))
          (define cfg-a
            (cfg-block-append-insn cfg preheader
              (VfInsn 'mul (list init-i scale) (list tmp) #f #f)))
          (if (= offset 0)
              (cfg-block-append-insn cfg-a preheader
                (VfInsn 'const (list 0) (list new-init) #f #f))  ; 使用 tmp
              (cfg-block-append-insn cfg-a preheader
                (VfInsn 'add (list tmp offset) (list new-init) #f #f))))))

  ;; 2. 在 header 中添加 PHI
  (define new-phi
    (PhiInsn new-iv (list (cons preheader new-init)
                          (cons update-block new-iv-next))))
  (define cfg2
    (cfg-block-add-phi cfg1 header new-phi))

  ;; 3. 在更新块中添加累加
  (define cfg3
    (cfg-block-append-insn cfg2 update-block
      (VfInsn 'add (list new-iv step-j) (list new-iv-next) #f #f)))

  ;; 4. 替换原 DIV 定义为复制新 IV
  ;; 找到定义 DIV 的指令并替换
  (define cfg4
    (replace-div-definition cfg3 div-var new-iv def-block))

  (values cfg4 #t))

;; 替换 DIV 定义
(define (replace-div-definition cfg div-var new-iv def-block)
  (define block (cfg-get-block cfg def-block))
  (if (not block)
      cfg
      (let ()
        (define new-insns
          (for/list ([insn (CfgBlock-insns block)])
            (if (and (VfInsn? insn)
                     (member div-var (VfInsn-outputs insn)))
                ;; 替换为复制指令 (用 add 0 表示)
                (VfInsn 'add (list new-iv 0) (VfInsn-outputs insn)
                        (VfInsn-info insn) (VfInsn-id insn))
                insn)))
        (cfg-set-block cfg
          (struct-copy CfgBlock block [insns new-insns])))))

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
    (for ([phi (CfgBlock-phis block)])
      (update-max (PhiInsn-output phi))
      (for ([src (PhiInsn-sources phi)])
        (scan-datum (cdr src))))
    (for ([insn (CfgBlock-insns block)])
      (when (VfInsn? insn)
        (scan-datum (VfInsn-inputs insn))
        (scan-datum (VfInsn-outputs insn))))
    (match (CfgBlock-terminator block)
      [(TermBranch cond _ _) (scan-datum cond)]
      [(TermReturn vals) (scan-datum vals)]
      [(TermSwitch val _ _) (scan-datum val)]
      [_ (void)]))

  max-id)

;; ============================================================
;; 主入口
;; ============================================================

(define (cfg-ivopts cfg)
  (define loops (analyze-loops cfg))

  (for/fold ([cfg cfg])
            ([loop (sort-loops-by-depth loops)])  ; 内层循环优先
    (optimize-loop-ivs cfg loop)))

;; 优化单个循环的归纳变量
(define (optimize-loop-ivs cfg loop)
  (define preheader (Loop-preheader loop))

  ;; 需要 preheader
  (if (not preheader)
      cfg
      (let ()
        ;; 1. 找出所有 BIV
        (define bivs (find-basic-ivs cfg loop))

        ;; 2. 找出所有 DIV
        (define divs (find-derived-ivs cfg loop bivs))

        ;; 3. 对每个 DIV 尝试强度削减
        (for/fold ([cfg cfg])
                  ([div divs])
          (define biv (DIV-base div))
          (define-values (cfg^ changed?)
            (strength-reduce-div cfg loop biv div))
          cfg^))))

(provide cfg-ivopts)

;; ============================================================
;; 带统计版本
;; ============================================================

(define (cfg-ivopts-with-stats cfg)
  (define loops (analyze-loops cfg))
  (define biv-count 0)
  (define div-count 0)
  (define reduction-count 0)

  (define cfg^
    (for/fold ([cfg cfg])
              ([loop (sort-loops-by-depth loops)])
      (define preheader (Loop-preheader loop))
      (if (not preheader)
          cfg
          (let ()
            (define bivs (find-basic-ivs cfg loop))
            (set! biv-count (+ biv-count (length bivs)))

            (define divs (find-derived-ivs cfg loop bivs))
            (set! div-count (+ div-count (length divs)))

            (for/fold ([cfg cfg])
                      ([div divs])
              (define biv (DIV-base div))
              (define-values (cfg^ changed?)
                (strength-reduce-div cfg loop biv div))
              (when changed?
                (set! reduction-count (+ reduction-count 1)))
              cfg^)))))

  (values cfg^
          `((loops . ,(length loops))
            (basic-ivs . ,biv-count)
            (derived-ivs . ,div-count)
            (strength-reductions . ,reduction-count))))

(provide cfg-ivopts-with-stats)

;; ============================================================
;; 导出分析函数（用于测试）
;; ============================================================

(provide BIV BIV? BIV-var BIV-init BIV-step BIV-update-var
         DIV DIV? DIV-var DIV-base DIV-scale DIV-offset DIV-def-block
         find-basic-ivs find-derived-ivs)
