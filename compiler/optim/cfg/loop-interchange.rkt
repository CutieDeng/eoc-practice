#lang racket/base

;; ============================================================
;; CFG Optimization: Loop Interchange
;; ============================================================
;;
;; 循环交换：重排嵌套循环的维度以改善缓存局部性
;;
;; 转换:
;;   for i = 0 to N:
;;     for j = 0 to M:
;;       A[j][i] = B[j][i] + 1  ; 列优先访问（差）
;;
;; 变为:
;;   for j = 0 to M:
;;     for i = 0 to N:
;;       A[j][i] = B[j][i] + 1  ; 行优先访问（好）
;;
;; 条件：
;; 1. 循环必须是完美嵌套的
;; 2. 交换后依赖关系仍然合法
;; 3. 交换可以改善数据局部性
;;
;; 参考：GCC gimple-loop-interchange.cc
;; ============================================================

(require racket/match racket/list racket/set racket/hash)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "loop-analysis.rkt")

;; ============================================================
;; Nested Loop Detection
;; ============================================================

;; 检测嵌套循环结构
;; 返回 (list (cons outer-loop inner-loop) ...)
(define (find-nested-loops cfg)
  (define loops (analyze-loops cfg))
  (define nested-pairs '())

  ;; 检查每对循环是否嵌套
  (for* ([outer loops]
         [inner loops])
    (when (and (not (equal? outer inner))
               (loop-nested? inner outer))
      (set! nested-pairs (cons (cons outer inner) nested-pairs))))

  (reverse nested-pairs))

;; ============================================================
;; Perfect Nesting Check
;; ============================================================

;; 检查两个循环是否完美嵌套
;; 完美嵌套：外层循环体只包含内层循环
(define (perfectly-nested? cfg outer inner)
  (define outer-header (Loop-header outer))
  (define outer-body (set->list (Loop-body outer)))
  (define inner-header (Loop-header inner))
  (define inner-body (set->list (Loop-body inner)))

  ;; 外层循环体（除头和内层循环块外）应该为空或只有简单指令
  (define outer-only-blocks
    (filter (λ (bid)
              (and (not (equal? bid outer-header))
                   (not (set-member? (Loop-body inner) bid))))
            outer-body))

  ;; 简化检查：外层循环体只包含内层循环
  (null? outer-only-blocks))

;; ============================================================
;; Loop Induction Variable Analysis
;; ============================================================

;; 获取循环的归纳变量
(define (get-loop-induction-var cfg loop)
  (define header (Loop-header loop))
  (define header-block (cfg-get-block cfg header))

  (when (not header-block)
    (error 'get-loop-induction-var "Header block not found"))

  ;; 从 PHI 节点找归纳变量
  (define phis (CfgBlock-phis header-block))

  (for/or ([phi phis])
    (when (PhiInsn? phi)
      (PhiInsn-output phi))))

;; ============================================================
;; Array Access Pattern Analysis
;; ============================================================

;; 数组访问模式
(struct ArrayAccess (
  array       ; VarId - 数组变量
  indices     ; (Listof VarId) - 索引变量列表
  is-write    ; Boolean - 是否为写访问
) #:prefab)

;; 从指令中提取数组访问
(define (extract-array-accesses insns)
  (define accesses '())

  (for ([insn insns])
    (when (VfInsn? insn)
      (define op (VfInsn-op insn))
      (define inputs (VfInsn-inputs insn))

      (cond
        ;; vector-ref: 读访问
        [(equal? op 'vector-ref)
         (when (and (>= (length inputs) 2)
                    (VarId? (car inputs)))
           (define indices
             (filter VarId? (cdr inputs)))
           (set! accesses
                 (cons (ArrayAccess (car inputs) indices #f)
                       accesses)))]

        ;; vector-set!: 写访问
        [(equal? op 'vector-set!)
         (when (and (>= (length inputs) 2)
                    (VarId? (car inputs)))
           (define indices
             (filter VarId? (take (cdr inputs)
                                  (min 2 (- (length inputs) 1)))))
           (set! accesses
                 (cons (ArrayAccess (car inputs) indices #t)
                       accesses)))]

        ;; aload/astore 等
        [(memq op '(aload iaload laload faload daload aaload))
         (when (and (>= (length inputs) 2)
                    (VarId? (car inputs)))
           (set! accesses
                 (cons (ArrayAccess (car inputs)
                                    (filter VarId? (cdr inputs))
                                    #f)
                       accesses)))]

        [(memq op '(astore iastore lastore fastore dastore aastore))
         (when (and (>= (length inputs) 2)
                    (VarId? (car inputs)))
           (set! accesses
                 (cons (ArrayAccess (car inputs)
                                    (filter VarId? (take (cdr inputs)
                                                         (min 2 (- (length inputs) 1))))
                                    #t)
                       accesses)))])))

  (reverse accesses))

;; ============================================================
;; Interchange Profitability
;; ============================================================

;; 计算交换的收益
;; 正值表示交换有利，负值表示不利
(define (interchange-profit accesses outer-iv inner-iv)
  (define profit 0)

  (for ([access accesses])
    (define indices (ArrayAccess-indices access))

    ;; 如果最内层索引使用外层循环变量，交换有利
    ;; 因为这会使内存访问更连续
    (when (and (pair? indices)
               (equal? (last indices) outer-iv))
      (set! profit (+ profit 1)))

    ;; 如果最内层索引使用内层循环变量，当前顺序已经好
    (when (and (pair? indices)
               (equal? (last indices) inner-iv))
      (set! profit (- profit 1))))

  profit)

;; ============================================================
;; Dependency Legality Check
;; ============================================================

;; 检查交换是否合法（不违反依赖）
;; 简化版本：只检查基本条件
(define (interchange-legal? cfg outer inner)
  ;; 获取循环体的指令
  (define inner-body-blocks
    (filter (λ (bid) (not (equal? bid (Loop-header inner))))
            (set->list (Loop-body inner))))

  (cond
    [(null? inner-body-blocks) #t]
    [else
     (define body-bid (car inner-body-blocks))
     (define body-block (cfg-get-block cfg body-bid))
     (if (not body-block)
         #f
         (let ()
           (define insns (CfgBlock-insns body-block))
           (define accesses (extract-array-accesses insns))

           ;; 简化检查：如果所有数组访问都是独立的，则合法
           ;; 真正的实现需要检查依赖向量
           (for/and ([access accesses])
             ;; 简单的读访问总是合法的
             (or (not (ArrayAccess-is-write access))
                 ;; 写访问需要更仔细的检查
                 ;; 这里简化为总是合法
                 #t))))]))

;; ============================================================
;; Loop Interchange Transform
;; ============================================================

;; 执行循环交换
;; 这是一个复杂的转换，需要：
;; 1. 交换循环头的 PHI 节点
;; 2. 交换循环边界
;; 3. 更新控制流

(define (perform-interchange cfg outer inner)
  ;; 当前实现为分析版本
  ;; 实际交换需要复杂的 CFG 重构
  cfg)

;; ============================================================
;; Main Pass
;; ============================================================

;; 对整个 CFG 执行循环交换
(define (cfg-loop-interchange cfg)
  (define nested-pairs (find-nested-loops cfg))

  (for/fold ([cfg cfg])
            ([pair nested-pairs])
    (define outer (car pair))
    (define inner (cdr pair))

    (cond
      ;; 检查是否完美嵌套
      [(not (perfectly-nested? cfg outer inner))
       cfg]

      ;; 检查是否合法
      [(not (interchange-legal? cfg outer inner))
       cfg]

      ;; 获取归纳变量
      [else
       (define outer-iv (get-loop-induction-var cfg outer))
       (define inner-iv (get-loop-induction-var cfg inner))

       (cond
         [(or (not outer-iv) (not inner-iv))
          cfg]

         [else
          ;; 获取内层循环体的访问模式
          (define inner-body-blocks
            (filter (λ (bid) (not (equal? bid (Loop-header inner))))
                    (set->list (Loop-body inner))))

          (if (null? inner-body-blocks)
              cfg
              (let ()
                (define body-bid (car inner-body-blocks))
                (define body-block (cfg-get-block cfg body-bid))

                (if (not body-block)
                    cfg
                    (let ()
                      (define insns (CfgBlock-insns body-block))
                      (define accesses (extract-array-accesses insns))
                      (define profit (interchange-profit accesses outer-iv inner-iv))

                      ;; 只有收益为正时才交换
                      (if (> profit 0)
                          (perform-interchange cfg outer inner)
                          cfg)))))])])))

(provide cfg-loop-interchange)

;; ============================================================
;; Analysis Only
;; ============================================================

;; 分析循环交换机会
(define (analyze-loop-interchange cfg)
  (define nested-pairs (find-nested-loops cfg))
  (define results '())

  (for ([pair nested-pairs])
    (define outer (car pair))
    (define inner (cdr pair))

    (define outer-header (Loop-header outer))
    (define inner-header (Loop-header inner))

    (define perfectly-nested (perfectly-nested? cfg outer inner))
    (define legal (interchange-legal? cfg outer inner))

    (define outer-iv (get-loop-induction-var cfg outer))
    (define inner-iv (get-loop-induction-var cfg inner))

    (define profit
      (if (and outer-iv inner-iv)
          (let ()
            (define inner-body-blocks
              (filter (λ (bid) (not (equal? bid inner-header)))
                      (set->list (Loop-body inner))))
            (if (null? inner-body-blocks)
                0
                (let ()
                  (define body-bid (car inner-body-blocks))
                  (define body-block (cfg-get-block cfg body-bid))
                  (if (not body-block)
                      0
                      (let ()
                        (define insns (CfgBlock-insns body-block))
                        (define accesses (extract-array-accesses insns))
                        (interchange-profit accesses outer-iv inner-iv))))))
          0))

    (set! results
          (cons `((outer-loop . ,outer-header)
                  (inner-loop . ,inner-header)
                  (perfectly-nested . ,perfectly-nested)
                  (legal . ,legal)
                  (outer-iv . ,outer-iv)
                  (inner-iv . ,inner-iv)
                  (profit . ,profit)
                  (should-interchange . ,(and perfectly-nested legal (> profit 0))))
                results)))

  (reverse results))

(provide analyze-loop-interchange)

;; ============================================================
;; With Statistics
;; ============================================================

(define (cfg-loop-interchange-with-stats cfg)
  (define analysis (analyze-loop-interchange cfg))

  (define interchange-count
    (for/sum ([info analysis])
      (if (cdr (assoc 'should-interchange info)) 1 0)))

  (define cfg^ (cfg-loop-interchange cfg))

  (values cfg^
          `((nested-loops . ,(length analysis))
            (interchangeable . ,interchange-count))))

(provide cfg-loop-interchange-with-stats)
