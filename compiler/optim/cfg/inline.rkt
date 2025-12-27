#lang racket/base

;; ============================================================
;; CFG Optimization: Function Inlining
;; ============================================================
;;
;; 函数内联：将函数调用替换为函数体
;;
;; 内联的好处：
;; 1. 消除调用开销
;; 2. 启用更多优化机会（常量传播、死代码消除等）
;; 3. 改善指令缓存局部性
;;
;; 内联的代价：
;; 1. 代码膨胀
;; 2. 编译时间增加
;; 3. 可能增加寄存器压力
;;
;; 参考：GCC tree-inline.cc
;; ============================================================

(require racket/match racket/list racket/set racket/hash)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")

;; ============================================================
;; Inline Candidate Detection
;; ============================================================

;; 内联候选信息
(struct InlineCandidate (
  call-site    ; (cons BlockId VfInsn) - 调用位置
  callee       ; Symbol - 被调用函数名
  args         ; (Listof Any) - 参数
) #:prefab)

;; 从 CFG 中找到所有调用指令
(define (find-call-sites cfg)
  (define candidates '())

  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (for ([insn (CfgBlock-insns block)])
        (when (VfInsn? insn)
          (define op (VfInsn-op insn))
          (when (memq op '(call invoke invokevirtual invokestatic
                           invokeinterface invokespecial))
            (define inputs (VfInsn-inputs insn))
            (when (and (pair? inputs) (symbol? (car inputs)))
              (set! candidates
                    (cons (InlineCandidate (cons bid insn)
                                           (car inputs)
                                           (cdr inputs))
                          candidates))))))))

  (reverse candidates))

;; ============================================================
;; Inlining Heuristics
;; ============================================================

;; 内联阈值
(define inline-threshold 50)  ; 最大指令数
(define always-inline-threshold 10)  ; 总是内联的小函数

;; 检查函数是否应该内联
(define (should-inline? callee-cfg call-count)
  (define insn-count (count-cfg-insns callee-cfg))

  (cond
    ;; 非常小的函数总是内联
    [(<= insn-count always-inline-threshold) #t]

    ;; 只被调用一次的函数内联
    [(= call-count 1) #t]

    ;; 根据大小决定
    [(<= insn-count inline-threshold) #t]

    ;; 太大的函数不内联
    [else #f]))

;; 计算 CFG 中的指令数
(define (count-cfg-insns cfg)
  (for/sum ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if block
        (length (CfgBlock-insns block))
        0)))

;; ============================================================
;; Inline Transform
;; ============================================================

;; 变量重命名计数器
(define inline-rename-counter 0)

;; 创建变量重命名映射
(define (create-rename-map callee-cfg prefix)
  (define rename-map (make-hash))

  (for ([bid (cfg-all-block-ids callee-cfg)])
    (define block (cfg-get-block callee-cfg bid))
    (when block
      ;; 重命名 PHI 输出
      (for ([phi (CfgBlock-phis block)])
        (when (PhiInsn? phi)
          (define old-var (PhiInsn-output phi))
          (set! inline-rename-counter (+ inline-rename-counter 1))
          (define new-var
            (VarId (string->symbol
                    (format "~a_~a_~a" prefix (VarId-id old-var)
                            inline-rename-counter))))
          (hash-set! rename-map old-var new-var)))

      ;; 重命名指令输出
      (for ([insn (CfgBlock-insns block)])
        (when (VfInsn? insn)
          (for ([out (VfInsn-outputs insn)])
            (set! inline-rename-counter (+ inline-rename-counter 1))
            (define new-var
              (VarId (string->symbol
                      (format "~a_~a_~a" prefix (VarId-id out)
                              inline-rename-counter))))
            (hash-set! rename-map out new-var))))))

  rename-map)

;; 重命名变量
(define (rename-var var rename-map)
  (if (VarId? var)
      (hash-ref rename-map var var)
      var))

;; 重命名指令中的变量
(define (rename-insn insn rename-map)
  (match insn
    [(VfInsn op inputs outputs info id)
     (VfInsn op
             (map (λ (v) (rename-var v rename-map)) inputs)
             (map (λ (v) (rename-var v rename-map)) outputs)
             info
             id)]
    [_ insn]))

;; ============================================================
;; Single Function Inlining
;; ============================================================

;; 执行单个函数的内联
;; caller-cfg: 调用者的 CFG
;; callee-cfg: 被调用者的 CFG
;; call-site: 调用位置
;; args: 调用参数
(define (inline-function caller-cfg callee-cfg call-site args)
  (define call-bid (car call-site))
  (define call-insn (cdr call-site))
  (define call-block (cfg-get-block caller-cfg call-bid))

  (when (not call-block)
    (error 'inline-function "Call block not found"))

  ;; 创建变量重命名映射
  (define rename-map (create-rename-map callee-cfg "inl"))

  ;; 绑定参数
  (define param-bindings
    (for/list ([arg args] [i (in-naturals)])
      (VfInsn 'copy (list arg)
              (list (VarId (string->symbol (format "param~a" i))))
              #f #f)))

  ;; 替换调用指令为内联的函数体
  ;; 简化版本：只处理单块函数
  (define callee-entry (Cfg-entry callee-cfg))
  (define callee-entry-block (cfg-get-block callee-cfg callee-entry))

  (cond
    [(not callee-entry-block)
     caller-cfg]

    [else
     ;; 获取内联的指令（重命名后）
     (define inlined-insns
       (for/list ([insn (CfgBlock-insns callee-entry-block)])
         (rename-insn insn rename-map)))

     ;; 替换调用指令
     (define new-insns
       (append-map
        (λ (insn)
          (if (equal? insn call-insn)
              (append param-bindings inlined-insns)
              (list insn)))
        (CfgBlock-insns call-block)))

     ;; 更新块
     (cfg-set-block caller-cfg
                    (struct-copy CfgBlock call-block
                                 [insns new-insns]))]))

;; ============================================================
;; Main Pass
;; ============================================================

;; 函数表：函数名 -> CFG
(define function-table (make-hash))

;; 注册函数
(define (register-function name cfg)
  (hash-set! function-table name cfg))

;; 获取函数
(define (get-function name)
  (hash-ref function-table name #f))

(provide register-function get-function find-call-sites InlineCandidate
         InlineCandidate-call-site InlineCandidate-callee InlineCandidate-args
         should-inline? count-cfg-insns)

;; 对 CFG 执行内联
(define (cfg-inline cfg)
  (define candidates (find-call-sites cfg))

  (for/fold ([cfg cfg])
            ([candidate candidates])
    (define callee-name (InlineCandidate-callee candidate))
    (define callee-cfg (get-function callee-name))

    (cond
      [(not callee-cfg)
       ;; 被调用函数不可用，跳过
       cfg]

      [(not (should-inline? callee-cfg 1))
       ;; 不应该内联，跳过
       cfg]

      [else
       ;; 执行内联
       (inline-function cfg callee-cfg
                        (InlineCandidate-call-site candidate)
                        (InlineCandidate-args candidate))])))

(provide cfg-inline)

;; ============================================================
;; Analysis Only
;; ============================================================

;; 分析内联机会
(define (analyze-inlining cfg)
  (define candidates (find-call-sites cfg))

  (for/list ([candidate candidates])
    (define callee-name (InlineCandidate-callee candidate))
    (define callee-cfg (get-function callee-name))

    `((call-site . ,(car (InlineCandidate-call-site candidate)))
      (callee . ,callee-name)
      (callee-available . ,(if callee-cfg #t #f))
      (callee-size . ,(if callee-cfg (count-cfg-insns callee-cfg) 'unknown))
      (should-inline . ,(if callee-cfg (should-inline? callee-cfg 1) #f)))))

(provide analyze-inlining)

;; ============================================================
;; With Statistics
;; ============================================================

(define (cfg-inline-with-stats cfg)
  (define candidates (find-call-sites cfg))
  (define before-insns (count-cfg-insns cfg))

  (define inlined-count 0)

  (define cfg^
    (for/fold ([cfg cfg])
              ([candidate candidates])
      (define callee-name (InlineCandidate-callee candidate))
      (define callee-cfg (get-function callee-name))

      (cond
        [(not callee-cfg) cfg]
        [(not (should-inline? callee-cfg 1)) cfg]
        [else
         (set! inlined-count (+ inlined-count 1))
         (inline-function cfg callee-cfg
                          (InlineCandidate-call-site candidate)
                          (InlineCandidate-args candidate))])))

  (define after-insns (count-cfg-insns cfg^))

  (values cfg^
          `((call-sites . ,(length candidates))
            (inlined . ,inlined-count)
            (insns-before . ,before-insns)
            (insns-after . ,after-insns))))

(provide cfg-inline-with-stats)
