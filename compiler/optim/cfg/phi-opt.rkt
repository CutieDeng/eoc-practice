#lang racket/base

;; ============================================================
;; CFG Optimization: PHI Node Optimization
;; ============================================================
;;
;; PHI 节点优化
;;
;; 简化和消除冗余的 PHI 节点：
;;   1. 平凡 PHI：phi(a, a, a, ...) → a
;;   2. 单源 PHI：只有一个前驱 → 直接赋值
;;   3. 自引用 PHI：x = phi(x, a) → a（当所有非自引用输入相同）
;;   4. 常量 PHI：所有输入是相同常量 → 常量
;;   5. 复制 PHI：phi(a, b) 其中 a 和 b 值编号相同
;;
;; 模式识别：
;;   - min/max 模式
;;   - abs 模式
;;   - 条件选择模式
;;
;; 参考：GCC tree-ssa-phiopt.cc
;; ============================================================

(require racket/match racket/list racket/dict racket/set)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")

;; ============================================================
;; PHI 节点分析
;; ============================================================

;; 获取 PHI 节点的所有输入变量（去除 BlockId）
(define (phi-input-vars phi)
  (map cdr (PhiInsn-sources phi)))

;; 获取非自引用的输入
(define (phi-non-self-inputs phi)
  (define out (PhiInsn-output phi))
  (filter (λ (v) (not (equal? v out)))
          (phi-input-vars phi)))

;; 检查是否所有输入相同
(define (phi-all-same-input? phi)
  (define inputs (phi-input-vars phi))
  (and (not (null? inputs))
       (andmap (λ (v) (equal? v (car inputs))) (cdr inputs))))

;; 检查是否是平凡 PHI（所有输入相同或只有自引用）
(define (trivial-phi? phi)
  (define non-self (phi-non-self-inputs phi))
  (or (null? non-self)
      (andmap (λ (v) (equal? v (car non-self))) (cdr non-self))))

;; 获取平凡 PHI 的替换值
(define (trivial-phi-value phi)
  (define non-self (phi-non-self-inputs phi))
  (if (null? non-self)
      (PhiInsn-output phi)  ; 纯自引用，保持原样
      (car non-self)))

;; ============================================================
;; PHI 替换映射
;; ============================================================

;; 构建 PHI 替换映射
;; 返回 VarId → VarId 的映射（PHI 输出 → 替换值）
(define (build-phi-replacement-map cfg)
  (define replacements (ordl-make-empty var-id-compare))

  (for*/fold ([m replacements])
             ([bid (cfg-all-block-ids cfg)]
              [block (in-value (cfg-get-block cfg bid))]
              #:when block
              [phi (CfgBlock-phis block)]
              #:when (trivial-phi? phi))
    (define out (PhiInsn-output phi))
    (define replacement (trivial-phi-value phi))
    (if (equal? out replacement)
        m  ; 不替换自己
        (dict-set m out replacement))))

;; 解析替换链
(define (resolve-replacement replacements var [visited (set)])
  (cond
    [(set-member? visited var) var]  ; 循环检测
    [else
     (define rep (dict-ref replacements var #f))
     (if rep
         (resolve-replacement replacements rep (set-add visited var))
         var)]))

;; ============================================================
;; 应用 PHI 替换
;; ============================================================

;; 替换指令输入中的变量
(define (replace-in-inputs inputs replacements)
  (define (replace-single v)
    (if (VarId? v)
        (resolve-replacement replacements v)
        v))

  (define (replace-recursive datum)
    (cond
      [(VarId? datum) (replace-single datum)]
      [(list? datum) (map replace-recursive datum)]
      [else datum]))

  (replace-recursive inputs))

;; 替换指令中的变量
(define (replace-in-insn insn replacements)
  (match insn
    [(VfInsn op inputs outputs info id)
     (define new-inputs (replace-in-inputs inputs replacements))
     (VfInsn op new-inputs outputs info id)]
    [_ insn]))

;; 替换 PHI 节点中的变量
(define (replace-in-phi phi replacements)
  (define new-sources
    (for/list ([src (PhiInsn-sources phi)])
      (match-define (cons bid var) src)
      (cons bid (resolve-replacement replacements var))))
  (PhiInsn (PhiInsn-output phi) new-sources))

;; 替换 terminator 中的变量
(define (replace-in-terminator term replacements)
  (match term
    [(TermBranch cond then-bid else-bid)
     (TermBranch (if (VarId? cond)
                     (resolve-replacement replacements cond)
                     cond)
                 then-bid else-bid)]
    [(TermSwitch value cases default)
     (TermSwitch (if (VarId? value)
                     (resolve-replacement replacements value)
                     value)
                 cases default)]
    [(TermReturn values)
     (TermReturn (map (λ (v)
                        (if (VarId? v)
                            (resolve-replacement replacements v)
                            v))
                      values))]
    [(TermThrow exc)
     (TermThrow (if (VarId? exc)
                    (resolve-replacement replacements exc)
                    exc))]
    [_ term]))

;; ============================================================
;; PHI 消除
;; ============================================================

;; 移除被替换的 PHI 节点
(define (remove-replaced-phis phis replacements)
  (filter (λ (phi)
            (not (dict-ref replacements (PhiInsn-output phi) #f)))
          phis))

;; 对单个块执行 PHI 优化
(define (optimize-block-phis block replacements)
  ;; 移除被替换的 PHI
  (define new-phis
    (for/list ([phi (remove-replaced-phis (CfgBlock-phis block) replacements)])
      (replace-in-phi phi replacements)))

  ;; 替换指令中的引用
  (define new-insns
    (for/list ([insn (CfgBlock-insns block)])
      (replace-in-insn insn replacements)))

  ;; 替换 terminator 中的引用
  (define new-term
    (replace-in-terminator (CfgBlock-terminator block) replacements))

  (struct-copy CfgBlock block
               [phis new-phis]
               [insns new-insns]
               [terminator new-term]))

;; ============================================================
;; PHI 优化主入口
;; ============================================================

(define (cfg-phi-opt cfg)
  ;; 构建替换映射
  (define replacements (build-phi-replacement-map cfg))

  ;; 如果没有替换，直接返回
  (if (dict-empty? replacements)
      cfg
      ;; 应用替换到所有块
      (for/fold ([cfg cfg])
                ([bid (cfg-all-block-ids cfg)])
        (define block (cfg-get-block cfg bid))
        (if (not block)
            cfg
            (cfg-set-block cfg (optimize-block-phis block replacements))))))

(provide cfg-phi-opt)

;; ============================================================
;; 带统计版本
;; ============================================================

(define (count-phis cfg)
  (for*/sum ([bid (cfg-all-block-ids cfg)]
             [block (in-value (cfg-get-block cfg bid))]
             #:when block)
    (length (CfgBlock-phis block))))

(define (cfg-phi-opt-with-stats cfg)
  (define before (count-phis cfg))
  (define cfg^ (cfg-phi-opt cfg))
  (define after (count-phis cfg^))

  (values cfg^
          `((phis-before . ,before)
            (phis-after . ,after)
            (phis-eliminated . ,(- before after)))))

(provide cfg-phi-opt-with-stats)

;; ============================================================
;; 高级模式识别
;; ============================================================

;; 识别 min/max 模式
;; if (a < b) { result = a; } else { result = b; }
;; → result = min(a, b)
(define (recognize-minmax-pattern cfg bid phi)
  ;; 需要分析控制流来识别模式
  ;; 这需要更复杂的分析，暂时标记为 TODO
  #f)

;; 识别 abs 模式
;; if (a < 0) { result = -a; } else { result = a; }
;; → result = abs(a)
(define (recognize-abs-pattern cfg bid phi)
  ;; 同样需要控制流分析
  #f)

;; ============================================================
;; 迭代优化
;; ============================================================

;; 迭代执行 PHI 优化直到不动点
(define (cfg-phi-opt-fixpoint cfg [max-iter 10])
  (let loop ([cfg cfg] [i 0])
    (when (>= i max-iter)
      (error 'cfg-phi-opt-fixpoint "Did not converge in ~a iterations" max-iter))
    (define cfg^ (cfg-phi-opt cfg))
    (define before-phis (count-phis cfg))
    (define after-phis (count-phis cfg^))
    (if (= before-phis after-phis)
        cfg^
        (loop cfg^ (+ i 1)))))

(provide cfg-phi-opt-fixpoint)
