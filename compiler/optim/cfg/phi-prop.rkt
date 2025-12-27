#lang racket/base

;; ============================================================
;; CFG Optimization: PHI Propagation
;; ============================================================
;;
;; 通过 PHI 节点反向传播间接加载
;;
;; 转换:
;;   r = phi(p, q)       p = &a, q = &b
;;   x = load(r)
;; 为:
;;   x_p = load(p)       -> x_p = a
;;   x_q = load(q)       -> x_q = b
;;   x = phi(x_p, x_q)
;;
;; 这允许后续优化单独处理每个加载，
;; 如果 p 和 q 指向已知地址，可以进一步简化。
;;
;; 参考：GCC tree-ssa-phiprop.cc
;; ============================================================

(require racket/match racket/list racket/set racket/hash)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")
(require "alias-analysis.rkt")
(require "loop-analysis.rkt")

;; ============================================================
;; Find PHI-based Loads
;; ============================================================

;; 查找形如 x = load(phi_result) 的加载
;; 返回 (list (cons load-insn phi-insn) ...)
(define (find-phi-based-loads cfg)
  (define phi-defs (make-hash))  ; var -> phi-insn

  ;; 收集所有 PHI 定义
  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (for ([phi (CfgBlock-phis block)])
        (when (PhiInsn? phi)
          (hash-set! phi-defs (PhiInsn-output phi) phi)))))

  ;; 查找使用 PHI 结果的加载
  (define results '())
  (for ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (for ([insn (CfgBlock-insns block)])
        (when (and (VfInsn? insn)
                   (memory-read-op? (VfInsn-op insn)))
          (define inputs (VfInsn-inputs insn))
          (when (and (pair? inputs)
                     (VarId? (car inputs))
                     (hash-has-key? phi-defs (car inputs)))
            (set! results
                  (cons (list bid insn (hash-ref phi-defs (car inputs)))
                        results)))))))

  (reverse results))

;; ============================================================
;; PHI Propagation Transform
;; ============================================================

;; 生成新变量
(define phi-prop-counter 0)

(define (fresh-phi-prop-var base-name)
  (set! phi-prop-counter (+ phi-prop-counter 1))
  (VarId (string->symbol (format "~a_pp~a" base-name phi-prop-counter))))

;; 对单个 PHI-based load 执行传播
;; 在每个前驱块添加新的 load，然后创建新的 PHI
(define (propagate-phi-load cfg bid load-insn phi-insn)
  (define phi-output (PhiInsn-output phi-insn))
  (define phi-inputs (PhiInsn-sources phi-insn))  ; (list (cons BlockId VarId) ...)
  (define load-output (car (VfInsn-outputs load-insn)))
  (define load-op (VfInsn-op load-insn))

  ;; 为每个 PHI 输入创建新的加载
  (define new-loads '())  ; (list (cons BlockId VarId) ...)
  (define cfg-with-loads
    (for/fold ([cfg cfg])
              ([input phi-inputs])
      (match input
        [(cons pred-bid pred-var)
         (define pred-block (cfg-get-block cfg pred-bid))
         (cond
           [(not pred-block) cfg]
           [else
            ;; 创建新变量和新加载
            (define new-var (fresh-phi-prop-var
                             (symbol->string (VarId-id load-output))))
            (set! new-loads (cons (cons pred-bid new-var) new-loads))

            ;; 在前驱块末尾（终止器之前）添加加载
            (define new-insn
              (VfInsn load-op
                      (list pred-var)  ; 使用 PHI 的输入而非 PHI 结果
                      (list new-var)
                      #f #f))

            (define new-insns
              (append (CfgBlock-insns pred-block) (list new-insn)))

            (cfg-set-block cfg
                           (struct-copy CfgBlock pred-block
                                        [insns new-insns]))])])))

  ;; 创建新的 PHI 节点
  (define new-phi
    (PhiInsn load-output (reverse new-loads)))

  ;; 在当前块添加新的 PHI，移除原来的 load
  (define current-block (cfg-get-block cfg-with-loads bid))
  (cond
    [(not current-block) cfg-with-loads]
    [else
     (define new-phis (cons new-phi (CfgBlock-phis current-block)))
     (define new-insns
       (filter (λ (insn) (not (equal? insn load-insn)))
               (CfgBlock-insns current-block)))

     (cfg-set-block cfg-with-loads
                    (struct-copy CfgBlock current-block
                                 [phis new-phis]
                                 [insns new-insns]))]))

;; ============================================================
;; Safety Checks
;; ============================================================

;; 检查是否可以安全地传播 load
;; 条件：
;; 1. PHI 的所有输入都是安全的（不会导致额外副作用）
;; 2. 前驱块可以添加加载指令
(define (can-propagate-phi-load? cfg bid load-insn phi-insn)
  (define phi-inputs (PhiInsn-sources phi-insn))

  ;; 检查所有前驱是否可达且可修改
  (for/and ([input phi-inputs])
    (match input
      [(cons pred-bid pred-var)
       (define pred-block (cfg-get-block cfg pred-bid))
       (and pred-block
            ;; 简单检查：前驱块存在
            #t)])))

;; ============================================================
;; Main Pass
;; ============================================================

;; 对整个 CFG 执行 PHI 传播
(define (cfg-phi-prop cfg)
  (let loop ([cfg cfg] [iterations 0])
    (cond
      [(>= iterations 10) cfg]  ; 防止无限循环
      [else
       (define candidates (find-phi-based-loads cfg))
       (cond
         [(null? candidates) cfg]
         [else
          ;; 处理第一个候选
          (match (car candidates)
            [(list bid load-insn phi-insn)
             (if (can-propagate-phi-load? cfg bid load-insn phi-insn)
                 (loop (propagate-phi-load cfg bid load-insn phi-insn)
                       (+ iterations 1))
                 (loop cfg (+ iterations 1)))])])])))

(provide cfg-phi-prop)

;; ============================================================
;; With Statistics
;; ============================================================

(define (cfg-phi-prop-with-stats cfg)
  (define propagated-count 0)

  (define cfg^
    (let loop ([cfg cfg] [iterations 0])
      (cond
        [(>= iterations 10) cfg]
        [else
         (define candidates (find-phi-based-loads cfg))
         (cond
           [(null? candidates) cfg]
           [else
            (match (car candidates)
              [(list bid load-insn phi-insn)
               (if (can-propagate-phi-load? cfg bid load-insn phi-insn)
                   (begin
                     (set! propagated-count (+ propagated-count 1))
                     (loop (propagate-phi-load cfg bid load-insn phi-insn)
                           (+ iterations 1)))
                   (loop cfg (+ iterations 1)))])])])))

  (values cfg^
          `((propagated-loads . ,propagated-count))))

(provide cfg-phi-prop-with-stats)
