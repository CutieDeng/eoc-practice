#lang racket/base

;; ============================================================
;; CFG Optimization: Forward Propagation
;; ============================================================
;;
;; 前向传播优化
;;
;; 将单次使用的表达式传播到使用点，实现操作合并和简化：
;;   1. 单次使用内联：只使用一次的定义可以内联到使用点
;;   2. 操作合并：连续操作可以合并为单一操作
;;   3. 模式简化：识别并简化特定操作模式
;;
;; 优化模式示例：
;;   - (a + b) + c 其中 (a+b) 单次使用 → 可重结合
;;   - (a << n) >> n → a & mask (位操作简化)
;;   - neg(neg(a)) → a
;;   - not(not(a)) → a
;;   - (a - b) + b → a
;;   - (a + b) - b → a
;;
;; 参考：GCC tree-ssa-forwprop.cc
;; ============================================================

(require racket/match racket/list racket/dict racket/set)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")

;; ============================================================
;; 使用计数分析
;; ============================================================

;; 统计每个变量的使用次数
(define (count-uses cfg)
  (define use-count (ordl-make-empty var-id-compare))

  (define (count-in-datum datum m)
    (cond
      [(VarId? datum)
       (dict-update m datum add1 0)]
      [(list? datum)
       (for/fold ([m m]) ([d datum])
         (count-in-datum d m))]
      [else m]))

  (for*/fold ([m use-count])
             ([bid (cfg-all-block-ids cfg)]
              [block (in-value (cfg-get-block cfg bid))]
              #:when block)
    (define m1
      (for/fold ([m m])
                ([insn (CfgBlock-insns block)])
        (if (VfInsn? insn)
            (count-in-datum (VfInsn-inputs insn) m)
            m)))
    ;; PHI 节点中的使用
    (define m2
      (for/fold ([m m1])
                ([phi (CfgBlock-phis block)])
        (for/fold ([m m])
                  ([src (PhiInsn-sources phi)])
          (count-in-datum (cdr src) m))))
    ;; terminator 中的使用
    (count-in-datum (terminator-uses (CfgBlock-terminator block)) m2)))

;; 检查变量是否只被使用一次
(define (single-use? use-count var)
  (= 1 (dict-ref use-count var 0)))

;; ============================================================
;; 定义映射
;; ============================================================

;; 存储变量到其定义指令的映射
(define (build-def-map cfg)
  (for*/fold ([def-map (ordl-make-empty var-id-compare)])
             ([bid (cfg-all-block-ids cfg)]
              [block (in-value (cfg-get-block cfg bid))]
              #:when block
              [insn (CfgBlock-insns block)]
              #:when (VfInsn? insn)
              [out (VfInsn-outputs insn)])
    (dict-set def-map out insn)))

;; ============================================================
;; 操作合并模式
;; ============================================================

;; 尝试合并两个操作
;; 返回合并后的 (op inputs) 或 #f
(define (try-combine-ops outer-op outer-inputs inner-insn inner-var)
  (match inner-insn
    [(VfInsn inner-op inner-inputs (list inner-out) _ _)
     (cond
       ;; 双重否定: not(not(a)) → a
       [(and (eq? outer-op 'not) (eq? inner-op 'not))
        (match inner-inputs
          [(list a) (list 'copy (list a))]
          [_ #f])]

       ;; 双重取负: neg(neg(a)) → a
       [(and (eq? outer-op 'neg) (eq? inner-op 'neg))
        (match inner-inputs
          [(list a) (list 'copy (list a))]
          [_ #f])]

       ;; (a + b) - b → a
       ;; outer: (inner-var, x) where inner-var = a + b
       ;; We want: if x == b (second operand of add), result is a
       [(and (eq? outer-op 'sub) (eq? inner-op 'add))
        (match inner-inputs
          [(list a b)
           (match outer-inputs
             [(list (== inner-var) (== b)) (list 'copy (list a))]
             [(list (== inner-var) (== a)) (list 'copy (list b))]
             [_ #f])]
          [_ #f])]

       ;; (a - b) + b → a
       ;; outer: (inner-var, x) or (x, inner-var) where inner-var = a - b
       ;; We want: if x == b, result is a
       [(and (eq? outer-op 'add) (eq? inner-op 'sub))
        (match inner-inputs
          [(list a b)
           (match outer-inputs
             [(list (== inner-var) (== b)) (list 'copy (list a))]
             [(list (== b) (== inner-var)) (list 'copy (list a))]
             [_ #f])]
          [_ #f])]

       ;; (a << n) >> n → a & ((1 << (bits - n)) - 1) for unsigned
       ;; 这需要知道位宽，暂时跳过

       ;; (a * c1) * c2 → a * (c1 * c2)
       [(and (eq? outer-op 'mul) (eq? inner-op 'mul))
        (match* (outer-inputs inner-inputs)
          [((list (== inner-var) (? integer? c2)) (list a (? integer? c1)))
           (list 'mul (list a (* c1 c2)))]
          [((list (? integer? c2) (== inner-var)) (list a (? integer? c1)))
           (list 'mul (list a (* c1 c2)))]
          [((list (== inner-var) (? integer? c2)) (list (? integer? c1) a))
           (list 'mul (list a (* c1 c2)))]
          [(_ _) #f])]

       ;; (a + c1) + c2 → a + (c1 + c2)
       [(and (eq? outer-op 'add) (eq? inner-op 'add))
        (match* (outer-inputs inner-inputs)
          [((list (== inner-var) (? integer? c2)) (list a (? integer? c1)))
           (list 'add (list a (+ c1 c2)))]
          [((list (? integer? c2) (== inner-var)) (list a (? integer? c1)))
           (list 'add (list a (+ c1 c2)))]
          [((list (== inner-var) (? integer? c2)) (list (? integer? c1) a))
           (list 'add (list a (+ c1 c2)))]
          [(_ _) #f])]

       ;; (a - c1) - c2 → a - (c1 + c2)
       [(and (eq? outer-op 'sub) (eq? inner-op 'sub))
        (match* (outer-inputs inner-inputs)
          [((list (== inner-var) (? integer? c2)) (list a (? integer? c1)))
           (list 'sub (list a (+ c1 c2)))]
          [(_ _) #f])]

       ;; (a << c1) << c2 → a << (c1 + c2)
       [(and (eq? outer-op 'shl) (eq? inner-op 'shl))
        (match* (outer-inputs inner-inputs)
          [((list (== inner-var) (? integer? c2)) (list a (? integer? c1)))
           (list 'shl (list a (+ c1 c2)))]
          [(_ _) #f])]

       ;; (a >> c1) >> c2 → a >> (c1 + c2) (算术右移)
       [(and (eq? outer-op 'shr) (eq? inner-op 'shr))
        (match* (outer-inputs inner-inputs)
          [((list (== inner-var) (? integer? c2)) (list a (? integer? c1)))
           (list 'shr (list a (+ c1 c2)))]
          [(_ _) #f])]

       ;; (a >>> c1) >>> c2 → a >>> (c1 + c2) (逻辑右移)
       [(and (eq? outer-op 'ushr) (eq? inner-op 'ushr))
        (match* (outer-inputs inner-inputs)
          [((list (== inner-var) (? integer? c2)) (list a (? integer? c1)))
           (list 'ushr (list a (+ c1 c2)))]
          [(_ _) #f])]

       ;; (a & c1) & c2 → a & (c1 & c2)
       [(and (eq? outer-op 'and) (eq? inner-op 'and))
        (match* (outer-inputs inner-inputs)
          [((list (== inner-var) (? integer? c2)) (list a (? integer? c1)))
           (list 'and (list a (bitwise-and c1 c2)))]
          [((list (? integer? c2) (== inner-var)) (list a (? integer? c1)))
           (list 'and (list a (bitwise-and c1 c2)))]
          [((list (== inner-var) (? integer? c2)) (list (? integer? c1) a))
           (list 'and (list a (bitwise-and c1 c2)))]
          [(_ _) #f])]

       ;; (a | c1) | c2 → a | (c1 | c2)
       [(and (eq? outer-op 'or) (eq? inner-op 'or))
        (match* (outer-inputs inner-inputs)
          [((list (== inner-var) (? integer? c2)) (list a (? integer? c1)))
           (list 'or (list a (bitwise-ior c1 c2)))]
          [((list (? integer? c2) (== inner-var)) (list a (? integer? c1)))
           (list 'or (list a (bitwise-ior c1 c2)))]
          [((list (== inner-var) (? integer? c2)) (list (? integer? c1) a))
           (list 'or (list a (bitwise-ior c1 c2)))]
          [(_ _) #f])]

       ;; (a ^ c1) ^ c2 → a ^ (c1 ^ c2)
       [(and (eq? outer-op 'xor) (eq? inner-op 'xor))
        (match* (outer-inputs inner-inputs)
          [((list (== inner-var) (? integer? c2)) (list a (? integer? c1)))
           (list 'xor (list a (bitwise-xor c1 c2)))]
          [((list (? integer? c2) (== inner-var)) (list a (? integer? c1)))
           (list 'xor (list a (bitwise-xor c1 c2)))]
          [((list (== inner-var) (? integer? c2)) (list (? integer? c1) a))
           (list 'xor (list a (bitwise-xor c1 c2)))]
          [(_ _) #f])]

       [else #f])]
    [_ #f]))

;; ============================================================
;; 比较操作简化
;; ============================================================

;; 尝试简化比较操作
(define (try-simplify-comparison op inputs def-map use-count)
  (match inputs
    [(list (? VarId? v) rhs)
     (define inner (dict-ref def-map v #f))
     (and inner
          (single-use? use-count v)
          (try-combine-comparison op v rhs inner))]
    [(list lhs (? VarId? v))
     (define inner (dict-ref def-map v #f))
     (and inner
          (single-use? use-count v)
          (try-combine-comparison-rhs op lhs v inner))]
    [_ #f]))

(define (try-combine-comparison op outer-var rhs inner)
  (match inner
    ;; cmp (a + c1), c2 → cmp a, (c2 - c1)
    [(VfInsn 'add (list a (? integer? c1)) _ _ _)
     (and (integer? rhs)
          (list op (list a (- rhs c1))))]
    ;; cmp (a - c1), c2 → cmp a, (c2 + c1)
    [(VfInsn 'sub (list a (? integer? c1)) _ _ _)
     (and (integer? rhs)
          (list op (list a (+ rhs c1))))]
    [_ #f]))

(define (try-combine-comparison-rhs op lhs outer-var inner)
  ;; 类似但处理右操作数
  #f)

;; ============================================================
;; 指令重写
;; ============================================================

;; 尝试对单条指令进行前向传播优化
(define (try-forward-prop-insn insn def-map use-count)
  (match insn
    [(VfInsn op inputs outputs info id)
     ;; 检查输入中是否有单次使用的变量可以进行操作合并
     (define result
       (for/or ([input inputs]
                [idx (in-naturals)])
         (and (VarId? input)
              (single-use? use-count input)
              (let ([inner (dict-ref def-map input #f)])
                (and inner
                     (let ([combined (try-combine-ops op inputs inner input)])
                       (and combined
                            (cons input combined))))))))

     (if result
         (let ()
           (match-define (cons replaced-var (list new-op new-inputs)) result)
           (values (VfInsn new-op new-inputs outputs info id) #t))
         (values insn #f))]
    [_ (values insn #f)]))

;; ============================================================
;; copy 指令处理
;; ============================================================

;; 在一个块中处理 copy 指令（由合并产生）
(define (eliminate-copies insns)
  (define copy-map (make-hash))

  ;; 第一遍：收集 copy 映射
  (for ([insn insns])
    (match insn
      [(VfInsn 'copy (list src) (list dst) _ _)
       (hash-set! copy-map dst src)]
      [_ (void)]))

  ;; 如果没有 copy，直接返回
  (if (hash-empty? copy-map)
      insns
      ;; 第二遍：替换使用并过滤 copy
      (for/list ([insn insns]
                 #:unless (match insn
                            [(VfInsn 'copy _ _ _ _) #t]
                            [_ #f]))
        (match insn
          [(VfInsn op inputs outputs info id)
           (define (subst v)
             (if (VarId? v)
                 (hash-ref copy-map v v)
                 v))
           (define (subst-rec datum)
             (cond
               [(VarId? datum) (subst datum)]
               [(list? datum) (map subst-rec datum)]
               [else datum]))
           (VfInsn op (subst-rec inputs) outputs info id)]
          [_ insn]))))

;; ============================================================
;; Pass 主体
;; ============================================================

;; 对单个块执行前向传播
(define (forward-prop-block block def-map use-count)
  (define-values (new-insns changed?)
    (for/fold ([acc '()] [changed #f])
              ([insn (CfgBlock-insns block)])
      (define-values (new-insn insn-changed?)
        (try-forward-prop-insn insn def-map use-count))
      (values (cons new-insn acc) (or changed insn-changed?))))

  ;; 处理生成的 copy 指令
  (define final-insns (eliminate-copies (reverse new-insns)))

  (values (struct-copy CfgBlock block [insns final-insns])
          changed?))

;; 前向传播主入口
(define (cfg-forward-prop cfg)
  ;; 构建分析信息
  (define use-count (count-uses cfg))
  (define def-map (build-def-map cfg))

  ;; 对所有块执行前向传播
  (define-values (cfg^ any-changed?)
    (for/fold ([cfg cfg] [changed #f])
              ([bid (cfg-all-block-ids cfg)])
      (define block (cfg-get-block cfg bid))
      (if (not block)
          (values cfg changed)
          (let-values ([(new-block block-changed?)
                        (forward-prop-block block def-map use-count)])
            (values (if block-changed?
                       (cfg-set-block cfg new-block)
                       cfg)
                    (or changed block-changed?))))))

  cfg^)

(provide cfg-forward-prop)

;; ============================================================
;; 迭代到不动点
;; ============================================================

(define (cfg-forward-prop-fixpoint cfg [max-iter 10])
  (let loop ([cfg cfg] [i 0])
    (when (>= i max-iter)
      (error 'cfg-forward-prop-fixpoint "Did not converge in ~a iterations" max-iter))
    (define use-count (count-uses cfg))
    (define def-map (build-def-map cfg))
    (define-values (cfg^ changed?)
      (for/fold ([cfg cfg] [changed #f])
                ([bid (cfg-all-block-ids cfg)])
        (define block (cfg-get-block cfg bid))
        (if (not block)
            (values cfg changed)
            (let-values ([(new-block block-changed?)
                          (forward-prop-block block def-map use-count)])
              (values (if block-changed?
                         (cfg-set-block cfg new-block)
                         cfg)
                      (or changed block-changed?))))))
    (if changed?
        (loop cfg^ (+ i 1))
        cfg^)))

(provide cfg-forward-prop-fixpoint)

;; ============================================================
;; 带统计版本
;; ============================================================

(define (cfg-forward-prop-with-stats cfg)
  (define (count-insns c)
    (for/sum ([bid (cfg-all-block-ids c)])
      (define block (cfg-get-block c bid))
      (if block (length (CfgBlock-insns block)) 0)))

  (define before (count-insns cfg))
  (define cfg^ (cfg-forward-prop cfg))
  (define after (count-insns cfg^))

  (values cfg^
          `((before . ,before)
            (after . ,after)
            (combined . ,(- before after)))))

(provide cfg-forward-prop-with-stats)
