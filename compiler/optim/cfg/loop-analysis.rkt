#lang racket/base

;; ============================================================
;; CFG Loop Analysis
;; ============================================================
;;
;; 循环分析：识别 CFG 中的循环结构
;;
;; 包含：
;;   1. 支配关系计算（Dominance）
;;   2. 后向边检测（Back Edges）
;;   3. 自然循环识别（Natural Loops）
;;
;; 参考：GCC cfgloop.cc, dominance.cc
;; ============================================================

(require racket/match racket/list racket/dict racket/set)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../cfg/raw.rkt")

;; ============================================================
;; CFG 图遍历辅助
;; ============================================================

;; 获取块的后继
(define (block-successors cfg bid)
  (define block (cfg-get-block cfg bid))
  (if (not block)
      '()
      (match (CfgBlock-terminator block)
        [(TermJump target) (list target)]
        [(TermBranch _ then-bid else-bid) (list then-bid else-bid)]
        [(TermSwitch _ cases default-bid)
         (cons default-bid (map cdr cases))]
        [(TermReturn _) '()]
        [(TermThrow _) '()]
        [(TermUnreachable) '()])))

;; 获取块的前驱
(define (compute-predecessors cfg)
  (define preds (make-hash))
  ;; 初始化所有块
  (for ([bid (cfg-all-block-ids cfg)])
    (hash-set! preds bid '()))
  ;; 填充前驱
  (for ([bid (cfg-all-block-ids cfg)])
    (for ([succ (block-successors cfg bid)])
      (hash-update! preds succ (λ (lst) (cons bid lst)) '())))
  preds)

;; ============================================================
;; 支配关系计算
;; ============================================================

;; 计算支配关系（使用迭代数据流算法）
;; 返回 BlockId → (set BlockId) 的映射
;; dom[b] = 支配 b 的所有块
(define (compute-dominators cfg)
  (define entry (cfg-get-entry cfg))
  (define all-blocks (cfg-all-block-ids cfg))
  (define all-blocks-set (list->set all-blocks))
  (define preds (compute-predecessors cfg))

  ;; 初始化
  (define dom (make-hash))
  (for ([bid all-blocks])
    (hash-set! dom bid
               (if (equal? bid entry)
                   (set bid)          ; 入口块只被自己支配
                   all-blocks-set)))  ; 其他块初始为全集

  ;; 迭代直到不动点
  (let loop ([changed #t])
    (when changed
      (define new-changed #f)
      (for ([bid all-blocks]
            #:when (not (equal? bid entry)))
        (define pred-list (hash-ref preds bid '()))
        (define new-dom
          (if (null? pred-list)
              (set bid)
              (set-add
               (apply set-intersect
                      (for/list ([p pred-list])
                        (hash-ref dom p (set))))
               bid)))
        (unless (equal? (hash-ref dom bid) new-dom)
          (set! new-changed #t)
          (hash-set! dom bid new-dom)))
      (loop new-changed)))

  dom)

;; 检查 a 是否支配 b
(define (dominates? dom a b)
  (set-member? (hash-ref dom b (set)) a))

;; 计算直接支配者（immediate dominator）
;; 返回 BlockId → BlockId 映射
(define (compute-idom cfg dom)
  (define entry (cfg-get-entry cfg))
  (define idom (make-hash))

  (for ([bid (cfg-all-block-ids cfg)]
        #:when (not (equal? bid entry)))
    (define dominators (set-remove (hash-ref dom bid (set)) bid))
    ;; idom 是最接近的支配者（支配 bid 但不支配其他支配者）
    (for ([d (in-set dominators)])
      (define is-idom
        (for/and ([other (in-set dominators)]
                  #:when (not (equal? other d)))
          (not (dominates? dom d other))))
      (when is-idom
        (hash-set! idom bid d))))

  idom)

;; ============================================================
;; 后向边检测
;; ============================================================

;; 后向边：从 b 到 a 的边，其中 a 支配 b
;; 返回 (list (cons source target) ...)
(define (find-back-edges cfg dom)
  (for*/list ([bid (cfg-all-block-ids cfg)]
              [succ (block-successors cfg bid)]
              #:when (dominates? dom succ bid))
    (cons bid succ)))

;; ============================================================
;; 自然循环识别
;; ============================================================

;; 循环结构
(struct Loop
  (header       ; BlockId - 循环头
   back-edges   ; (list (cons src header) ...) - 后向边
   body         ; (set BlockId) - 循环体中的块
   preheader    ; BlockId or #f - 预头（如果存在）
   exits)       ; (list BlockId) - 出口块
  #:prefab)

;; 从后向边计算自然循环
;; 自然循环 = header + 所有能到达后向边源而不经过 header 的块
(define (compute-natural-loop cfg back-edge preds)
  (match-define (cons tail header) back-edge)

  (define body (mutable-set header))
  (define worklist (mutable-set))

  ;; 从 tail 开始向上遍历
  (unless (equal? tail header)
    (set-add! body tail)
    (set-add! worklist tail))

  (let loop ()
    (unless (set-empty? worklist)
      (define n (set-first worklist))
      (set-remove! worklist n)
      (for ([p (hash-ref preds n '())])
        (unless (set-member? body p)
          (set-add! body p)
          (set-add! worklist p)))
      (loop)))

  (for/set ([b body]) b))

;; 查找循环的出口块（循环内有边到循环外的块）
(define (find-loop-exits cfg loop-body)
  (for*/list ([bid (in-set loop-body)]
              [succ (block-successors cfg bid)]
              #:when (not (set-member? loop-body succ)))
    bid))

;; 查找或创建预头（preheader）
;; 预头是循环头的唯一循环外前驱
(define (find-preheader cfg header loop-body preds)
  (define outside-preds
    (for/list ([p (hash-ref preds header '())]
               #:when (not (set-member? loop-body p)))
      p))
  ;; 如果只有一个循环外前驱，它就是预头
  (if (= (length outside-preds) 1)
      (car outside-preds)
      #f))

;; 分析所有循环
(define (analyze-loops cfg)
  (define dom (compute-dominators cfg))
  (define preds (compute-predecessors cfg))
  (define back-edges (find-back-edges cfg dom))

  ;; 按 header 分组后向边
  (define header-to-back-edges (make-hash))
  (for ([be back-edges])
    (hash-update! header-to-back-edges (cdr be)
                  (λ (lst) (cons be lst)) '()))

  ;; 为每个 header 构建循环
  (for/list ([(header bes) (in-hash header-to-back-edges)])
    ;; 合并所有后向边的自然循环
    (define body
      (apply set-union
             (for/list ([be bes])
               (compute-natural-loop cfg be preds))))

    (define exits (find-loop-exits cfg body))
    (define preheader (find-preheader cfg header body preds))

    (Loop header bes body preheader exits)))

(provide Loop Loop-header Loop-back-edges Loop-body Loop-preheader Loop-exits
         analyze-loops
         compute-dominators dominates?
         compute-predecessors block-successors
         find-back-edges)

;; ============================================================
;; 循环嵌套分析
;; ============================================================

;; 判断循环 inner 是否嵌套在 outer 中
(define (loop-nested? inner outer)
  (and (not (equal? (Loop-header inner) (Loop-header outer)))
       (subset? (Loop-body inner) (Loop-body outer))))

;; 按嵌套深度排序循环（内层在前）
(define (sort-loops-by-depth loops)
  (sort loops
        (λ (a b)
          (> (set-count (Loop-body a))
             (set-count (Loop-body b))))))

(provide loop-nested? sort-loops-by-depth)
