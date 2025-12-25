#lang racket/base

;; ============================================================
;; Semantic Layer: 结构化节点操作
;; ============================================================
;;
;; 提供结构化节点（Gamma, Theta, Lambda 等）的创建和操作
;; ============================================================

(require racket/match racket/dict racket/list)
(require "../core-def.rkt")
(require "../raw/region-ctor.rkt")
(require "../raw/node-value.rkt")
(require "../safe/node.rkt")

;; === 结构化节点创建 ===

;; 创建 Gamma 节点（条件分支）
;; pred-cnt: 谓词/条件数（通常为 1，即 if-then-else）
;; branch-regions: 分支区域列表
;; 输入端口: [condition, args...]
;; 输出端口: [results...]
(define (rvsdg/create-gamma region input-cnt output-cnt branch-regions)
  (define-values (node-id in-id out-id region^)
    (rvsdg/create-node region input-cnt output-cnt))
  (define region^^
    (rvsdg-raw/set-node-value region^ node-id (Gamma branch-regions)))
  (values node-id in-id out-id region^^))

(provide rvsdg/create-gamma)

;; 创建简单的 if-then-else Gamma
(define (rvsdg/create-if-then-else region then-region else-region arg-cnt result-cnt)
  ;; 输入: 1 条件 + arg-cnt 参数
  ;; 输出: result-cnt 结果
  (rvsdg/create-gamma region (+ 1 arg-cnt) result-cnt (list then-region else-region)))

(provide rvsdg/create-if-then-else)

;; 创建 Theta 节点（循环）
;; body-region: 循环体区域
;; 输入端口: [loop-vars..., invariants...]
;; 输出端口: [loop-results...]
(define (rvsdg/create-theta region input-cnt output-cnt body-region)
  (define-values (node-id in-id out-id region^)
    (rvsdg/create-node region input-cnt output-cnt))
  (define region^^
    (rvsdg-raw/set-node-value region^ node-id (Theta body-region)))
  (values node-id in-id out-id region^^))

(provide rvsdg/create-theta)

;; 创建 Lambda 节点（函数定义）
;; body-region: 函数体区域
;; param-cnt: 参数数量
;; result-cnt: 返回值数量
;; 输入端口: [captures...] (闭包捕获的自由变量)
;; 输出端口: [function-value]
(define (rvsdg/create-lambda region capture-cnt body-region)
  (define-values (node-id in-id out-id region^)
    (rvsdg/create-node region capture-cnt 1))  ; Lambda 输出一个函数值
  (define region^^
    (rvsdg-raw/set-node-value region^ node-id (Lambda body-region)))
  (values node-id in-id out-id region^^))

(provide rvsdg/create-lambda)

;; 创建 Delta 节点（全局/可变变量）
(define (rvsdg/create-delta region input-cnt output-cnt body-region)
  (define-values (node-id in-id out-id region^)
    (rvsdg/create-node region input-cnt output-cnt))
  (define region^^
    (rvsdg-raw/set-node-value region^ node-id (Delta body-region)))
  (values node-id in-id out-id region^^))

(provide rvsdg/create-delta)

;; 创建 Phi 节点（相互递归函数组）
(define (rvsdg/create-phi region input-cnt output-cnt body-regions)
  (define-values (node-id in-id out-id region^)
    (rvsdg/create-node region input-cnt output-cnt))
  (define region^^
    (rvsdg-raw/set-node-value region^ node-id (Phi body-regions)))
  (values node-id in-id out-id region^^))

(provide rvsdg/create-phi)

;; 创建 Omega 节点（程序根）
(define (rvsdg/create-omega region input-cnt output-cnt body-region)
  (define-values (node-id in-id out-id region^)
    (rvsdg/create-node region input-cnt output-cnt))
  (define region^^
    (rvsdg-raw/set-node-value region^ node-id (Omega body-region)))
  (values node-id in-id out-id region^^))

(provide rvsdg/create-omega)

;; === 效果系统节点 ===

;; 创建 Psi 节点（prompt/reset 边界）
(define (rvsdg/create-psi region tag input-cnt output-cnt body-region)
  (define-values (node-id in-id out-id region^)
    (rvsdg/create-node region input-cnt output-cnt))
  (define region^^
    (rvsdg-raw/set-node-value region^ node-id (Psi tag body-region)))
  (values node-id in-id out-id region^^))

(provide rvsdg/create-psi)

;; 创建 Shift 节点（续体捕获）
(define (rvsdg/create-shift region tag input-cnt output-cnt)
  (define-values (node-id in-id out-id region^)
    (rvsdg/create-node region input-cnt output-cnt))
  (define region^^
    (rvsdg-raw/set-node-value region^ node-id (Shift tag)))
  (values node-id in-id out-id region^^))

(provide rvsdg/create-shift)

;; 创建 EffHandler 节点（效果处理器）
(define (rvsdg/create-eff-handler region ops input-cnt output-cnt handler-region return-region)
  (define-values (node-id in-id out-id region^)
    (rvsdg/create-node region input-cnt output-cnt))
  (define region^^
    (rvsdg-raw/set-node-value region^ node-id (EffHandler ops handler-region return-region)))
  (values node-id in-id out-id region^^))

(provide rvsdg/create-eff-handler)

;; 创建 EffPerform 节点（效果调用）
(define (rvsdg/create-eff-perform region op-name input-cnt output-cnt)
  (define-values (node-id in-id out-id region^)
    (rvsdg/create-node region input-cnt output-cnt))
  (define region^^
    (rvsdg-raw/set-node-value region^ node-id (EffPerform op-name)))
  (values node-id in-id out-id region^^))

(provide rvsdg/create-eff-perform)

;; === 子区域访问 ===

;; 获取结构化节点的子区域
(define (rvsdg/get-sub-regions region node-id)
  (define value (rvsdg/get-node-value region node-id))
  (cond
    [(Gamma? value) (Gamma-region* value)]
    [(Theta? value) (list (Theta-region value))]
    [(Lambda? value) (list (Lambda-region value))]
    [(Delta? value) (list (Delta-region value))]
    [(Phi? value) (Phi-region* value)]
    [(Omega? value) (list (Omega-region value))]
    [(Psi? value) (list (Psi-region value))]
    [(EffHandler? value) (list (EffHandler-handler-region value)
                                (EffHandler-return-region value))]
    [else '()]))

(provide rvsdg/get-sub-regions)

;; 设置结构化节点的子区域
(define (rvsdg/set-sub-region region node-id index new-sub-region)
  (define value (rvsdg/get-node-value region node-id))
  (define new-value
    (cond
      [(Gamma? value)
       (define regions (Gamma-region* value))
       (Gamma (list-set regions index new-sub-region))]
      [(Theta? value)
       (unless (= index 0) (error 'rvsdg/set-sub-region "Theta has only 1 region"))
       (Theta new-sub-region)]
      [(Lambda? value)
       (unless (= index 0) (error 'rvsdg/set-sub-region "Lambda has only 1 region"))
       (Lambda new-sub-region)]
      [(Delta? value)
       (unless (= index 0) (error 'rvsdg/set-sub-region "Delta has only 1 region"))
       (Delta new-sub-region)]
      [(Phi? value)
       (define regions (Phi-region* value))
       (Phi (list-set regions index new-sub-region))]
      [(Omega? value)
       (unless (= index 0) (error 'rvsdg/set-sub-region "Omega has only 1 region"))
       (Omega new-sub-region)]
      [(Psi? value)
       (unless (= index 0) (error 'rvsdg/set-sub-region "Psi has only 1 region"))
       (Psi (Psi-tag value) new-sub-region)]
      [else
       (error 'rvsdg/set-sub-region "Not a structured node: ~a" value)]))
  (rvsdg-raw/set-node-value region node-id new-value))

(provide rvsdg/set-sub-region)

;; === 节点类型谓词 ===

(define (rvsdg/structured-node? region node-id)
  (define value (rvsdg/get-node-value region node-id))
  (and value
       (or (Gamma? value) (Theta? value) (Lambda? value)
           (Delta? value) (Phi? value) (Omega? value)
           (Psi? value) (EffHandler? value))))

(provide rvsdg/structured-node?)

(define (rvsdg/simple-node? region node-id)
  (define value (rvsdg/get-node-value region node-id))
  (and value (Simple? value)))

(provide rvsdg/simple-node?)
