#lang racket/base

;; ============================================================
;; Safe Layer: Region 不变量验证
;; ============================================================
;;
;; 提供 Region 的完整性检查，确保数据结构的一致性
;; ============================================================

(require racket/match racket/dict)
(require "../core-def.rkt")
(require "../raw/query.rkt")
(require "../raw/node-ctor.rkt")

;; === 验证结果 ===

(struct ValidationError (kind message details) #:transparent)

(provide (struct-out ValidationError))

;; === 单项验证 ===

;; 验证节点的端口映射一致性
(define (validate-node-ports region node-id)
  (define inputs (rvsdg-raw/node-inputs region node-id))
  (define outputs (rvsdg-raw/node-outputs region node-id))

  (cond
    [(not inputs)
     (ValidationError 'missing-inputs
                      "Node has no input port mapping"
                      node-id)]
    [(not outputs)
     (ValidationError 'missing-outputs
                      "Node has no output port mapping"
                      node-id)]
    [else
     ;; 检查所有输入端口的反向映射
     (match-define (cons input-id input-cnt) inputs)
     (for/or ([i (in-range input-cnt)])
       (define port (rvsdg-raw/input-offset input-id i))
       (define owner (rvsdg-raw/input-node region port))
       (if (equal? owner node-id)
           #f
           (ValidationError 'input-owner-mismatch
                            "Input port owner mismatch"
                            (list port node-id owner))))]))

(define (validate-node-outputs-mapping region node-id)
  (define outputs (rvsdg-raw/node-outputs region node-id))
  (when outputs
    (match-define (cons output-id output-cnt) outputs)
    (for/or ([i (in-range output-cnt)])
      (define port (rvsdg-raw/output-offset output-id i))
      (define owner (rvsdg-raw/output-node region port))
      (if (equal? owner node-id)
          #f
          (ValidationError 'output-owner-mismatch
                           "Output port owner mismatch"
                           (list port node-id owner))))))

;; 验证边的连接一致性
(define (validate-wire region wire-id)
  (define input-id (rvsdg-raw/wire-input region wire-id))
  (define output-id (rvsdg-raw/wire-output region wire-id))

  (cond
    [(not input-id)
     (ValidationError 'wire-no-input
                      "Wire has no input endpoint"
                      wire-id)]
    [(not output-id)
     (ValidationError 'wire-no-output
                      "Wire has no output endpoint"
                      wire-id)]
    [else
     ;; 检查反向映射
     (define reverse-input (rvsdg-raw/input-wire region input-id))
     (define reverse-output (rvsdg-raw/output-wire region output-id))
     (cond
       [(not (equal? reverse-input wire-id))
        (ValidationError 'wire-input-mismatch
                         "Wire-input reverse mapping mismatch"
                         (list wire-id input-id reverse-input))]
       [(not (equal? reverse-output wire-id))
        (ValidationError 'wire-output-mismatch
                         "Wire-output reverse mapping mismatch"
                         (list wire-id output-id reverse-output))]
       [else #f])]))

;; === 完整验证 ===

;; 验证整个 Region 的一致性
;; 返回: (Listof ValidationError) 或 '()
(define (rvsdg/validate-region region)
  (define errors '())

  ;; 验证所有节点
  (for ([node-id (in-list (rvsdg-raw/all-node-ids region))])
    (define port-error (validate-node-ports region node-id))
    (when port-error
      (set! errors (cons port-error errors)))
    (define output-error (validate-node-outputs-mapping region node-id))
    (when output-error
      (set! errors (cons output-error errors))))

  ;; 验证所有边
  (for ([wire-id (in-list (rvsdg-raw/all-wire-ids region))])
    (define wire-error (validate-wire region wire-id))
    (when wire-error
      (set! errors (cons wire-error errors))))

  (reverse errors))

(provide rvsdg/validate-region)

;; 验证 Region 是否有效（无错误）
(define (rvsdg/region-valid? region)
  (null? (rvsdg/validate-region region)))

(provide rvsdg/region-valid?)

;; 断言 Region 有效，否则报错
(define (rvsdg/assert-valid! region [context ""])
  (define errors (rvsdg/validate-region region))
  (unless (null? errors)
    (error 'rvsdg/assert-valid!
           "Region validation failed~a: ~a"
           (if (string=? context "") "" (format " (~a)" context))
           errors)))

(provide rvsdg/assert-valid!)
