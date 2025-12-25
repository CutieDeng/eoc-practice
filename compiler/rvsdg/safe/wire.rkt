#lang racket/base

;; ============================================================
;; Safe Layer: 带检查的连接操作
;; ============================================================
;;
;; 提供安全的边连接操作，包括：
;; - 前置条件检查
;; - 自动分配 Wire ID
;; - 原子性操作
;; ============================================================

(require racket/match racket/dict)
(require "../core-def.rkt")
(require "../raw/wire.rkt")
(require "../raw/connect.rkt")
(require "../raw/query.rkt")

;; === 存在性检查 ===

(define (rvsdg/wire-exists? region wire-id)
  (dict-has-key? (Region-wire->input region) wire-id))

(define (rvsdg/input-exists? region input-id)
  (dict-has-key? (Region-input->node region) input-id))

(define (rvsdg/output-exists? region output-id)
  (dict-has-key? (Region-output->node region) output-id))

(provide rvsdg/wire-exists?)
(provide rvsdg/input-exists?)
(provide rvsdg/output-exists?)

;; === 连接状态检查 ===

(define (rvsdg/input-connected? region input-id)
  (and (rvsdg-raw/input-wire region input-id) #t))

(define (rvsdg/output-connected? region output-id)
  (and (rvsdg-raw/output-wire region output-id) #t))

(provide rvsdg/input-connected?)
(provide rvsdg/output-connected?)

;; === 连接操作 ===

;; 连接输出端口到输入端口（分配新的 Wire）
;; 前置条件：
;; - output-id 存在
;; - input-id 存在且未连接
;; 返回: (Values WireId Region)
(define (rvsdg/connect region output-id input-id)
  ;; 检查端口存在性
  (unless (rvsdg/output-exists? region output-id)
    (error 'rvsdg/connect "Output port does not exist: ~a" output-id))
  (unless (rvsdg/input-exists? region input-id)
    (error 'rvsdg/connect "Input port does not exist: ~a" input-id))
  ;; 检查输入端口未连接
  (when (rvsdg/input-connected? region input-id)
    (error 'rvsdg/connect "Input port already connected: ~a" input-id))

  ;; 分配 Wire ID
  (define-values (wire-id region^) (rvsdg-raw/alloc-wire-ids region 1))
  ;; 建立连接
  (define region^^ (rvsdg-raw/wire-input-output-connect region^ wire-id input-id output-id))
  (values wire-id region^^))

(provide rvsdg/connect)

;; === 断开操作 ===

;; 断开输入端口的连接
;; 前置条件：input-id 已连接
(define (rvsdg/disconnect-input region input-id)
  (define wire-id (rvsdg-raw/input-wire region input-id))
  (unless wire-id
    (error 'rvsdg/disconnect-input "Input port not connected: ~a" input-id))
  (define output-id (rvsdg-raw/wire-output region wire-id))
  (rvsdg-raw/wire-input-output-disconnect region wire-id input-id output-id))

(provide rvsdg/disconnect-input)

;; 断开输出端口的连接（如果已连接）
;; 注意：一个输出可以连接到多个输入，这里只断开到指定输入的连接
(define (rvsdg/disconnect-output region output-id)
  (define wire-id (rvsdg-raw/output-wire region output-id))
  (unless wire-id
    (error 'rvsdg/disconnect-output "Output port not connected: ~a" output-id))
  (define input-id (rvsdg-raw/wire-input region wire-id))
  (rvsdg-raw/wire-input-output-disconnect region wire-id input-id output-id))

(provide rvsdg/disconnect-output)

;; === 重连操作 ===

;; 重连输入端口到新的输出端口（原子操作）
;; 前置条件：input-id 已连接
(define (rvsdg/reconnect-input region input-id new-output-id)
  (unless (rvsdg/input-connected? region input-id)
    (error 'rvsdg/reconnect-input "Input port not connected: ~a" input-id))
  (unless (rvsdg/output-exists? region new-output-id)
    (error 'rvsdg/reconnect-input "New output port does not exist: ~a" new-output-id))

  ;; 获取当前连接
  (define wire-id (rvsdg-raw/input-wire region input-id))
  (define old-output-id (rvsdg-raw/wire-output region wire-id))

  ;; 重连
  (rvsdg-raw/wire-output-reconnect region wire-id new-output-id old-output-id))

(provide rvsdg/reconnect-input)

;; === 查询操作 ===

;; 获取输入端口的数据源（输出端口）
(define (rvsdg/get-input-source region input-id)
  (define wire-id (rvsdg-raw/input-wire region input-id))
  (and wire-id (rvsdg-raw/wire-output region wire-id)))

(provide rvsdg/get-input-source)

;; 获取输出端口连接的目标（输入端口）
;; 注意：当前实现每个 wire 只连接一个输入，返回单个值
(define (rvsdg/get-output-target region output-id)
  (define wire-id (rvsdg-raw/output-wire region output-id))
  (and wire-id (rvsdg-raw/wire-input region wire-id)))

(provide rvsdg/get-output-target)

;; 获取输入端口所属的节点
(define (rvsdg/get-input-node region input-id)
  (rvsdg-raw/input-node region input-id))

(provide rvsdg/get-input-node)

;; 获取输出端口所属的节点
(define (rvsdg/get-output-node region output-id)
  (rvsdg-raw/output-node region output-id))

(provide rvsdg/get-output-node)
