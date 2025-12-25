#lang racket/base

;; ============================================================
;; Raw Layer: Region 初始化与比较函数
;; ============================================================

(require racket/dict)
(require "../core-def.rkt")
(require cutie-ftree)

;; === 比较函数生成器 ===

(define (((abstract-compare-generator compare) map) lhs rhs)
  (compare (map lhs) (map rhs)))

(define integer-compare-generator (abstract-compare-generator integer-compare))

;; ID 类型的比较函数
(define wire-compare (integer-compare-generator WireId-id))
(define input-compare (integer-compare-generator InputId-id))
(define output-compare (integer-compare-generator OutputId-id))
(define node-compare (integer-compare-generator NodeId-id))

;; Symbol 比较函数（用于 info 的 key）
;; cutie-ftree 比较函数返回 '<, '>, '=
(define (symbol-compare a b)
  (define sa (symbol->string a))
  (define sb (symbol->string b))
  (cond
    [(string<? sa sb) '<]
    [(string>? sa sb) '>]
    [else '=]))

(provide wire-compare input-compare output-compare node-compare)
(provide symbol-compare)
(provide integer-compare-generator abstract-compare-generator)

;; === 保留的节点 ID ===

(define INPUT-NODE-ID 0)   ; Region 的输入虚拟节点
(define OUTPUT-NODE-ID 1)  ; Region 的输出虚拟节点
(define START-NODE-CNT 2)  ; 用户节点从 2 开始

(provide INPUT-NODE-ID OUTPUT-NODE-ID START-NODE-CNT)

;; === 空 Region 实例 ===

(define Region-empty-instance
  (Region
    (ordl-make-empty symbol-compare)   ; info
    (ordl-make-empty wire-compare)     ; wire->input
    (ordl-make-empty wire-compare)     ; wire->output
    (ordl-make-empty input-compare)    ; input->wire
    (ordl-make-empty input-compare)    ; input->node
    (ordl-make-empty output-compare)   ; output->wire
    (ordl-make-empty output-compare)   ; output->node
    (ordl-make-empty node-compare)     ; node->input
    (ordl-make-empty node-compare)     ; node->output
    (ordl-make-empty node-compare)     ; node->value
    0                                  ; wire-cnt
    0                                  ; input-cnt
    0                                  ; output-cnt
    START-NODE-CNT                     ; node-cnt
  ))

(define (Region-empty) Region-empty-instance)
(provide Region-empty)

;; === info 操作 ===

(define (rvsdg-raw/get-info region key)
  (define info (Region-info region))
  (dict-ref info key #f))

(define (rvsdg-raw/set-info region key value)
  (define info (Region-info region))
  (define info^ (dict-set info key value))
  (struct-copy Region region [info info^]))

(define (rvsdg-raw/remove-info region key)
  (define info (Region-info region))
  (define info^ (dict-remove info key))
  (struct-copy Region region [info info^]))

(define (rvsdg-raw/update-info region key f default)
  (define old-value (or (rvsdg-raw/get-info region key) default))
  (rvsdg-raw/set-info region key (f old-value)))

(provide rvsdg-raw/get-info)
(provide rvsdg-raw/set-info)
(provide rvsdg-raw/remove-info)
(provide rvsdg-raw/update-info)
