#lang racket/base

;; ============================================================
;; 转发模块：cutie-ftree
;; ============================================================
;;
;; 统一的依赖入口，避免不同深度模块使用不同的相对路径
;; 用法：
;;   - compiler/*.rkt:       (require "ftree.rkt")
;;   - compiler/*/*.rkt:     (require "../ftree.rkt")
;;   - compiler/*/*/*.rkt:   (require "../../ftree.rkt")
;; ============================================================

(require "../cutie-ftree/main.rkt")

(provide (all-from-out "../cutie-ftree/main.rkt"))
