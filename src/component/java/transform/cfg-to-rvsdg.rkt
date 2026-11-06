#lang racket/base

;; ============================================================
;; Component: Java SSA CFG → RVSDG (kernel-Cfg port of the structurizer)
;; ============================================================
;;
;; The algorithm lives in component/rvsdg/structurize.rkt and reads the
;; CFG through component/rvsdg/port.rkt; the kernel Cfg produced by
;; jvm-cfg->ssa (+ normalize-try-exits) is its own adapter.
;; ============================================================

(require "../../rvsdg/port.rkt" "../../rvsdg/structurize.rkt")
(provide cfg->rvsdg)

(define (cfg->rvsdg cfg) (structurize (make-cv kernel-cfg-port cfg)))
