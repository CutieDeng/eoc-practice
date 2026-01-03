#lang racket/base

;; Run All AArch64 Unit Tests
;;
;; Usage:
;;   raco test compiler/component/aarch64/tests/test-*.rkt
;;
;; Or run individual test files:
;;   raco test compiler/component/aarch64/tests/test-types.rkt
;;   raco test compiler/component/aarch64/tests/test-cfg.rkt
;;   raco test compiler/component/aarch64/tests/test-parser.rkt
;;   raco test compiler/component/aarch64/tests/test-interp.rkt
;;   raco test compiler/component/aarch64/tests/test-sve-interp.rkt
;;   raco test compiler/component/aarch64/tests/test-regalloc.rkt

(module+ test
  (require (submod "test-types.rkt" test)
           (submod "test-cfg.rkt" test)
           (submod "test-parser.rkt" test)
           (submod "test-interp.rkt" test)
           (submod "test-sve-interp.rkt" test)
           (submod "test-regalloc.rkt" test)))
