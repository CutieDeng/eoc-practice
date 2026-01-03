#lang racket/base

;; Run All AArch64 Unit Tests

(require rackunit
         rackunit/text-ui
         (prefix-in types: "test-types.rkt")
         (prefix-in cfg: "test-cfg.rkt")
         (prefix-in parser: "test-parser.rkt")
         (prefix-in interp: "test-interp.rkt")
         (prefix-in sve: "test-sve-interp.rkt"))

;; Collect all test suites
(define all-aarch64-tests
  (test-suite
   "All AArch64 Tests"
   types:all-tests
   cfg:all-tests
   parser:all-tests
   interp:all-tests
   sve:all-tests))

;; Run tests
(module+ main
  (displayln "Running AArch64 Type Tests...")
  (displayln "============================")
  (require (submod "test-types.rkt" test))

  (displayln "\nRunning AArch64 CFG Tests...")
  (displayln "============================")
  (require (submod "test-cfg.rkt" test))

  (displayln "\nRunning AArch64 Parser Tests...")
  (displayln "================================")
  (require (submod "test-parser.rkt" test))

  (displayln "\nRunning AArch64 Interpreter Tests...")
  (displayln "=====================================")
  (require (submod "test-interp.rkt" test))

  (displayln "\nRunning AArch64 SVE Interpreter Tests...")
  (displayln "=========================================")
  (require (submod "test-sve-interp.rkt" test))

  (displayln "\n========================================")
  (displayln "All AArch64 tests completed!"))
