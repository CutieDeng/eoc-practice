#lang racket/base

;; ============================================================
;; opt: pass_fixup_cfg (gcc/tree-cfg.cc execute_fixup_cfg), the instance
;; at the start of the local optimization passes (dump "fixup_cfg2")
;; ============================================================
;;
;; Every call whose callee is now known const / pure / looping gets
;; update_stmt: its virtual operands are rebuilt (a const call loses
;; VUSE and VDEF, a pure call its VDEF; the uses of a dropped VDEF move
;; to the VUSE and the name is released) and its use operands relinked
;; (a fresh use-list stamp); TODO_cleanup_cfg follows.
;;
;; Not ported: profile scaling against the IPA count, noreturn call
;; fixup, write-only store removal, __builtin_unreachable for blocks
;; without successors.
;; ============================================================

(require "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
         "../gimple.rkt"
         "../cfg.rkt"
         "../ssa.rkt"
         "../print.rkt"
         "ssa-ops.rkt"
         "mfn.rkt")

(provide fixup-cfg-pass)

(define (fixup-cfg-pass st fndecl cfg0)
  (define st0 (gs-set-fndecl st fndecl))
  (define cfg (cfg-clear-messages cfg0))
  (define m (gcfg->mfn cfg))
  (define todo #f)
  (for* ([bb (in-pvector (cfg-bbs cfg))] [id (in-list (mfn-stmts m bb))])
    (define g (mfn-stmt m id))
    (when (and (gcall? g) (gcall-fndecl g))
      (define flags (call-ecf st0 g))
      (when (not (zero? (bitwise-and flags (bitwise-ior ECF-CONST ECF-PURE ECF-LOOPING))))
        (set! todo #t)
        ;; update_stmt: rebuild the virtual operands
        (define const? (not (zero? (bitwise-and flags ECF-CONST))))
        (define pure? (not (zero? (bitwise-and flags ECF-PURE))))
        (define g1
          (cond
            [(and (or const? pure?) (stmt-vdef g))
             (mfn-unlink-vdef! m id)
             (mfn-release-name! m (stmt-vdef g))
             (stmt-set-flag g 'vdef #f)]
            [else g]))
        (define g2 (if (and const? (stmt-vuse g1)) (stmt-set-flag g1 'vuse #f) g1))
        ;; the statement is relinked even when its real operands did not change
        (mfn-replace! m id g2)
        (mfn-relink! m id))))
  (define cfg1 (mfn->gcfg m))
  (if todo (cleanup-tree-cfg st0 cfg1 print-cond-rhs) (values cfg1 st0)))
