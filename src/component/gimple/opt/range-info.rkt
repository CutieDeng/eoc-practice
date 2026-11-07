#lang racket/base

;; ============================================================
;; opt: SSA_NAME_RANGE_INFO (gcc/tree-ssanames.cc set_range_info,
;; duplicate_ssa_name_range_info; gcc/value-query.cc
;; gimple_range_global)
;; ============================================================
;;
;; The global range of an integral SSA name survives between passes:
;; CCP (nonzero_p) and the ranger's export_global_ranges store it,
;; duplicate_ssa_name copies it, later range queries start from it.
;; The table is the 'range-info flag of the function declaration: an
;; immutable hash SSA version -> irange.
;;
;;   (range-info st fndecl name)             -> irange or #f
;;   (set-range-info st fndecl name r)       -> (values st line-or-#f)
;;       set_range_info: R intersected into the stored range (VARYING
;;       when none); nothing happens for an undefined / varying R or
;;       when the intersection does not change the stored range.  The
;;       line is the dump text "Global Exported: NAME = RANGE" (GCC
;;       prints it whenever dump_file is set; the caller decides).
;;       Pointer names (ptr_info nonnull) are not modelled: #f.
;;   (copy-range-info st fndecl from to)     -> st
;;   (gimple-range-global st fndecl name)    -> irange
;; ============================================================

(require "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
         "../tree.rkt"
         (only-in "../print.rkt" print-expr)
         "ssa-ops.rkt"
         "irange.rkt")

(provide range-info set-range-info copy-range-info gimple-range-global range-type?)

(define (table st fndecl) (decl-flag st fndecl 'range-info (hasheqv)))

;; value_range::supports_type_p for irange: integral types (pointers are prange)
(define (range-type? st ty) (and ty (integral-type-p st ty)))

(define (range-info st fndecl name)
  (hash-ref (table st fndecl) (ssa-name-version name) #f))

(define (set-range-info st fndecl name r)
  (define ty (tree-type name))
  (cond
    [(or (ir-undefined? r) (ir-varying? r)) (values st #f)]
    [(not (range-type? st ty)) (values st #f)]
    [else
     (define t (tinfo st ty))
     (define old (or (range-info st fndecl name) (ir-varying t)))
     (define-values (new changed?) (ir-intersect old r))
     (cond
       [(or (not changed?) (ir-undefined? new)) (values st #f)]
       [else
        (define st1 (set-decl-flag st fndecl 'range-info (hash-set (table st fndecl) (ssa-name-version name) new)))
        (values st1 (format "Global Exported: ~a = ~a" (print-expr st name) (ir->string st (gimple-range-global st1 fndecl name))))])]))

(define (copy-range-info st fndecl from to)
  (define r (range-info st fndecl from))
  (if r (set-decl-flag st fndecl 'range-info (hash-set (table st fndecl) (ssa-name-version to) r)) st))

;; gimple_range_global: the stored range, a parameter's default
;; definition from its range info, an uninitialized local UNDEFINED
(define (gimple-range-global st fndecl name)
  (define ty (tree-type name))
  (define t (tinfo st ty))
  (cond
    [(ssa-default-def? name)
     (define sym (ssa-name-var name))
     (cond
       [(and sym (eq? (tree-code sym) 'parm_decl))
        (define r (and (not (pointer-type-p st ty)) (range-info st fndecl name)))
        (if (and r (not (ir-undefined? r))) r (ir-varying t))]
       [(and sym (eq? (tree-code sym) 'result_decl)) (ir-varying t)]
       [else (ir-undefined t)])]
    [else
     (define r (and (not (pointer-type-p st ty)) (range-info st fndecl name)))
     (if (and r (not (ir-undefined? r))) r (ir-varying t))]))
