#lang racket/base

;; ============================================================
;; opt: the -O1 early pass sequence (doc/roadmap.md 7.1)
;; ============================================================
;;
;; (o1-passes st) -> list of (cons stage-name pass)
;;   pass : st fndecl cfg -> (values cfg st)
;; in GCC's dump order for the dump's GCC version.  A pass that is not
;; ported yet but changes nothing on the fixtures is the identity (its
;; dump must still equal the previous one, which the -O1 test checks);
;; the roadmap table says which ones.
;; ============================================================

(require "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
         "../cfg.rkt"
         "ccp.rkt"
         "forwprop.rkt"
         "vn.rkt")

(provide o1-passes identity-pass ealias)

;; a pass without effect: a fresh dump_file, nothing else
(define (identity-pass st fndecl cfg) (values (cfg-clear-messages cfg) st))

;; pass_build_ealias: points-to analysis is not ported; GCC >= 13 prints
;; the constraint solver's progress lines for every function
;; (solve_constraints, `if (dump_file)`), GCC 12 also dumps the
;; constraints and the points-to sets (not reproduced)
(define (ealias st fndecl cfg)
  (define lines
    '("" "Collapsing static cycles and doing variable substitution" "Building predecessor graph"
      "Detecting pointer and location equivalences" "Rewriting constraints and unifying variables"
      "Uniting pointer but not location equivalent variables" "Finding indirect cycles" "Solving graph"))
  (values (for/fold ([c (cfg-clear-messages cfg)]) ([l (in-list lines)]) (cfg-add-message c l)) st))

(define (o1-passes st)
  (define major (gcc-major-version st))
  (append
   (list (cons 'ccp1 ccp)
         (cons 'forwprop1 forwprop)
         (cons 'ethread identity-pass)
         (cons 'esra identity-pass)
         (cons 'ealias ealias))
   (if (>= major 13) (list (cons 'phiprop1 identity-pass)) '())
   (list (cons 'fre1 fre))))
