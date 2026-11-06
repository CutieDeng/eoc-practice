#lang racket/base

;; ============================================================
;; opt: the -O1 early pass sequence (doc/roadmap.md 7.1)
;; ============================================================
;;
;; (o1-passes st) -> list of (list stage-name pass remove-unused-locals?)
;;   pass : st fndecl cfg -> (values cfg st)
;;   remove-unused-locals? : the pass asks for TODO_remove_unused_locals
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
         "vn.rkt"
         "dse.rkt"
         "dce.rkt"
         "profile.rkt"
         "pure-const.rkt")

(provide o1-passes identity-pass ealias pass-name pass-fn pass-prunes-locals?)
(define pass-name car)
(define pass-fn cadr)
(define pass-prunes-locals? caddr)

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
  (define (p name fn [locals? #f]) (list name fn locals?))
  (append
   (list (p 'ccp1 ccp)
         (p 'forwprop1 forwprop)
         (p 'ethread identity-pass)
         (p 'esra identity-pass)
         (p 'ealias ealias))
   (if (>= major 13) (list (p 'phiprop1 identity-pass)) '())
   (list (p 'fre1 fre)
         (p 'mergephi1 identity-pass)
         (p 'dse1 dse)
         ;; pass_cd_dce, remove_unused_locals since GCC 15
         (p 'cddce1 dce (>= major 15))
         (p 'phiopt1 identity-pass)
         (p 'iftoswitch identity-pass))
   (if (>= major 14) (list (p 'sccopy1 identity-pass)) '())
   (list (p 'profile_estimate profile-estimate) (p 'local-pure-const1 local-pure-const))))
