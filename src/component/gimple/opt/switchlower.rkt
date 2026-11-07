#lang racket/base

;; ============================================================
;; opt: pass_lower_switch (gcc/tree-switch-conversion.cc,
;; switch_decision_tree), dump "switchlower1"
;; ============================================================
;;
;; (switchlower-pass st fndecl cfg) -> (values cfg st)
;;
;; Every GIMPLE_SWITCH (group_case_labels_stmt first) becomes a decision
;; tree of conditions: the cases form simple clusters (one per case,
;; probability = edge probability / cases on the edge), the ordered list
;; is balanced (balance_case_nodes), the switch block is split off and
;; its incoming edge split, the function is dumped
;; (dump_function_to_file, the local decls come from the driver through
;; the 'local-decls flag), emit_case_nodes builds the comparisons
;; (do_jump_if_equal / emit_cmp_and_jump_insns: GCC >= 13 scales the
;; count of each new false block), the fall-through goes to the default
;; block, the switch block is deleted and the PHI arguments of the new
;; edges are filled from the old switch edges.  TODO_update_ssa (the
;; virtual operands marked for renaming: dump of the symbol set and the
;; block counts) and TODO_cleanup_cfg follow.
;;
;; Not ported: bit-test and jump-table clusters (an error names them),
;; range cases inside a decision tree (generate_range_test).
;; ============================================================

(require racket/list racket/string
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../cfg.rkt"
         (only-in "../print.rkt" print-ssa-function stmt->string print-cond-rhs)
         (only-in "../ssa.rkt" virtual-operand?)
         "generic-fold.rkt")

(provide switchlower-pass)

;; ------------------------------------------------------------
;; profile_probability / profile_count arithmetic (values only)
;; ------------------------------------------------------------

(define MAXP 134217728)
(define (rdiv x y) (quotient (+ x (quotient y 2)) y))
(define (p+ a b) (min MAXP (+ a b)))
(define (p- a b) (if (>= a b) (- a b) 0))
(define (p/ a b)
  (cond
    [(zero? a) 0]
    [(>= a b) MAXP]
    [else (min MAXP (rdiv (* a MAXP) b))]))
(define (p/n a n) (if (= n 1) a (rdiv a n)))
(define (pinv a) (- MAXP a))
(define (count-apply c p) (and c (rdiv (* c p) MAXP)))

(define (set-prob c src dest p)
  (define pf (GCfg-profile c))
  (if pf (cfg-set-profile c (GProf (GProf-counts pf) (hash-set (GProf-probs pf) (cons src dest) p))) c))
(define (set-count c bb n)
  (define pf (GCfg-profile c))
  (if (and pf n) (cfg-set-profile c (GProf (intmap-set (GProf-counts pf) bb n) (GProf-probs pf))) c))

;; ------------------------------------------------------------
;; the pass
;; ------------------------------------------------------------

(struct node (low high case-bb prob [subtree #:mutable] [left #:mutable] [right #:mutable]) #:transparent)

(define (switchlower-pass st0 fndecl cfg0)
  (define st (gs-set-fndecl st0 fndecl))
  (define cfg (cfg-clear-messages cfg0))
  (define switches
    (for/list ([bb (in-pvector (cfg-bbs cfg))]
               #:when (let ([g (bb-last-stmt cfg bb)]) (and g (gswitch? g))))
      bb))
  (cond
    [(null? switches) (values cfg st)]
    [else
     (define-values (cfg1 st1)
       (for/fold ([c cfg] [s st]) ([bb (in-list switches)])
         (lower-switch s fndecl c bb)))
     ;; TODO_update_ssa: mark_virtual_operands_for_renaming
     (define cfg2 (update-ssa-vops-messages st1 fndecl cfg1))
     (cleanup-tree-cfg st1 cfg2 print-cond-rhs)]))

;; dump_update_ssa / update_ssa for the virtual operand symbol: nothing
;; is defined in the new blocks, so only the messages remain
(define (update-ssa-vops-messages st fndecl cfg)
  ;; gimple_vop (cfun): the variable of any virtual operand
  (define vop
    (for*/first ([bb (in-pvector (cfg-bbs cfg))]
                 [g (in-sequences (in-pvector (GBlock-phis (cfg-block cfg bb))) (in-pvector (GBlock-stmts (cfg-block cfg bb))))]
                 [v (in-value (cond [(gphi? g) (and (virtual-operand? (ssa-name-var (gphi-result g))) (gphi-result g))]
                                    [else (or (stmt-flag g 'vuse #f) (stmt-flag g 'vdef #f))]))]
                 #:when v)
      (ssa-name-var v)))
  (define nblocks (last-basic-block cfg))
  ;; prepare_block_for_update walks the dominator tree from ENTRY: every
  ;; block but EXIT
  (define nupdate (add1 (for/sum ([bb (in-pvector (cfg-bbs cfg))] #:unless (memv bb (list ENTRY EXIT))) 1)))
  (for/fold ([c cfg]) ([l (list "" "Symbols to be put in SSA form"
                               (format "{ D.~a }" (if vop (decl-uid vop) "?"))
                               "Incremental SSA update started at block: 0"
                               (format "Number of blocks in CFG: ~a" nblocks)
                               (format "Number of blocks to update: ~a (~a%)" nupdate
                                       (let ([pct (/ (* 100.0 nupdate) nblocks)])
                                         (pad3 (number->string (inexact->exact (round* pct))))))
                               "" "")])
    (cfg-add-message c l)))
(define (pad3 s) (string-append (make-string (max 0 (- 3 (string-length s))) #\space) s))
;; printf %3.0f: round half to even
(define (round* x) (let ([r (round x)]) r))

(define (lower-switch st fndecl c0 bb)
  (define g (bb-last-stmt c0 bb))
  (define major (gcc-major-version st))
  (define spot (let ([l (stmt-loc g)]) (and l (loc-spot l))))
  (define c1 (cfg-add-message c0 (format "beginning to process the following SWITCH statement (~a:~a) : ------- "
                                         (if spot (car spot) "") (if spot (cadr spot) 0))))
  (define c2 (cfg-add-message c1 (stmt->string st c1 bb g)))
  (define c3 (cfg-add-message c2 ""))
  (define index (gswitch-index g))
  (define (case-dest cs) (label-to-block c3 (case-label cs)))
  (define default-bb (case-dest (gswitch-default-case g)))
  (define cases (pvector->list (gswitch-cases g)))
  ;; group_case_labels_stmt: nothing to merge or drop on the fixtures
  (for ([cs (in-list cases)])
    (when (= (case-dest cs) default-bb) (error 'switchlower "a case going to the default block (group_case_labels_stmt) is not ported")))
  ;; compute_cases_per_edge
  (define per-edge (for/fold ([h (hasheqv)]) ([cs (in-list cases)]) (hash-update h (case-dest cs) add1 0)))
  (define max-c (for/fold ([m 0]) ([cs (in-list (reverse cases))])
                  (define n (hash-ref per-edge (case-dest cs)))
                  (if (> n m) (+ n (if (case-high cs) 1 0)) m)))
  (define clusters
    (for/list ([cs (in-list cases)])
      (define d (case-dest cs))
      (node (case-low cs) (or (case-high cs) (case-low cs)) d
            (p/n (or (prof-prob c3 bb d) 0) (hash-ref per-edge d)) #f #f #f)))
  ;; bit tests need several cases on one edge, jump tables at least
  ;; case_values_threshold () clusters
  (when (> max-c 1) (error 'switchlower "bit-test clusters are not ported"))
  (when (>= (length clusters) 4) (error 'switchlower "jump-table clusters are not ported"))
  (define c4 (cfg-add-message c3 (string-append ";; GIMPLE switch case clusters: "
                                                (apply string-append
                                                       (for/list ([n (in-list clusters)])
                                                         (if (equal? (integer-cst-value (node-low n)) (integer-cst-value (node-high n)))
                                                             (format "~a " (integer-cst-value (node-low n)))
                                                             (format "~a-~a " (integer-cst-value (node-low n)) (integer-cst-value (node-high n)))))))))
  (try-switch-expansion st fndecl c4 bb g clusters default-bb major))

(define (try-switch-expansion st0 fndecl c0 sbb g clusters default-bb major)
  (define st st0)
  (define index (gswitch-index g))
  (define default-prob (or (prof-prob c0 sbb default-bb) 0))
  (define case-bbs (cons default-bb (map node-case-bb clusters)))
  ;; record_phi_operand_mapping: result -> the argument from the switch block
  (define mapping
    (for*/fold ([m (hasheqv)]) ([b (in-list case-bbs)] [ph (in-pvector (GBlock-phis (cfg-block c0 b)))])
      (define a (for/first ([a (in-pvector (gphi-args ph))] #:when (= (car a) sbb)) (cdr a)))
      (if a (hash-set m (ssa-name-version (gphi-result ph)) a) m)))
  ;; split the switch off its block (split_block after the statement
  ;; before the switch) and split the edge between them
  (define nstmts (pvector-length (GBlock-stmts (cfg-block c0 sbb))))
  (define-values (swbb c1) (split-block c0 sbb (sub1 nstmts)))
  (define-values (bb c2) (split-edge c1 sbb swbb))
  ;; balance_case_nodes: two or fewer nodes stay a list
  (define head (balance clusters))
  ;; dump_function_to_file
  (define c3 (dump-function st fndecl c2))
  (define stamp (box (cfg-next-stamp c3)))
  (define c (box c3))
  (define (C) (unbox c))
  ;; do_jump_if_equal / emit_cmp_and_jump_insns
  (define (jump-if code bb op1 label-bb p)
    (define t (tree-type index))
    (define-values (op1* s1) (with-gs st (lambda () (gf-convert t op1))))
    (set! st s1)
    (define cond0 (gcond (stmt-loc g) (hasheq) code index op1* #f #f))
    (define-values (cnd n) (restamp #f cond0 (unbox stamp)))
    (set-box! stamp n)
    (define b (cfg-block (C) bb))
    (set-box! c (set-block (C) (block-set-stmts b (pvector-cons-right (GBlock-stmts b) cnd))))
    (define-values (fb c*) (split-block (C) bb (pvector-length (GBlock-stmts (cfg-block (C) bb)))))
    (define c** (set-edge-flags* c* bb fb EDGE-FALSE-VALUE))
    (define c*3 (set-prob c** bb fb (pinv p)))
    (define c*4 (if (>= major 13) (set-count c*3 fb (count-apply (prof-count c*3 bb) (pinv p))) c*3))
    (define-values (_new c*5) (make-edge c*4 bb label-bb EDGE-TRUE-VALUE))
    (define c*6 (add-phi-slots (set-prob c*5 bb label-bb p) label-bb bb))
    (set-box! c c*6)
    fb)
  (define (emit-jump bb case-bb)
    (define old (GEdge-dest (single-succ-edge* (C) bb)))
    (set-box! c (add-phi-slots (redirect-edge-succ (C) bb old case-bb) case-bb bb)))
  ;; emit_case_nodes for single-value nodes without left children
  (define (emit-case-nodes bb nd default-prob)
    (cond
      [(not nd) bb]
      [(not (equal? (integer-cst-value (node-low nd)) (integer-cst-value (node-high nd))))
       (error 'switchlower "range cases are not ported")]
      [else
       (define p (p/ (node-prob nd) (p+ (node-subtree nd) default-prob)))
       (define bb1 (jump-if 'eq_expr bb (node-low nd) (node-case-bb nd) p))
       (set-node-subtree! nd (p- (node-subtree nd) (if (>= major 13) (node-prob nd) p)))
       (define l (node-left nd)) (define r (node-right nd))
       (cond
         [(and l r) (error 'switchlower "two-child case nodes are not ported")]
         [r
          (cond
            [(or (node-left r) (node-right r)) (error 'switchlower "deeper right subtrees are not ported")]
            [else
             (define p2 (p/ (node-subtree r) (p+ (node-subtree nd) default-prob)))
             (jump-if 'eq_expr bb1 (node-low r) (node-case-bb r) p2)])]
         [l (error 'switchlower "left subtrees are not ported")]
         [else bb1])]))
  (define last-bb (emit-case-nodes bb head default-prob))
  (when last-bb (emit-jump last-bb default-bb))
  ;; remove the switch and its block
  (define swb (cfg-block (C) swbb))
  (set-box! c (set-block (C) (block-set-stmts swb (pvector-empty))))
  (set-box! c (delete-basic-block (C) swbb))
  ;; fix_phi_operands_for_edges
  (for* ([b (in-list case-bbs)])
    (define blk (cfg-block (C) b))
    (when (for*/or ([ph (in-pvector (GBlock-phis blk))] [a (in-pvector (gphi-args ph))]) (not (cdr a)))
      (define phis
        (for/pvector ([ph (in-pvector (GBlock-phis blk))])
          (gphi (gphi-loc ph) (gphi-flags ph) (gphi-var ph) (gphi-result ph)
                (for/pvector ([a (in-pvector (gphi-args ph))])
                  (if (cdr a) a (cons (car a) (hash-ref mapping (ssa-name-version (gphi-result ph)))))))))
      (define-values (phis* n) (restamp-phis* (GBlock-phis blk) phis (unbox stamp)))
      (set-box! stamp n)
      (set-box! c (set-block (C) (block-set-phis blk phis*)))))
  (values (C) st))

(define (restamp-phis* olds news n0)
  (for/fold ([acc (pvector-empty)] [n n0] #:result (values acc n)) ([o (in-pvector olds)] [nw (in-pvector news)])
    (define-values (g n1) (restamp o nw n))
    (values (pvector-cons-right acc g) n1)))

(define (single-succ-edge* c bb) (pvector-first (cfg-succs c bb)))
(define (set-edge-flags* c src dest f)
  (set-succs c src (for/pvector ([e (in-pvector (cfg-succs c src))])
                     (if (= (GEdge-dest e) dest) (GEdge dest f (GEdge-goto-locus e)) e))))

;; execute_on_growing_pred: a new edge SRC -> DEST gets an empty
;; argument slot in DEST's PHIs (filled by fix_phi_operands_for_edges)
(define (add-phi-slots c dest src)
  (define b (cfg-block c dest))
  (if (pvector-empty? (GBlock-phis b))
      c
      (set-block c (block-set-phis b (for/pvector ([ph (in-pvector (GBlock-phis b))])
                                       (gphi (gphi-loc ph) (gphi-flags ph) (gphi-var ph) (gphi-result ph)
                                             (pvector-cons-right (gphi-args ph) (cons src #f))))))))

;; balance_case_nodes for at most two nodes: one level, the head's subtree
;; probability covers the list
(define (balance clusters)
  (cond
    [(null? clusters) #f]
    [(> (length clusters) 2) (error 'switchlower "balancing more than two case nodes is not ported")]
    [else
     (define head (car clusters))
     (set-node-subtree! head (node-prob head))
     (for ([n (in-list (cdr clusters))])
       (set-node-subtree! n (node-prob n))
       (set-node-subtree! head (p+ (node-subtree head) (node-subtree n))))
     (for ([a (in-list clusters)] [b (in-list (cdr clusters))]) (set-node-right! a b))
     head]))

;; dump_function_to_file into the messages
(define (dump-function st fndecl c)
  (define locals (decl-flag st fndecl 'local-decls (pvector-empty)))
  (define out (open-output-string))
  (print-ssa-function st fndecl locals (cfg-clear-messages c) out)
  (define lines (string-split (get-output-string out) "\n" #:trim? #f))
  (for/fold ([c c]) ([l (in-list (drop-right lines 1))]) (cfg-add-message c l)))
