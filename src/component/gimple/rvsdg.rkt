#lang racket/base

;; ============================================================
;; rvsdg: the GCfg adapter of the CFG -> RVSDG structurizer
;; ============================================================
;;
;; Implements component/rvsdg/port.rkt for the SSA GCfg of ssa.rkt.
;; The structurizer reads blocks as the kernel exchange prefabs
;; (CfgBlock / PhiInsn / VfInsn / Term:*); this adapter builds those
;; views once, up front, from the GIMPLE statements:
;;
;;   SSA name version N        -> (VarId N)          values
;;   INTEGER_CST / REAL / ...  -> literal input (number, (real . x), bytes)
;;   gassign lhs = rhs         -> VfInsn (gimple . <rhs code>)
;;   load from a memory ref    -> VfInsn (gimple . load), inputs = the
;;                                SSA uses of the reference + a shape
;;                                literal (ref <shape>) that names them
;;                                positionally ((in k)); stores likewise
;;                                (gimple . store) with the value last
;;   gcall                     -> VfInsn (gimple call <name>)
;;   gcond                     -> comparison VfInsn into a fresh VarId
;;                                + Term:cond on it (TRUE/FALSE edge dests)
;;   gswitch                   -> Term:switch (case value . BlockId)
;;   greturn                   -> Term:ret; fall-through -> Term:jump
;;   gphi                      -> PhiInsn (pred BlockId . VarId)
;;   params                    -> the default definitions x_N(D), in
;;                                version order ('java/ssa-param-names
;;                                / 'java/max-local as the structurizer
;;                                expects)
;; Memory stays as opaque load/store placeholders until the memory
;; state threading of roadmap 6.4.
;;
;; (gimple->rvsdg st fndecl cfg) -> Lambda
;; ============================================================

(require racket/list racket/match
         "../../kernel/data/data.rkt"
         "../../kernel/ir/gimple/types.rkt"
         "../../kernel/ir/cfg/types.rkt"
         "../../component/rvsdg/port.rkt"
         "../../component/rvsdg/structurize.rkt"
         "state.rkt" "tree.rkt" "gimple.rkt" "gimplify.rkt" "cfg.rkt" "ssa.rkt")

(provide gimple->rvsdg gcfg-port gcfg-adapter)

;; adapter value: prebuilt views
(struct gad (entry blocks params))       ; blocks: idmap BlockId -> CfgBlock

(define (ssa-var t) (VarId (ssa-name-version t)))

;; ------------------------------------------------------------
;; operands
;; ------------------------------------------------------------

;; literal for a constant / address operand
(define (literal st t)
  (case (tree-code t)
    [(integer_cst) (integer-cst-value t)]
    [(real_cst) (cons 'real (or (real-cst-value t) (real-cst-hex t)))]
    [(string_cst) (string-cst-bytes t)]
    [(addr_expr) (list 'addr (ref-shape st (tree-operand t 0) (lambda (x) (list 'tree (tree-code x)))))]
    [else (list 'tree (tree-code t))]))

(define (memory-ref? st t)
  (or (memq (tree-code t) '(mem_ref component_ref array_ref array_range_ref indirect_ref bit_field_ref view_convert_expr realpart_expr imagpart_expr))
      (and (memq (tree-code t) '(var_decl parm_decl result_decl)) (not (is-gimple-reg st t)))))

;; shape datum of a memory reference; USE maps an SSA operand to a placeholder
(define (ref-shape st t use)
  (case (tree-code t)
    [(ssa_name) (use t)]
    [(var_decl parm_decl result_decl) (list 'var (decl-uid t) (decl-name t))]
    [(mem_ref) (list 'mem (ref-shape st (tree-operand t 0) use) (literal st (tree-operand t 1)))]
    [(component_ref) (list 'field (ref-shape st (tree-operand t 0) use) (decl-name (tree-operand t 1)))]
    [(array_ref array_range_ref) (list 'index (ref-shape st (tree-operand t 0) use) (ref-shape st (tree-operand t 1) use))]
    [(integer_cst real_cst string_cst) (literal st t)]
    [(addr_expr) (list 'addr (ref-shape st (tree-operand t 0) use))]
    [else (cons (tree-code t) (for/list ([o (in-pvector (tree-ops t))]) (ref-shape st o use)))]))

;; the SSA names inside a reference, in operand order
(define (ref-uses t)
  (cond
    [(not t) '()]
    [(eq? (tree-code t) 'ssa_name) (list t)]
    [else (append* (for/list ([o (in-pvector (tree-ops t))]) (ref-uses o)))]))

;; inputs + shape literal for a reference
(define (ref-inputs st t)
  (define uses (ref-uses t))
  (define pos (for/hash ([u (in-list uses)] [k (in-naturals)]) (values (ssa-name-version u) k)))
  (values (for/pvector ([u (in-list uses)]) (ssa-var u))
          (list 'ref (ref-shape st t (lambda (u) (list 'in (hash-ref pos (ssa-name-version u))))))))

(define (mk-insn op inputs outputs) (VfInsn op inputs outputs #f #f))

;; ------------------------------------------------------------
;; block views
;; ------------------------------------------------------------

;; per-function conversion state: fresh VarId counter
(struct cs (next))

;; an operand as an input: (values input extra-insns cs)
(define (operand st c t)
  (cond
    [(not t) (values #f (pvector-empty) c)]
    [(eq? (tree-code t) 'ssa_name) (values (ssa-var t) (pvector-empty) c)]
    [(memory-ref? st t)
     (define-values (ins shape) (ref-inputs st t))
     (define v (VarId (cs-next c)))
     (values v (pvector (mk-insn '(gimple . load) (pvector-cons-right ins shape) (pvector v))) (cs (add1 (cs-next c))))]
    [else (values (literal st t) (pvector-empty) c)]))

;; several operands in order
(define (operands st c ts)
  (for/fold ([ins (pvector-empty)] [extra (pvector-empty)] [c c] #:result (values ins extra c)) ([t (in-list ts)])
    (define-values (x e c*) (operand st c t))
    (values (pvector-cons-right ins x) (pvector-append extra e) c*)))

;; a value that must be a VarId (terminator operands)
(define (as-var st c t)
  (define-values (x extra c1) (operand st c t))
  (cond
    [(VarId? x) (values x extra c1)]
    [else
     (define v (VarId (cs-next c1)))
     (values v (pvector-cons-right extra (mk-insn '(gimple . const) (pvector x) (pvector v))) (cs (add1 (cs-next c1))))]))

(define (stmt-views st c g)
  ;; -> (values insns c)
  (cond
    [(gassign? g)
     (define lhs (gassign-lhs g))
     (define code (gassign-code g))
     (define rhs-trees (filter values (list (gassign-rhs1 g) (gassign-rhs2 g) (gassign-rhs3 g))))
     (cond
       [(eq? (tree-code lhs) 'ssa_name)
        (define out (ssa-var lhs))
        (cond
          ;; a plain load
          [(and (eq? (gimple-rhs-class st code) 'single) (memory-ref? st (gassign-rhs1 g)))
           (define-values (ins shape) (ref-inputs st (gassign-rhs1 g)))
           (values (pvector (mk-insn '(gimple . load) (pvector-cons-right ins shape) (pvector out))) c)]
          [else
           (define-values (ins extra c1) (operands st c rhs-trees))
           (values (pvector-cons-right extra (mk-insn (cons 'gimple code) ins (pvector out))) c1)])]
       [else
        ;; store: the rhs is a single operand (GIMPLE memory stores are is_gimple_val rhs)
        (define-values (val extra c1) (operand st c (gassign-rhs1 g)))
        (define-values (ins shape) (ref-inputs st lhs))
        (values (pvector-cons-right extra (mk-insn '(gimple . store) (pvector-cons-right (pvector-cons-right ins shape) val) (pvector-empty))) c1)])]
    [(gcall? g)
     (define fn (gcall-fn g))
     (define name (cond [(and (eq? (tree-code fn) 'addr_expr) (decl-name (tree-operand fn 0))) => values]
                        [(decl-name fn) => values]
                        [else (tree-code fn)]))
     (define-values (ins extra c1) (operands st c (pvector->list (gcall-args g))))
     (define lhs (gcall-lhs g))
     (cond
       [(not lhs) (values (pvector-cons-right extra (mk-insn (list 'gimple 'call name) ins (pvector-empty))) c1)]
       [(eq? (tree-code lhs) 'ssa_name)
        (values (pvector-cons-right extra (mk-insn (list 'gimple 'call name) ins (pvector (ssa-var lhs)))) c1)]
       [else
        (define v (VarId (cs-next c1)))
        (define-values (sins shape) (ref-inputs st lhs))
        (values (pvector-append extra (pvector (mk-insn (list 'gimple 'call name) ins (pvector v))
                                               (mk-insn '(gimple . store) (pvector-cons-right (pvector-cons-right sins shape) v) (pvector-empty))))
                (cs (add1 (cs-next c1))))])]
    [else (values (pvector-empty) c)]))     ; labels, predicts, nops carry nothing

(define (block-view st cfg i c)
  ;; -> (values CfgBlock c)
  (define stmts (GBlock-stmts (cfg-block cfg i)))
  (define body (for/pvector ([g (in-pvector stmts)] #:unless (or (gcond? g) (gswitch? g) (greturn? g))) g))
  (define-values (insns c1)
    (for/fold ([acc (pvector-empty)] [c c] #:result (values acc c)) ([g (in-pvector body)])
      (define-values (vs c*) (stmt-views st c g))
      (values (pvector-append acc vs) c*)))
  (define last (bb-last-stmt cfg i))
  (define succs (cfg-succs cfg i))
  (define (dest e) (BlockId (GEdge-dest e)))
  (define-values (insns2 term c2)
    (cond
      [(gcond? last)
       (define-values (ins extra ca) (operands st c1 (list (gcond-lhs last) (gcond-rhs last))))
       (define v (VarId (cs-next ca)))
       (define-values (t f) (extract-true-false-edges cfg i))
       (values (pvector-append insns (pvector-cons-right extra (mk-insn (cons 'gimple (gcond-code last)) ins (pvector v))))
               (Term:cond v (dest t) (dest f))
               (cs (add1 (cs-next ca))))]
      [(gswitch? last)
       (define-values (v extra c2) (as-var st c1 (gswitch-index last)))
       (define cases
         (for*/pvector ([cs* (in-pvector (gswitch-cases last))]
                        [k (in-range (integer-cst-value (case-low cs*))
                                     (add1 (integer-cst-value (or (case-high cs*) (case-low cs*)))))])
           (cons k (BlockId (label-to-block* cfg (case-label cs*))))))
       (values (pvector-append insns extra)
               (Term:switch v cases (BlockId (label-to-block* cfg (case-label (gswitch-default-case last)))))
               c2)]
      [(greturn? last)
       (cond
         [(greturn-retval last)
          (define-values (v extra c2) (as-var st c1 (greturn-retval last)))
          (values (pvector-append insns extra) (Term:ret (pvector v)) c2)]
         [else (values insns (Term:ret (pvector-empty)) c1)])]
      [(= (pvector-length succs) 1) (values insns (Term:jump (dest (pvector-first succs))) c1)]
      [(pvector-empty? succs) (values insns (Term:unreachable) c1)]
      [else (error 'gimple->rvsdg "block ~a: ~a successors without a control statement" i (pvector-length succs))]))
  (define phis
    (for/pvector ([ph (in-pvector (GBlock-phis (cfg-block cfg i)))])
      (PhiInsn (ssa-var (gphi-result ph))
               (for/pvector ([arg (in-pvector (gphi-args ph))])
                 (cons (BlockId (car arg)) (ssa-var (cdr arg)))))))
  (values (CfgBlock (BlockId i) phis insns2 term (hash)) c2))

;; label -> block index through the cfg's own map
(define (label-to-block* cfg l)
  (or (intmap-ref (GCfg-label->bb cfg) (decl-uid l) #f)
      (error 'gimple->rvsdg "label ~a has no block" (decl-uid l))))

;; ------------------------------------------------------------
;; the adapter and the port
;; ------------------------------------------------------------

(define (gcfg-adapter st cfg)
  (define names (ssa-names-in-version-order cfg))
  (define max-version (for/fold ([m 0]) ([n (in-pvector names)]) (max m (ssa-name-version n))))
  (define params (for/pvector ([n (in-pvector names)] #:when (tree-attr n 'default-def #f)) (ssa-var n)))
  (define-values (blocks _c)
    (for/fold ([m idmap-empty] [c (cs (add1 max-version))] #:result (values m c)) ([i (in-pvector (cfg-bbs cfg))])
      (define-values (blk c*) (block-view st cfg i c))
      (values (idmap-set m (BlockId i) blk) c*)))
  (define entry (BlockId (GEdge-dest (pvector-first (cfg-succs cfg ENTRY)))))
  (gad entry blocks params))

(define gcfg-port
  (rvsdg-port (lambda (a) (gad-entry a))
              (lambda (a) (list->pvector (idmap-keys (gad-blocks a))))
              (lambda (a bid) (idmap-ref (gad-blocks a) bid #f))
              (lambda (a key default)
                (case key
                  [(java/max-local) (pvector-length (gad-params a))]
                  [(java/ssa-param-names) (gad-params a)]
                  [else default]))))

(define (gimple->rvsdg st fndecl cfg)
  (structurize (make-cv gcfg-port (gcfg-adapter st cfg))))
