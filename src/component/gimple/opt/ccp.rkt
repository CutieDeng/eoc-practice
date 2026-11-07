#lang racket/base

;; ============================================================
;; opt: pass_ccp (gcc/tree-ssa-ccp.cc), first instance "ccp1", pure
;; ============================================================
;;
;; Conditional constant propagation with the bit-level lattice of
;; -ftree-bit-ccp (on at -O1): every SSA name is UNINITIALIZED,
;; UNDEFINED, CONSTANT (value + mask of unknown bits; a VARYING SSA
;; operand enters as a CONSTANT copy of itself with mask -1) or
;; VARYING.  The propagation runs on opt/propagate.rkt; ccp_finalize is
;; substitute_and_fold over the dominator tree followed by
;; simple_dce_from_worklist; when anything changed the pass asks for
;; TODO_cleanup_cfg, i.e. cleanup_tree_cfg runs.
;;
;; (ccp st fndecl cfg #:cond->string f) -> (values cfg st)
;;
;; Follows the GCC source function by function (likely_value,
;; surely_varying_stmt_p, get_default_value, ccp_lattice_meet,
;; set_lattice_value, get_value_for_expr, bit_value_unop/binop,
;; evaluate_stmt, visit_phi/visit_assignment/visit_cond_stmt,
;; ccp_folder::fold_stmt, substitute_and_fold_dom_walker::
;; before_dom_children, simple_dce_from_worklist).  Not ported: the
;; built-in function cases of evaluate_stmt (malloc/alloca/assume_
;; aligned/bswap/...), nonzero-bits/range info (none exists at ccp1),
;; ipa-cp parameter bits, the pointer alignment of anything but &DECL
;; (the dump carries no DECL_ALIGN: TYPE_ALIGN of the object is used),
;; EH and abnormal edges, match.pd beyond constant folding (opt/fold.rkt).
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../../../driver/dominance/dom.rkt"
         "../state.rkt"
         "../tree.rkt"
         "../gimple.rkt"
         "../gimplify.rkt"
         "../cfg.rkt"
         "../ssa.rkt"
         "../print.rkt"
         "ssa-ops.rkt"
         "propagate.rkt"
         "fold.rkt")

(provide ccp)

;; ------------------------------------------------------------
;; widest_int arithmetic on exact integers
;; ------------------------------------------------------------

(define BIG 100000)
(define (zext x prec) (bitwise-and x (sub1 (arithmetic-shift 1 prec))))
(define (sext x prec)
  (define m (zext x prec))
  (if (bitwise-bit-set? m (sub1 prec)) (- m (arithmetic-shift 1 prec)) m))
(define (ext x prec unsigned?) (if unsigned? (zext x prec) (sext x prec)))
(define (bit-and-not a b) (bitwise-and a (bitwise-not b)))
(define (ctz x) (if (zero? x) BIG (let loop ([x x] [n 0]) (if (odd? x) n (loop (arithmetic-shift x -1) (add1 n))))))
(define (popcount x) (if (< x 0) BIG (let loop ([x x] [n 0]) (if (zero? x) n (loop (arithmetic-shift x -1) (+ n (bitwise-and x 1)))))))
;; wi::clz / wi::clrsb normalized to WIDTH (as the C code does with get_precision)
(define (clz-in x width) (max 0 (- width (integer-length x))))
(define (clrsb-in x width) (max 0 (- width 1 (integer-length x))))
;; unsigned comparison of widest_ints: negative numbers are the large ones
(define (scmp a b) (cond [(< a b) -1] [(> a b) 1] [else 0]))
(define (ucmp a b) (cond [(and (< a 0) (>= b 0)) 1] [(and (>= a 0) (< b 0)) -1] [else (scmp a b)]))
(define (wcmp a b unsigned?) (if unsigned? (ucmp a b) (scmp a b)))
(define (ltu? a b) (< (ucmp a b) 0))
(define (neg? x unsigned?) (and (not unsigned?) (< x 0)))
(define (mask-low n) (sub1 (arithmetic-shift 1 n)))          ; wi::mask (n, false)
(define (mask-high n) (bitwise-not (mask-low n)))            ; wi::mask (n, true)
(define (lrotate x n width)
  (define ux (zext x width))
  (zext (bitwise-ior (arithmetic-shift ux n) (arithmetic-shift ux (- n width))) width))
(define (rrotate x n width) (lrotate x (- width n) width))
(define (exact-log2 x) (if (and (> x 0) (= x (arithmetic-shift 1 (sub1 (integer-length x))))) (sub1 (integer-length x)) -1))
(define (udiv-trunc a b) (quotient a b))

;; ------------------------------------------------------------
;; Lattice
;; ------------------------------------------------------------

(struct lv (lat value mask) #:transparent)      ; lat: 'uninitialized 'undefined 'constant 'varying
(define UNINIT (lv 'uninitialized #f 0))
(define (varying) (lv 'varying #f -1))
(define (undefined) (lv 'undefined #f 0))
(define (lat? v l) (eq? (lv-lat v) l))
(define (icst? t) (and t (eq? (tree-code t) 'integer_cst)))

;; pass context: the state, the input CFG, its dominators, the lattice
;; (vector indexed by SSA version) and the engine once it exists
(struct C (st cfg dom vals [eng #:mutable] [messages #:mutable] [stamp #:mutable]))   ; messages: dump_file lines, reversed; stamp: next use-list stamp

(define (type-prec c ty) (type-precision* (C-st c) ty))
(define (type-uns? c ty) (type-unsigned?* (C-st c) ty))
(define (int-or-ptr? c ty) (or (integral-type-p (C-st c) ty) (pointer-type-p (C-st c) ty)))
(define (ssa-ver t) (ssa-name-version t))

;; prop_simulate_again_p (SSA_NAME_DEF_STMT (name)); a default def has no statement
(define (sim-again-def? c name)
  (define u (eng-def-uid (C-eng c) name))
  (and u (eng-simulate-again? (C-eng c) u)))

;; get_default_value
(define (get-default-value c name)
  (define st (C-st c))
  (define g (eng-def-stmt (C-eng c) name))
  (cond
    [(not g)
     (define v (ssa-name-var name))
     (if (and (not (ssa-virtual? name)) v (var-p v)) (undefined) (varying))]
    [(gassign? g)
     (define rhs1 (gassign-rhs1 g))
     (define cst (and (gassign-single-p st g) (decl-p st rhs1) (get-symbol-constant-value st rhs1)))
     (if cst (lv 'constant cst 0) (undefined))]
    [(or (and (gcall? g) (gcall-lhs g)) (gphi? g)) (undefined)]
    [else (varying)]))

;; get_value: #f for a name outside the table
(define (get-value c name)
  (define vals (C-vals c))
  (define i (ssa-ver name))
  (cond
    [(>= i (vector-length vals)) #f]
    [else
     (when (lat? (vector-ref vals i) 'uninitialized) (vector-set! vals i (get-default-value c name)))
     (vector-ref vals i)]))

;; get_constant_value
(define (get-constant-value c t)
  (cond
    [(not (ssa-name? t)) (and (is-gimple-min-invariant (C-st c) t) t)]
    [else
     (define v (get-value c t))
     (and v (lat? v 'constant) (or (not (icst? (lv-value v))) (zero? (lv-mask v))) (lv-value v))]))

(define (set-value-varying! c name) (vector-set! (C-vals c) (ssa-ver name) (varying)))

;; get_value_from_alignment: &DECL by TYPE_ALIGN of the object
(define (get-value-from-alignment c expr)
  (define st (C-st c))
  (define type (tree-type expr))
  (define prec (type-prec c type))
  (define obj (tree-operand expr 0))
  (define align-bits
    (if (and obj (decl-p st obj) (tree-type obj)) (or (type-attr st (tree-type obj) 'align #f) 8) 8))
  (define mask (bit-and-not (if (or (pointer-type-p st type) (type-uns? c type)) (mask-low prec) -1)
                            (sub1 (quotient align-bits 8))))
  (if (= (sext mask prec) -1)
      (varying)
      (lv 'constant (build-int-cst st type 0) mask)))

;; get_value_for_expr
(define (get-value-for-expr c expr for-bits?)
  (define st (C-st c))
  (define val
    (cond
      [(ssa-name? expr)
       (define v0 (or (get-value c expr) (varying)))
       (define v1
         (cond
           [(and for-bits? (lat? v0 'constant))
            (cond
              [(eq? (tree-code (lv-value v0)) 'addr_expr) (get-value-from-alignment c (lv-value v0))]
              [(not (icst? (lv-value v0))) (varying)]
              [else v0])]
           [else v0]))
       (if (and (not for-bits?) (lat? v1 'varying)) (lv 'constant expr -1) v1)]
      [(and (is-gimple-min-invariant st expr) (or (not for-bits?) (icst? expr))) (lv 'constant expr 0)]
      [(eq? (tree-code expr) 'addr_expr) (get-value-from-alignment c expr)]
      [else (varying)]))
  (define ty (tree-type expr))
  (if (and (lat? val 'varying) (integral-type-p st ty) (type-uns? c ty))
      (lv 'varying (lv-value val) (zext (lv-mask val) (type-prec c ty)))
      val))

;; ccp_lattice_meet: the meet of V1 and V2 (V1 is the accumulator)
(define (ccp-lattice-meet c v1 v2)
  (define (copy? v) (and (lat? v 'constant) (ssa-name? (lv-value v))))
  (cond
    [(and (lat? v1 'undefined) (not (copy? v2))) v2]
    [(and (lat? v2 'undefined) (not (copy? v1))) v1]
    [(or (lat? v1 'varying) (lat? v2 'varying)) (varying)]
    [(and (lat? v1 'constant) (lat? v2 'constant) (icst? (lv-value v1)) (icst? (lv-value v2)))
     (define mask (bitwise-ior (lv-mask v1) (lv-mask v2)
                               (bitwise-xor (integer-cst-value (lv-value v1)) (integer-cst-value (lv-value v2)))))
     (if (= (sext mask (type-prec c (tree-type (lv-value v1)))) -1)
         (varying)
         (lv 'constant (lv-value v1) mask))]
    [(and (lat? v1 'constant) (lat? v2 'constant) (equal? (lv-value v1) (lv-value v2))) v1]
    [(and (lat? v1 'constant) (lat? v2 'constant)
          (or (eq? (tree-code (lv-value v1)) 'addr_expr) (eq? (tree-code (lv-value v2)) 'addr_expr)))
     (define a (if (eq? (tree-code (lv-value v1)) 'addr_expr) (get-value-for-expr c (lv-value v1) #t) v1))
     (define b (if (eq? (tree-code (lv-value v2)) 'addr_expr) (get-value-for-expr c (lv-value v2) #t) v2))
     (ccp-lattice-meet c a b)]
    [else (varying)]))

;; set_lattice_value: #t when the value changed
(define (set-lattice-value! c name new0)
  (define vals (C-vals c))
  (define i (ssa-ver name))
  (define old (vector-ref vals i))
  ;; GCC >= 13 (r13-3875 "Restore CCP copy propagation"): no meet for a
  ;; constant -> copy transition
  (define new
    (if (and (not (lat? old 'uninitialized))
             (not (and (>= (gcc-major-version (C-st c)) 13)
                       (lat? old 'constant) (constant-class-p (C-st c) (lv-value old))
                       (lat? new0 'constant) (ssa-name? (lv-value new0)))))
        (ccp-lattice-meet c new0 old)
        new0))
  (define changed?
    (or (not (eq? (lv-lat old) (lv-lat new)))
        (and (lat? new 'constant)
             (or (not (eq? (tree-code (lv-value new)) (tree-code (lv-value old))))
                 (and (icst? (lv-value new))
                      (or (not (= (lv-mask new) (lv-mask old)))
                          (not (= (bit-and-not (integer-cst-value (lv-value old)) (lv-mask new))
                                  (bit-and-not (integer-cst-value (lv-value new)) (lv-mask new))))))
                 (and (not (icst? (lv-value new))) (not (equal? (lv-value new) (lv-value old))))))))
  (when changed? (vector-set! vals i new))
  changed?)

;; ------------------------------------------------------------
;; likely_value, surely_varying_stmt_p, ccp_initialize
;; ------------------------------------------------------------

;; the non-SSA operands likely_value scans (gimple_op from the first rhs)
(define (non-ssa-operands g)
  (define ops
    (cond
      [(gassign? g) (list (gassign-rhs1 g) (gassign-rhs2 g) (gassign-rhs3 g))]
      [(gcall? g) (pvector->list (gcall-args g))]
      [(gcond? g) (list (gcond-lhs g) (gcond-rhs g))]
      [(gswitch? g) (list (gswitch-index g))]
      [else '()]))
  (for/list ([o (in-list ops)] #:when (and o (not (ssa-name? o)))) o))

(define (likely-value c g)
  (define st (C-st c))
  (cond
    [(stmt-has-volatile-ops? g) 'varying]
    [else
     (define has-constant #f) (define has-undefined #f) (define all-undefined #t) (define has-nsa #f)
     (for ([u (in-list (stmt-real-uses g))])
       (define v (get-value c u))
       (if (and v (lat? v 'undefined)) (set! has-undefined #t) (set! all-undefined #f))
       (when (and v (lat? v 'constant)) (set! has-constant #t))
       (when (or (ssa-default-def? u) (not (sim-again-def? c u))) (set! has-nsa #t)))
     (for ([op (in-list (non-ssa-operands g))])
       (cond
         [(is-gimple-min-invariant st op) (set! has-constant #t)]
         [(eq? (tree-code op) 'constructor)
          (when (for/or ([e (in-pvector (constructor-elts op))])
                  (define v (cond [(pair? e) (cdr e)] [(Tree? e) (tree-attr e 'value #f)] [else #f]))
                  (and (Tree? v) (constant-class-p st v)))
            (set! has-constant #t))]))
     (when has-constant (set! all-undefined #f))
     (cond
       [(and has-undefined all-undefined) 'undefined]
       [(and (gassign? g) has-undefined
             (memq (gassign-code g) '(plus_expr minus_expr pointer_plus_expr bit_xor_expr addr_expr)))
        'undefined]
       [has-undefined 'constant]
       [(or has-constant has-nsa (stmt-vuse g)) 'constant]
       [else 'varying])]))

(define (surely-varying-stmt-p c g)
  (cond
    [(stmt-has-volatile-ops? g) #t]
    [(gcall? g)
     (define d (call-fndecl g))
     (or (not (gcall-lhs g)) (and d (not (call-fndecl-builtin? d))))]
    [(stmt-vdef g) #t]
    [else (not (or (gassign? g) (gcond? g) (gswitch? g)))]))

(define (ccp-initialize! c e)
  (define cfg (C-cfg c))
  (for* ([b (in-pvector (cfg-bbs cfg))] [uid (in-list (cdr (eng-uids-of-block e b)))])
    (define g (eng-stmt e uid))
    (define is-varying (if (stmt-ends-bb? g) #f (surely-varying-stmt-p c g)))
    (when is-varying (for ([d (in-list (stmt-all-defs g))]) (set-value-varying! c d)))
    (eng-set-simulate-again! e uid (not is-varying)))
  (for* ([b (in-pvector (cfg-bbs cfg))] [uid (in-list (car (eng-uids-of-block e b)))])
    (eng-set-simulate-again! e uid (not (ssa-virtual? (gphi-result (eng-stmt e uid)))))))

;; ------------------------------------------------------------
;; ccp_fold
;; ------------------------------------------------------------

(define (valueize-op c t) (or (and (ssa-name? t) (get-constant-value c t)) t))

(define (fold-stmt-to-constant-1 c g)
  (define st (C-st c))
  (define (valueize t) (valueize-op c t))
  (or (gimple-simplify-const st g valueize)
      (cond
        [(gassign? g)
         (case (gimple-rhs-class st (gassign-code g))
           [(single)
            (define rhs (gassign-rhs1 g))
            (cond
              [(ssa-name? rhs) (valueize rhs)]
              [(eq? (tree-code rhs) 'constructor) #f]
              [(eq? (tree-code-class st (tree-code rhs)) 'tcc_reference) #f]     ; fold_const_aggregate_ref_1
              [(decl-p st rhs) (get-symbol-constant-value st rhs)]
              [else rhs])]
           [(binary)
            (define code (gassign-code g))
            (cond
              [(memq code '(eq_expr ne_expr))
               (define lhs (gassign-lhs g)) (define op0 (gassign-rhs1 g))
               (and (useless-type-conversion-p st (tree-type lhs) (tree-type op0))
                    (let* ([op1 (valueize (gassign-rhs2 g))]
                           [op0* (valueize op0)])
                      (define-values (a b) (if (icst? op0*) (values op1 op0*) (values op0* op1)))
                      (and (icst? b)
                           (or (and (eq? code 'ne_expr) (integer-zerop b)) (and (eq? code 'eq_expr) (integer-onep b)))
                           a)))]
              [else #f])]
           [(ternary)
            (define op0 (valueize (gassign-rhs1 g)))
            (and (eq? (gassign-code g) 'cond_expr) (icst? op0)
                 (valueize (if (integer-zerop op0) (gassign-rhs3 g) (gassign-rhs2 g))))]
           [else #f])]
        [else #f])))

(define (ccp-fold c g)
  (if (gswitch? g) (valueize-op c (gswitch-index g)) (fold-stmt-to-constant-1 c g)))

;; ------------------------------------------------------------
;; bit_value_unop / bit_value_binop
;; ------------------------------------------------------------

;; value_mask_to_min_max -> (values min max)
(define (value-mask-to-min-max val mask unsigned? prec)
  (define mn (bit-and-not val mask))
  (define mx (bitwise-ior val mask))
  (cond
    [(and (not unsigned?) (< mask 0))
     (define sign-bit (arithmetic-shift 1 (sub1 prec)))
     (values (sext (bitwise-xor mn sign-bit) prec) (sext (bitwise-xor mx sign-bit) prec))]
    [else (values mn mx)]))

;; -> (values val mask)
(define (bv-unop code uns prec rval rmask runs rprec)
  (case code
    [(bit_not_expr) (values (bitwise-not rval) rmask)]
    [(negate_expr)
     (define-values (tv tm) (bv-unop 'bit_not_expr uns prec rval rmask uns prec))
     (bv-binop 'plus_expr uns prec tv tm uns prec 1 0 uns prec)]
    [(nop_expr convert_expr)
     (define m (ext rmask rprec runs)) (define v (ext rval rprec runs))
     (values (ext v prec uns) (ext m prec uns))]
    [(abs_expr absu_expr)
     (cond
       [(= (sext rmask rprec) -1) (values 0 -1)]
       [(< rmask 0)
        (define-values (tv tm) (bv-unop 'negate_expr runs rprec rval rmask uns prec))
        (define tm* (bitwise-ior tm rmask (bitwise-xor rval tv)))
        (values (ext tv prec uns) (ext tm* prec uns))]
       [(< rval 0) (bv-unop 'negate_expr uns prec rval rmask uns prec)]
       [else (values rval rmask)])]
    [else (values 0 -1)]))

;; bit_value_mult_const -> (values val mask)
(define (bv-mult-const uns width rval rmask c0)
  (define rval-lo (bit-and-not rval rmask))
  (cond
    [(not (zero? rval-lo))
     (let loop ([c c0] [sum-val 0] [sum-mask 0])
       (cond
         [(zero? c) (values (ext sum-val width uns) (ext sum-mask width uns))]
         [else
          (define bitpos (ctz c))
          (define term-mask (arithmetic-shift rmask bitpos))
          (define term-val (arithmetic-shift rval-lo bitpos))
          (define lo (+ sum-val term-val))
          (define hi (+ (bitwise-ior sum-val sum-mask) (bitwise-ior term-val term-mask)))
          (loop (bitwise-xor c (arithmetic-shift 1 bitpos)) lo
                (bitwise-ior sum-mask term-mask (bitwise-xor lo hi)))]))]
    [else
     (let loop ([c c0] [sum-mask 0])
       (cond
         [(zero? c) (values 0 (ext sum-mask width uns))]
         [else
          (define bitpos (ctz c))
          (define term-mask (arithmetic-shift rmask bitpos))
          (define hi (+ sum-mask term-mask))
          (loop (bitwise-xor c (arithmetic-shift 1 bitpos)) (bitwise-ior sum-mask term-mask hi))]))]))

;; get_individual_bits: up to MAX single-bit values of X, lowest first
(define (individual-bits x max)
  (let loop ([x x] [acc '()] [n 0])
    (if (or (>= n max) (zero? x))
        (reverse acc)
        (let ([b (arithmetic-shift 1 (ctz x))]) (loop (bitwise-xor x b) (cons b acc) (add1 n))))))

(define gray-code-bit-flips
  #(0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 5
    0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0))

;; bit_value_binop (the widest_int level) -> (values val mask)
(define (bv-binop code0 uns width r1val r1mask r1uns r1prec r2val r2mask r2uns r2prec)
  (define swap? (memq code0 '(ge_expr gt_expr)))
  (define code (case code0 [(ge_expr) 'le_expr] [(gt_expr) 'lt_expr] [else code0]))
  (define (rshift x n) (arithmetic-shift x (- n)))
  (define (lshift x n) (arithmetic-shift x n))
  (case code
    [(bit_and_expr)
     (values (bitwise-and r1val r2val)
             (bitwise-and (bitwise-ior r1mask r2mask) (bitwise-ior r1val r1mask) (bitwise-ior r2val r2mask)))]
    [(bit_ior_expr)
     (values (bitwise-ior r1val r2val)
             (bit-and-not (bitwise-ior r1mask r2mask)
                          (bitwise-ior (bit-and-not r1val r1mask) (bit-and-not r2val r2mask))))]
    [(bit_xor_expr) (values (bitwise-xor r1val r2val) (bitwise-ior r1mask r2mask))]
    [(lrotate_expr rrotate_expr)
     (cond
       [(zero? r2mask)
        (cond
          [(zero? r2val) (values r1val r1mask)]
          [else
           (define-values (shift code*)
             (if (neg? r2val r2uns)
                 (values (- r2val) (if (eq? code 'rrotate_expr) 'lrotate_expr 'rrotate_expr))
                 (values r2val code)))
           (define rot (if (eq? code* 'rrotate_expr) rrotate lrotate))
           (values (ext (rot r1val shift width) width uns) (ext (rot r1mask shift width) width uns))])]
       [(and (ltu? (bitwise-ior r2val r2mask) width) (<= (popcount r2mask) 4))
        (define rot (if (eq? code 'rrotate_expr) rrotate lrotate))
        (define bits (list->vector (individual-bits r2mask 4)))
        (define count (sub1 (arithmetic-shift 1 (vector-length bits))))
        (define shift0 (bit-and-not r2val r2mask))
        (let loop ([i 0] [shift shift0] [res-val (rot r1val shift0 width)] [res-mask (rot r1mask shift0 width)])
          (cond
            [(>= i count) (values (ext (bit-and-not res-val res-mask) width uns) (ext res-mask width uns))]
            [else
             (define shift* (bitwise-xor shift (vector-ref bits (vector-ref gray-code-bit-flips i))))
             (define tv (rot r1val shift* width)) (define tm (rot r1mask shift* width))
             (loop (add1 i) shift* res-val (bitwise-ior res-mask tm (bitwise-xor res-val tv)))]))]
       [else (values 0 -1)])]
    [(lshift_expr rshift_expr)
     (cond
       [(zero? r2mask)
        (cond
          [(zero? r2val) (values r1val r1mask)]
          [(neg? r2val r2uns) (values 0 -1)]
          [(eq? code 'rshift_expr)
           (values (rshift (ext r1val width uns) r2val) (rshift (ext r1mask width uns) r2val))]
          [else (values (ext (lshift r1val r2val) width uns) (ext (lshift r1mask r2val) width uns))])]
       [(ltu? (bitwise-ior r2val r2mask) width)
        (cond
          [(<= (popcount r2mask) 4)
           (define bits (list->vector (individual-bits r2mask 4)))
           (define count (sub1 (arithmetic-shift 1 (vector-length bits))))
           (define shift0 (bit-and-not r2val r2mask))
           (define-values (arg-val arg-mask sh)
             (if (eq? code 'rshift_expr)
                 (values (ext r1val width uns) (ext r1mask width uns) rshift)
                 (values r1val r1mask lshift)))
           (let loop ([i 0] [shift shift0] [res-val (sh arg-val shift0)] [res-mask (sh arg-mask shift0)])
             (cond
               [(>= i count)
                (define rm (ext res-mask width uns)) (define rv (ext res-val width uns))
                (values (bit-and-not rv rm) rm)]
               [else
                (define shift* (bitwise-xor shift (vector-ref bits (vector-ref gray-code-bit-flips i))))
                (define tv (sh arg-val shift*)) (define tm (sh arg-mask shift*))
                (loop (add1 i) shift* res-val (bitwise-ior res-mask tm (bitwise-xor res-val tv)))]))]
          [(zero? (bitwise-ior r1val r1mask)) (values 0 0)]
          [(eq? code 'lshift_expr)
           (define tmp (lshift (lshift (mask-low width) (ctz (bitwise-ior r1val r1mask))) (bit-and-not r2val r2mask)))
           (values 0 (ext tmp width uns))]
          [(not (neg? (bitwise-ior r1val r1mask) uns))
           (define arg (bitwise-ior r1val r1mask))
           (define lz (clz-in (zext arg width) width))
           (define tmp (rshift (rshift (mask-low width) lz) (bit-and-not r2val r2mask)))
           (values 0 (ext tmp width uns))]
          [(not (< r1mask 0))
           (define arg (bit-and-not r1val r1mask))
           (define sb (clrsb-in (sext arg width) width))
           (define tmp (rshift (rshift (mask-low width) sb) (bit-and-not r2val r2mask)))
           (values (sext (bitwise-not tmp) width) (sext tmp width))]
          [else (values 0 -1)])]
       [else (values 0 -1)])]
    [(plus_expr pointer_plus_expr)
     (define lo (ext (+ (bit-and-not r1val r1mask) (bit-and-not r2val r2mask)) width uns))
     (define hi (ext (+ (bitwise-ior r1val r1mask) (bitwise-ior r2val r2mask)) width uns))
     (values lo (ext (bitwise-ior r1mask r2mask (bitwise-xor lo hi)) width uns))]
    [(minus_expr pointer_diff_expr)
     (define lo (ext (- (bit-and-not r1val r1mask) (bitwise-ior r2val r2mask)) width uns))
     (define hi (ext (- (bitwise-ior r1val r1mask) (bit-and-not r2val r2mask)) width uns))
     (values lo (ext (bitwise-ior r1mask r2mask (bitwise-xor lo hi)) width uns))]
    [(mult_expr)
     (cond
       [(and (zero? r2mask) (not (neg? r2val uns)) (< (popcount r2val) 8))
        (bv-mult-const uns width r1val r1mask r2val)]
       [(and (zero? r1mask) (not (neg? r1val uns)) (< (popcount r1val) 8))
        (bv-mult-const uns width r2val r2mask r1val)]
       [else
        (define r1tz (ctz (bitwise-ior r1val r1mask)))
        (define r2tz (ctz (bitwise-ior r2val r2mask)))
        (cond
          [(>= (+ r1tz r2tz) width) (values 0 0)]
          [(> (+ r1tz r2tz) 0) (values 0 (ext (mask-high (+ r1tz r2tz)) width uns))]
          [else (values 0 -1)])])]
    [(eq_expr ne_expr)
     (define m (bitwise-ior r1mask r2mask))
     (if (not (= (bit-and-not r1val m) (bit-and-not r2val m)))
         (values (if (eq? code 'eq_expr) 0 1) 0)
         (values 0 1))]
    [(lt_expr le_expr)
     (define-values (o1val o1mask o2val o2mask)
       (if swap? (values r2val r2mask r1val r1mask) (values r1val r1mask r2val r2mask)))
     (define-values (min1 max1) (value-mask-to-min-max o1val o1mask r1uns r1prec))
     (define-values (min2 max2) (value-mask-to-min-max o2val o2mask r1uns r1prec))
     (define maxmin (wcmp max1 min2 r1uns))
     (define minmax (wcmp min1 max2 r1uns))
     (cond
       [(< maxmin (if (eq? code 'le_expr) 1 0)) (values 1 0)]
       [(> minmax (if (eq? code 'lt_expr) -1 0)) (values 0 0)]
       [(= maxmin minmax) (values (if (eq? code 'le_expr) 1 0) 0)]
       [else (values 0 1)])]
    [(min_expr max_expr)
     (define-values (min1 max1) (value-mask-to-min-max r1val r1mask uns width))
     (define-values (min2 max2) (value-mask-to-min-max r2val r2mask uns width))
     (cond
       [(<= (wcmp max1 min2 uns) 0) (if (eq? code 'min_expr) (values r1val r1mask) (values r2val r2mask))]
       [(>= (wcmp min1 max2 uns) 0) (if (eq? code 'min_expr) (values r2val r2mask) (values r1val r1mask))]
       [else (values r1val (bitwise-ior r1mask r2mask (bitwise-xor r1val r2val)))])]
    [(trunc_mod_expr)
     (define r1max (bitwise-ior r1val r1mask))
     (define r2max (bitwise-ior r2val r2mask))
     (define shift (if (zero? r2mask) (exact-log2 r2val) -1))
     (define-values (tv tm)
       (if (not (= shift -1))
           (bv-binop 'bit_and_expr uns width r1val r1mask r1uns r1prec (sub1 r2val) r2mask r2uns r2prec)
           (values #f #f)))
     (cond
       [(and tv (or uns (not (< r1max 0)) (and (zero? tm) (zero? tv)))) (values tv tm)]
       [(or uns (and (not (< r1max 0)) (not (< r2max 0))))
        (define r2min (bit-and-not r2val r2mask))
        (cond
          [(zero? r2min) (values 0 -1)]
          [(ltu? r1max r2min) (values r1val r1mask)]
          [else
           (define bits0 (integer-length (zext r2max width)))
           (define bits (if (= r2max (arithmetic-shift 1 bits0)) (sub1 bits0) bits0))
           (values 0 (mask-low bits))])]
       [else (values 0 -1)])]
    [(exact_div_expr trunc_div_expr)
     (define r1max (bitwise-ior r1val r1mask))
     (define r2max (bitwise-ior r2val r2mask))
     (define shift
       (if (and (zero? r2mask) (or (eq? code 'exact_div_expr) uns (not (< r1max 0)))) (exact-log2 r2val) -1))
     (cond
       [(not (= shift -1))
        (bv-binop 'rshift_expr uns width r1val r1mask r1uns r1prec shift r2mask r2uns r2prec)]
       [(or uns (and (not (< r1max 0)) (not (< r2max 0))))
        (define r2min (bit-and-not r2val r2mask))
        (cond
          [(zero? r2min) (values 0 -1)]
          [(ltu? r1max r2min) (values 0 0)]
          [else
           (define upper (udiv-trunc (zext r1max width) r2min))
           (values 0 (mask-low (integer-length upper)))])]
       [else (values 0 -1)])]
    [else (values 0 -1)]))

(define (value-to-wide-int v) (if (icst? (lv-value v)) (integer-cst-value (lv-value v)) 0))

(define (bit-value-unop c code type rhs)
  (define rval (get-value-for-expr c rhs #t))
  (cond
    [(lat? rval 'undefined) rval]
    [else
     (define rtype (tree-type rhs))
     (define prec (type-prec c type))
     (define-values (value mask)
       (bv-unop code (type-uns? c type) prec (value-to-wide-int rval) (lv-mask rval) (type-uns? c rtype) (type-prec c rtype)))
     (if (not (= (sext mask prec) -1))
         (lv 'constant (build-int-cst (C-st c) type value) mask)
         (varying))]))

(define (bit-value-binop c code type rhs1 rhs2)
  (define r1 (get-value-for-expr c rhs1 #t))
  (define r2 (get-value-for-expr c rhs2 #t))
  (cond
    [(or (lat? r1 'undefined) (lat? r2 'undefined)) (varying)]
    [else
     (define t1 (tree-type rhs1)) (define t2 (tree-type rhs2))
     (define prec (type-prec c type))
     (define-values (value0 mask0)
       (bv-binop code (type-uns? c type) prec
                 (value-to-wide-int r1) (lv-mask r1) (type-uns? c t1) (type-prec c t1)
                 (value-to-wide-int r2) (lv-mask r2) (type-uns? c t2) (type-prec c t2)))
     ;; x * x is never 2 mod 4
     (define-values (value mask)
       (if (and (eq? code 'mult_expr) (equal? rhs1 rhs2) (> prec 1))
           (values (if (not (= (sext mask0 prec) -1)) (bit-and-not value0 2) 0) (bit-and-not mask0 2))
           (values value0 mask0)))
     (if (not (= (sext mask prec) -1))
         (lv 'constant (build-int-cst (C-st c) type value) mask)
         (varying))]))

;; ------------------------------------------------------------
;; evaluate_stmt
;; ------------------------------------------------------------

(define (evaluate-stmt c g)
  (define st (C-st c))
  (define likely (likely-value c g))
  (define simplified #f)
  (define is-constant #f)
  (define val (varying))
  (define result #f)
  (case likely
    [(constant)
     (set! simplified (ccp-fold c g))
     (when (and simplified (ssa-name? simplified))
       (cond
         [(or (ssa-default-def? simplified) (not (sim-again-def? c simplified)))
          (define v (get-value c simplified))
          (when (and v (not (lat? v 'varying))) (set! result v))]
         [else (set! simplified #f)]))
     (unless result
       (set! is-constant (and simplified (is-gimple-min-invariant st simplified) #t))
       (when is-constant (set! result (lv 'constant simplified 0))))]
    [(varying)
     (cond
       [(gassign? g) (when (gassign-single-p st g) (set! simplified (gassign-rhs1 g)))]
       [(gswitch? g) (set! simplified (gswitch-index g))])
     (set! is-constant (and simplified (is-gimple-min-invariant st simplified) #t))
     (when is-constant (set! val (lv 'constant simplified 0)))]
    [else (set! result (undefined))])
  (cond
    [result result]
    [else
     ;; bitwise tracking (flag_tree_bit_ccp)
     (when (and (not is-constant)
                (or (eq? likely 'constant) (gcall? g)
                    (and (gassign-single-p st g) (eq? (gassign-code g) 'addr_expr))))
       (set! val (varying))
       (cond
         [(gassign? g)
          (define lhs-ty (tree-type (gassign-lhs g)))
          (define rhs1 (gassign-rhs1 g))
          (when (and (int-or-ptr? c lhs-ty) (int-or-ptr? c (tree-type rhs1)))
            (case (gimple-rhs-class st (gassign-code g))
              [(single) (set! val (get-value-for-expr c rhs1 #t))]
              [(unary) (set! val (bit-value-unop c (gassign-code g) lhs-ty rhs1))]
              [(binary) (set! val (bit-value-binop c (gassign-code g) lhs-ty rhs1 (gassign-rhs2 g)))]
              [else (void)]))]
         [(gcond? g)
          (define ty (tree-type (gcond-lhs g)))
          (when (int-or-ptr? c ty)
            (set! val (bit-value-binop c (gcond-code g) ty (gcond-lhs g) (gcond-rhs g))))]
         [else (void)])
       (set! is-constant (lat? val 'constant)))
     (cond
       [is-constant val]
       [(and simplified (ssa-name? simplified)) (lv 'constant simplified -1)]
       [else (varying)])]))

;; ------------------------------------------------------------
;; visitors
;; ------------------------------------------------------------

(define (visit-assignment c g)
  (define lhs (stmt-get-lhs g))
  (cond
    [(ssa-name? lhs)
     (define val (evaluate-stmt c g))
     (cond
       [(set-lattice-value! c lhs val) (values (if (lat? val 'varying) 'varying 'interesting) #f lhs)]
       [else (values 'not-interesting #f #f)])]
    [else (values 'not-interesting #f #f)]))

(define (visit-cond-stmt c bb g)
  (define val (evaluate-stmt c g))
  (cond
    [(or (not (lat? val 'constant)) (not (zero? (lv-mask val)))) (values 'varying #f #f)]
    [else
     (define e (find-taken-edge (C-st c) (C-cfg c) bb (lv-value val)))
     (if e (values 'interesting (GEdge-dest e) #f) (values 'varying #f #f))]))

(define (visit-stmt c bb g)
  (cond
    [(gassign? g) (visit-assignment c g)]
    [(and (gcall? g) (gcall-lhs g)) (visit-assignment c g)]
    [(or (gcond? g) (gswitch? g)) (visit-cond-stmt c bb g)]
    [else
     (for ([d (in-list (stmt-all-defs g))]) (set-value-varying! c d))
     (values 'varying #f #f)]))

(define (visit-phi c bb ph)
  (define e (C-eng c))
  (define dom (C-dom c))
  (define-values (new-val non-exec)
    (let loop ([args (pvector->list (gphi-args ph))] [new-val (undefined)] [non-exec #f] [first #t])
      (cond
        [(or (null? args) (lat? new-val 'varying)) (values new-val non-exec)]
        [(eng-edge-executable? e (car (car args)) bb)
         (define arg-val (get-value-for-expr c (cdr (car args)) #f))
         (loop (cdr args) (if first arg-val (ccp-lattice-meet c new-val arg-val)) non-exec #f)]
        [else (loop (cdr args) new-val #t first)])))
  (define new-val*
    (if (and non-exec (lat? new-val 'constant) (ssa-name? (lv-value new-val))
             (not (ssa-default-def? (lv-value new-val)))
             (let ([du (eng-def-uid e (lv-value new-val))])
               (and du (not (dom-dominates? dom (dom-index dom (eng-uid-bb e du)) (dom-index dom bb))))))
        (varying)
        new-val))
  (cond
    [(set-lattice-value! c (gphi-result ph) new-val*) (values (if (lat? new-val* 'varying) 'varying 'interesting) (gphi-result ph))]
    [else (values 'not-interesting #f)]))

;; ------------------------------------------------------------
;; ccp_finalize: substitute_and_fold + simple_dce_from_worklist
;; ------------------------------------------------------------

;; may_propagate_copy (no abnormal edges here)
(define (may-propagate-copy c dest orig)
  (and (useless-type-conversion-p (C-st c) (tree-type dest) (tree-type orig))
       (not (and (ssa-name? dest) (ssa-virtual? dest)))))

(define (gimple-cond-make c g true?)
  (define st (C-st c))
  (gcond (gcond-loc g) (gcond-flags g) 'ne_expr
         (build-int-cst st (boolean-type st) (if true? 1 0)) (build-int-cst st (boolean-type st) 0)
         (gcond-true-label g) (gcond-false-label g)))

;; commutative_tree_code / swap_tree_comparison
(define commutative-codes
  '(plus_expr mult_expr mult_highpart_expr min_expr max_expr bit_ior_expr bit_xor_expr bit_and_expr
    ne_expr eq_expr unordered_expr ordered_expr uneq_expr ltgt_expr truth_and_expr truth_xor_expr truth_or_expr
    widen_mult_expr))
(define (swap-tree-comparison code)
  (case code
    [(lt_expr) 'gt_expr] [(gt_expr) 'lt_expr] [(le_expr) 'ge_expr] [(ge_expr) 'le_expr]
    [(unlt_expr) 'ungt_expr] [(ungt_expr) 'unlt_expr] [(unle_expr) 'unge_expr] [(unge_expr) 'unle_expr]
    [else code]))

;; fold_stmt (gimple-fold.cc): constant folding and the operand-order
;; canonicalization of gimple_resimplify2: (values g changed?)
(define (fold-stmt-generic c g)
  (define st (C-st c))
  (define r (gimple-simplify-const st g values))
  (define (swap? code a b)
    (and (or (comparison-class-p st (make-tree code)) (memq code commutative-codes))
         (tree-swap-operands-p st a b)))
  (cond
    [r
     (cond
       [(gcond? g) (values (gimple-cond-make c g (not (integer-zerop r))) #t)]
       [(gassign? g) (values (gassign (gassign-loc g) (gassign-flags g) (gassign-lhs g) (tree-code r) r #f #f) #t)]
       [else (values g #f)])]
    [(and (gcond? g) (swap? (gcond-code g) (gcond-lhs g) (gcond-rhs g)))
     (values (gcond (gcond-loc g) (gcond-flags g) (swap-tree-comparison (gcond-code g)) (gcond-rhs g) (gcond-lhs g)
                    (gcond-true-label g) (gcond-false-label g))
             #t)]
    [(and (gassign? g) (eq? (gimple-rhs-class st (gassign-code g)) 'binary)
          (swap? (gassign-code g) (gassign-rhs1 g) (gassign-rhs2 g)))
     (values (gassign (gassign-loc g) (gassign-flags g) (gassign-lhs g) (swap-tree-comparison (gassign-code g))
                      (gassign-rhs2 g) (gassign-rhs1 g) #f)
             #t)]
    [else (values g #f)]))

;; ccp_folder::fold_stmt: (values g changed?)
(define (ccp-folder-fold-stmt c g)
  (define st (C-st c))
  (cond
    [(gcond? g)
     (define val (evaluate-stmt c g))
     (cond
       [(and (lat? val 'constant) (zero? (lv-mask val)))
        (set-C-messages! c (cons (format "Folding predicate ~a to ~a" (print-cond-rhs st g) (print-expr st (lv-value val)))
                                 (C-messages c)))
        (values (gimple-cond-make c g (not (integer-zerop (lv-value val)))) #t)]
       [else (values g #f)])]
    [(gcall? g)
     (define lhs (gcall-lhs g))
     (define val (and (ssa-name? lhs) (get-constant-value c lhs)))
     (define ecf (call-ecf (C-st c) g))
     (cond
       [(and val (not (zero? (bitwise-and ecf (bitwise-ior ECF-CONST ECF-PURE)))) (zero? (bitwise-and ecf ECF-LOOPING)))
        ;; gimplify_and_update_call_from_tree with a gimple value: lhs = val
        (define rhs (if (useless-type-conversion-p st (tree-type lhs) (tree-type val)) val (fold-convert st (tree-type lhs) val)))
        (values (gassign (gcall-loc g) (gcall-flags g) lhs (tree-code rhs) rhs #f #f) #t)]
       [(gcall-internal? g) (values g #f)]
       [else
        ;; constant arguments whose declared parameter type matches
        ;; TYPE_ARG_TYPES: a TREE_LIST chain of type uids
        (define argt-list
          (let loop ([p (let ([ft (gcall-fntype g)]) (and ft (type-attr st ft 'arg-types #f)))] [acc '()])
            (if (and p (Tree? p)) (loop (tree-chain p) (cons (tree-value p) acc)) (reverse acc))))
        (define changed #f)
        (define args*
          (for/pvector ([a (in-pvector (gcall-args g))] [k (in-naturals)])
            (define argt-uid (and (< k (length argt-list)) (list-ref argt-list k)))
            (define v (and (ssa-name? a) (get-constant-value c a)))
            (cond
              [(and v (exact-integer? argt-uid)
                    (useless-type-conversion-p st (type-main-variant st argt-uid) (type-main-variant st (tree-type v))))
               (set! changed #t) v]
              [else a])))
        (values (if changed (gcall (gcall-loc g) (gcall-flags g) lhs (gcall-fn g) args* (gcall-fntype g) (gcall-internal? g) ecf) g)
                changed)])]
    [(gassign? g)
     (define lhs (gassign-lhs g))
     (define val (and (gassign-single-p st g) (ssa-name? lhs) (get-constant-value c lhs)))
     (cond
       [val
        (define rhs (if (useless-type-conversion-p st (tree-type lhs) (tree-type val)) val (build1 st 'view_convert_expr (tree-type lhs) val)))
        (values (gassign (gassign-loc g) (gassign-flags g) lhs (tree-code rhs) rhs #f #f) #t)]
       [else (values g #f)])]
    [else (values g #f)]))

;; replace_uses_in: (values g replaced?)
(define (replace-uses-in c g)
  (define replaced #f)
  (define g*
    (stmt-map-uses (C-st c) g
                   (lambda (u)
                     (define val (get-constant-value c u))
                     (cond
                       [(and val (not (equal? val u)) (may-propagate-copy c u val)) (set! replaced #t) val]
                       [else u]))))
  (values g* replaced))

;; update_stmt / SET_USE bookkeeping: NEW replaces OLD (gimple.rkt restamp)
(define (restamp! c old new)
  (define-values (g n) (restamp old new (C-stamp c)))
  (set-C-stamp! c n)
  g)

;; substitute_and_fold_dom_walker::before_dom_children on one block:
;; (values cfg changed? dce-worklist)
(define (fold-block c cfg bb dce0)
  (define st (C-st c))
  (define e (C-eng c))
  (define changed #f)
  (define dce dce0)
  (define b (cfg-block cfg bb))
  ;; PHIs
  (define phis*
    (for/pvector ([ph (in-pvector (GBlock-phis b))])
      (define res (gphi-result ph))
      (cond
        [(ssa-virtual? res) ph]
        [else
         (define sprime (get-constant-value c res))
         (cond
           [(and sprime (not (equal? sprime res)) (may-propagate-copy c res sprime))
            (set! dce (intbits-set dce (ssa-ver res)))
            ph]
           [else
            (restamp! c ph
                      (phi-set-args ph (for/pvector ([arg (in-pvector (gphi-args ph))])
                                         (define a (cdr arg))
                                         (define val (and (ssa-name? a) (get-constant-value c a)))
                                         (cond
                                           [(and val (not (equal? val a)) (may-propagate-copy c a val)) (set! changed #t) (cons (car arg) val)]
                                           [else arg]))))])])))
  ;; statements
  (define succs (cfg-succs cfg bb))
  (define stmts*
    (for/pvector ([g (in-pvector (GBlock-stmts b))])
      (define lhs (stmt-ssa-lhs g))
      (define sprime (and lhs (get-constant-value c lhs)))
      (cond
        [(and sprime (not (equal? sprime lhs)) (may-propagate-copy c lhs sprime) (not (stmt-has-side-effects? (C-st c) g)))
         (set! dce (intbits-set dce (ssa-ver lhs)))
         g]
        [else
         (define-values (g1 replaced?) (replace-uses-in c g))
         (define-values (g2 folded1?) (if replaced? (fold-stmt-generic c g1) (values g1 #f)))
         (define-values (g3 folded2?) (ccp-folder-fold-stmt c g2))
         (define-values (g4 fixed?)
           (cond
             [(and (gcond? g3) (= 2 (pvector-length succs)))
              (define e0 (pvector-ref succs 0)) (define e1 (pvector-ref succs 1))
              (define x0 (eng-edge-executable? e bb (GEdge-dest e0)))
              (define x1 (eng-edge-executable? e bb (GEdge-dest e1)))
              (cond
                [(not (eq? x0 x1))
                 (values (gimple-cond-make c g3 (eq? (not (zero? (bitwise-and (GEdge-flags e0) EDGE-TRUE-VALUE))) x0)) #t)]
                [else (values g3 #f)])]
             [else (values g3 #f)]))
         (when (or replaced? folded1? folded2? fixed?) (set! changed #t))
         (restamp! c g g4)])))
  (define cfg1 (set-block cfg (block-set-stmts (block-set-phis b phis*) stmts*)))
  ;; propagate_into_phi_args
  (define cfg2
    (for/fold ([cfg cfg1]) ([ed (in-pvector succs)])
      (define d (GEdge-dest ed))
      (define db (cfg-block cfg d))
      (cond
        [(or (not db) (pvector-empty? (GBlock-phis db))) cfg]
        [else
         (set-block cfg (block-set-phis
                         db (for/pvector ([ph (in-pvector (GBlock-phis db))])
                              (restamp! c ph
                               (phi-set-args ph (for/pvector ([arg (in-pvector (gphi-args ph))])
                                                 (define a (cdr arg))
                                                 (cond
                                                   [(and (= (car arg) bb) (ssa-name? a) (not (ssa-virtual? a)))
                                                    (define val (get-constant-value c a))
                                                    (cond
                                                      [(and val (is-gimple-min-invariant st val) (may-propagate-copy c a val))
                                                       (set! changed #t) (cons (car arg) val)]
                                                      [else arg])]
                                                   [else arg])))))))])))
  (values cfg2 changed dce))

;; simple_dce_from_worklist over the folded CFG
(define (simple-dce c cfg worklist0)
  ;; per-block mutable copies; removed entries become #f
  (define phis (make-hasheqv)) (define stmts (make-hasheqv))
  (for ([i (in-pvector (cfg-bbs cfg))])
    (define b (cfg-block cfg i))
    (hash-set! phis i (list->vector (pvector->list (GBlock-phis b))))
    (hash-set! stmts i (list->vector (pvector->list (GBlock-stmts b)))))
  ;; defs: version -> (vector bb phi? index); use counts; PHI self-use counts
  (define defs (make-hasheqv)) (define uses (make-hasheqv)) (define self (make-hasheqv))
  (define (bump! h k) (hash-update! h k add1 0))
  (for ([i (in-pvector (cfg-bbs cfg))])
    (for ([ph (in-vector (hash-ref phis i))] [k (in-naturals)])
      (define res (gphi-result ph))
      (hash-set! defs (ssa-ver res) (vector i #t k))
      (for ([a (in-list (phi-ssa-args ph))])
        (bump! uses (ssa-ver (cdr a)))
        (when (ssa-name=? (cdr a) res) (bump! self (ssa-ver res)))))
    (for ([g (in-vector (hash-ref stmts i))] [k (in-naturals)])
      (for ([d (in-list (stmt-all-defs g))]) (hash-set! defs (ssa-ver d) (vector i #f k)))
      (for ([u (in-list (stmt-real-uses g))]) (bump! uses (ssa-ver u)))))
  (define released (make-hasheqv))
  (let loop ([wl worklist0])
    (unless (intbits-empty? wl)
      (define i (intbits-first wl))
      (define wl1 (intbits-clear wl i))
      (define d (and (not (hash-ref released i #f)) (hash-ref defs i #f)))
      (cond
        [(not d) (loop wl1)]
        [else
         (define bb (vector-ref d 0)) (define phi? (vector-ref d 1)) (define k (vector-ref d 2))
         (define t (vector-ref (hash-ref (if phi? phis stmts) bb) k))
         (define n-uses (hash-ref uses i 0))
         (cond
           [(and (> n-uses 0) (or (not phi?) (not (= n-uses (hash-ref self i 0))))) (loop wl1)]
           [(and (not phi?) (stmt-has-side-effects? (C-st c) t))
            (cond
              [(gcall? t)
               (vector-set! (hash-ref stmts bb) k (gcall (gcall-loc t) (gcall-flags t) #f (gcall-fn t) (gcall-args t) (gcall-fntype t) (gcall-internal? t) (gcall-ecf t)))
               (hash-set! released i #t)])
            (loop wl1)]
           [else
            (define used (if phi? (map cdr (phi-ssa-args t)) (stmt-real-uses t)))
            (define wl2
              (for/fold ([wl wl1]) ([u (in-list used)])
                (hash-update! uses (ssa-ver u) sub1 0)
                (if (ssa-default-def? u) wl (intbits-set wl (ssa-ver u)))))
            (vector-set! (hash-ref (if phi? phis stmts) bb) k #f)
            (hash-set! released i #t)
            (loop wl2)])])))
  (for/fold ([cfg cfg]) ([i (in-pvector (cfg-bbs cfg))])
    (define b (cfg-block cfg i))
    (set-block cfg (block-set-stmts (block-set-phis b (for/pvector ([ph (in-vector (hash-ref phis i))] #:when ph) ph))
                                    (for/pvector ([g (in-vector (hash-ref stmts i))] #:when g) g)))))

;; GCC <= 13: the queued statements are removed as they are (stmts_to_remove),
;; no use counts, no transitive removal
(define (remove-queued c cfg queued)
  (for/fold ([cfg cfg]) ([i (in-pvector (cfg-bbs cfg))])
    (define b (cfg-block cfg i))
    (define (keep-phi? ph) (not (intbits-ref queued (ssa-ver (gphi-result ph)))))
    (define (keep-stmt? g) (let ([l (stmt-ssa-lhs g)]) (not (and l (intbits-ref queued (ssa-ver l))))))
    (set-block cfg (block-set-stmts (block-set-phis b (pvector-filter (GBlock-phis b) keep-phi?))
                                    (pvector-filter (GBlock-stmts b) keep-stmt?)))))

;; substitute_and_fold: (values cfg changed?)
(define (ccp-finalize c)
  (define dom (C-dom c))
  (define-values (cfg changed dce)
    (let walk ([idx 0] [cfg (C-cfg c)] [changed #f] [dce intbits-empty])
      (define bb (dom-node dom idx))
      (define-values (cfg1 ch1 dce1)
        (if (or (= bb ENTRY) (= bb EXIT)) (values cfg #f dce) (fold-block c cfg bb dce)))
      (for/fold ([cfg cfg1] [changed (or changed ch1)] [dce dce1]) ([ch (in-pvector (dom-children dom idx))])
        (walk ch cfg changed dce))))
  ;; GCC >= 14 (r14-402): simple_dce_from_worklist
  (values (if (>= (gcc-major-version (C-st c)) 14) (simple-dce c cfg dce) (remove-queued c cfg dce)) changed))

;; ------------------------------------------------------------
;; do_ssa_ccp
;; ------------------------------------------------------------

(define (ccp st fndecl cfg)
  (define st0 (gs-set-fndecl st fndecl))
  (define dom (compute-dom (pvector-cons-left (pvector-cons-left (cfg-bbs cfg) EXIT) ENTRY) ENTRY
                           (lambda (i) (for/pvector ([e (in-pvector (cfg-succs cfg i))]) (GEdge-dest e)))))
  (define c (C st0 (cfg-clear-messages cfg) dom (make-vector (add1 (GS-ssa-version st0)) UNINIT) #f '() (cfg-next-stamp cfg)))
  (ssa-propagate cfg dom
                 #:backedge-worklists? (<= (gcc-major-version st0) 12)
                 #:init (lambda (e) (set-C-eng! c e) (ccp-initialize! c e))
                 #:visit-stmt (lambda (e uid g) (visit-stmt c (eng-uid-bb e uid) g))
                 #:visit-phi (lambda (e uid ph) (visit-phi c (eng-uid-bb e uid) ph)))
  (define-values (cfg1 changed?) (ccp-finalize c))
  (define cfg2 (for/fold ([cfg cfg1]) ([m (in-list (reverse (C-messages c)))]) (cfg-add-message cfg m)))
  (if changed? (cleanup-tree-cfg st0 cfg2 print-cond-rhs) (values cfg2 st0)))
