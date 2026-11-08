#lang racket/base

;; ============================================================
;; gimple: print GIMPLE in the text format of GCC's -fdump-tree-*
;; ============================================================
;;
;; Output is compared (after renumbering) with GCC's own dumps, so the
;; formatting follows gcc/gimple-pretty-print.cc and
;; gcc/tree-pretty-print.cc for the constructs the C subset produces.
;; Every printer takes the state: type uids and declaration flags
;; (artificial, label-uid) are resolved through it.
;; ============================================================

(require racket/port racket/string racket/list racket/match racket/math
         "../../kernel/data/data.rkt"
         "../../kernel/ir/gimple/types.rkt"
         "state.rkt"
         "tree.rkt"
         "gimple.rkt"
         "cfg.rkt"
         "ssa.rkt")

(provide print-function print-lowered-function print-cfg-function print-ssa-function print-type print-expr
         print-declarator print-stmt print-cond-rhs print-cfg-mode current-print-block print-vops? decl-print-name label-name
         stmt->string)

;; ------------------------------------------------------------
;; Types (dump_generic_node on types, no flags)
;; ------------------------------------------------------------

(define (quals-prefix st ty)
  (define q (type-attr st ty 'quals 0))
  (string-append (if (bitwise-bit-set? q 0) "const " "")
                 (if (bitwise-bit-set? q 1) "volatile " "")
                 (if (bitwise-bit-set? q 2) "restrict " "")))


;; dump_generic_node for an INTEGER_TYPE without a name
;; (build_nonstandard_integer_type): named after the C type of that
;; precision (aarch64: char 8, short 16, int 32, long 64, long long 64)
(define (nameless-integer-type-name uns? prec)
  (define (pick u s) (if uns? u s))
  (case prec
    [(8) (pick "unsigned char" "signed char")]
    [(16) (pick "unsigned short" "signed short")]
    [(32) (pick "unsigned int" "signed int")]
    [(64) (pick "unsigned long" "signed long")]
    [else
     (if (and (>= prec 8) (= prec (expt 2 (sub1 (integer-length prec)))))
         (format "~a~a_t" (pick "uint" "int") prec)
         (format "<unnamed-~a:~a>" (pick "unsigned" "signed") prec))]))

(define (print-type st ty)
  (define code (type-code st ty))
  (case code
    [(void_type) (string-append (quals-prefix st ty) "void")]
    [(integer_type real_type boolean_type enumeral_type complex_type bitint_type)
     (define nm (type-name-string st ty))
     (string-append (quals-prefix st ty)
                    (cond
                      [(eq? code 'enumeral_type) (string-append "enum " (or nm "<anon>"))]
                      [nm nm]
                      [(eq? code 'integer_type) (nameless-integer-type-name (type-attr st ty 'unsigned) (type-attr st ty 'precision))]
                      [else (format "<unnamed-~a:~a>" (if (type-attr st ty 'unsigned) "unsigned" "signed") (type-attr st ty 'precision))]))]
    [(record_type union_type)
     (define nm (type-name-string st ty))
     (string-append (quals-prefix st ty) (if (eq? code 'record_type) "struct " "union ") (or nm "<anon>"))]
    [(pointer_type reference_type)
     (define pointee (type-type st ty))
     (cond
       [(func-or-method-type-p st pointee)
        (string-append (print-type st (type-type st pointee)) " (*) (" (print-arg-types st pointee) ")")]
       [else (string-append (print-type st pointee) " *"
                            (let ([q (quals-prefix st ty)]) (if (string=? q "") "" (string-append " " (string-trim q)))))])]
    [(array_type)
     (string-append (print-type st (type-type st ty)) "[" (array-dim st ty) "]")]
    [(function_type method_type)
     (string-append (print-type st (type-type st ty)) " (" (print-arg-types st ty) ")")]
    [else (format "<~a>" code)]))

(define (array-dim st ty)
  (define dom (type-attr st ty 'domain))
  (define mx (and dom (type-attr st dom 'max)))
  (if (and mx (eq? (tree-code mx) 'integer_cst)) (number->string (add1 (integer-cst-value mx))) ""))

;; print_declaration-style "type name": arrays use declarator syntax (int a[4][2])
(define (print-declarator st ty name)
  (let loop ([t ty] [dims '()])
    (cond
      [(eq? (type-code st t) 'array_type) (loop (type-type st t) (cons (array-dim st t) dims))]
      [else
       (format "~a ~a~a" (print-type st t) name
               (apply string-append (for/list ([d (reverse dims)]) (format "[~a]" d))))])))

(define (print-arg-types st fty)
  (let loop ([p (type-attr st fty 'arg-types)] [acc '()])
    (cond
      [(not p) (string-join (reverse acc) ", ")]
      [(void-type-p st (tree-value p)) (string-join (reverse acc) ", ")]
      [else (loop (tree-chain p) (cons (print-type st (tree-value p)) acc))])))

;; ------------------------------------------------------------
;; Names
;; ------------------------------------------------------------

(define (decl-print-name st d)
  (define nm (decl-name d))
  (cond
    [nm nm]                                  ; user names, and artificial but named (iftmp.0, retval.1 ...)
    [else (format "D.~a" (decl-uid d))]))

;; LABEL_DECL: name, else <L%d> once the CFG assigned LABEL_DECL_UID, else <D.%u>
(define (label-name st l)
  (define nm (decl-name l))
  (cond
    [(and nm (not (decl-flag st l 'artificial))) (format "<~a>" nm)]
    [(decl-flag st l 'label-uid #f) => (lambda (u) (format "<L~a>" u))]
    [else (format "<D.~a>" (decl-uid l))]))

;; dump_gimple_switch appends the edge probability ("[INV]") once a CFG exists;
;; print-blocks binds the cfg and the block so the probability can be found
(define print-cfg-mode (make-parameter #f))
(define current-print-block (make-parameter #f))     ; (cons cfg bb) or #f

;; dump_probability: "[%.2f%%]" of to_reg_br_prob_base / 100, "[INV]" when uninitialized
(define (probability-string c src dest)
  (define p (prof-prob c src dest))
  (cond
    [(not p) "[INV]"]
    [else
     (define v (prof-to-reg-br-prob-base p))
     (define v1 (if (and (< v 1) (> p 0)) 1 v))
     (format "[~a.~a~a%]" (quotient v1 100) (quotient (remainder v1 100) 10) (remainder v1 10))]))
(define (prof-to-reg-br-prob-base p) (quotient (+ (* p 10000) 67108864) 134217728))
(define (block-header-string c bb)
  (define n (prof-count c bb))
  (cond
    [(prof-count-zero? c bb) (format "  <bb ~a> [count: 0]:\n" bb)]   ; dump_profile: ipa_p
    [n (format "  <bb ~a> [local count: ~a]:\n" bb n)]
    [else (format "  <bb ~a> :\n" bb)]))
;; -vops: "# .MEM_N = VDEF <.MEM_M>" / "# VUSE <.MEM_M>" before a statement
(define print-vops? (make-parameter #f))

;; SSA_NAME: identifier_version, var-name_version or _version; default
;; definitions carry "(D)"
(define (ssa-print-name s)
  (define id (ssa-name-identifier s))
  (define v (ssa-name-var s))
  (string-append
   (cond
     [id (format "~a_~a" id (ssa-name-version s))]
     [(and v (decl-name v)) (format "~a_~a" (decl-name v) (ssa-name-version s))]
     [else (format "_~a" (ssa-name-version s))])
   (if (tree-attr s 'default-def #f) "(D)" "")))

;; ------------------------------------------------------------
;; Constants
;; ------------------------------------------------------------

;; real_to_decimal (gcc/real.cc) style: exact decimal expansion, "d.ddde[+-]x",
;; at least one fractional digit, at most 54 significant digits (double).
(define (print-real r)
  (define x
    (cond
      [(real-cst-value r) => values]
      [(real-cst-hex r) => parse-hex-float]
      [else 0.0]))
  (define ex (inexact->exact x))
  (cond
    [(zero? ex) "0.0"]
    [else
     (define neg (negative? ex))
     (define a (abs ex))
     (define e (let loop ([e (- (exact-floor (/ (log (exact->inexact a)) (log 10))) 1)])
                 (cond [(>= (/ a (expt 10 e)) 10) (loop (add1 e))]
                       [(< (/ a (expt 10 e)) 1) (loop (sub1 e))]
                       [else e])))
     (define m (/ a (expt 10 e)))
     (define max-digits 54)
     (define digits
       (let loop ([m m] [n 0] [acc '()])
         (cond
           [(or (zero? m) (>= n max-digits)) (reverse acc)]
           [else
            (define d (exact-floor m))
            (loop (* 10 (- m d)) (add1 n) (cons d acc))])))
     (define int-digit (car digits))
     (define frac (let ([f (cdr digits)])
                    (define trimmed (let loop ([l (reverse f)]) (if (and (pair? l) (zero? (car l))) (loop (cdr l)) (reverse l))))
                    (if (null? trimmed) '(0) trimmed)))
     (format "~a~a.~ae~a~a" (if neg "-" "") int-digit (apply string-append (map number->string frac))
             (if (negative? e) "-" "+") (abs e))]))

(define (parse-hex-float s)
  (define m (regexp-match #px"^(-?)0x([0-9a-fA-F]+)(?:\\.([0-9a-fA-F]*))?p([+-]?[0-9]+)$" s))
  (cond
    [(not m) 0.0]
    [else
     (define neg (string=? (cadr m) "-"))
     (define ip (string->number (caddr m) 16))
     (define fs (or (cadddr m) ""))
     (define fp (if (string=? fs "") 0 (/ (string->number fs 16) (expt 16 (string-length fs)))))
     (define ex (string->number (list-ref m 4)))
     (define v (* (+ ip fp) (expt 2 ex)))
     (exact->inexact (if neg (- v) v))]))

(define (print-string-cst bytes)
  (define b (if (and (> (bytes-length bytes) 0) (zero? (bytes-ref bytes (sub1 (bytes-length bytes)))))
                (subbytes bytes 0 (sub1 (bytes-length bytes)))
                bytes))
  (string-append "\""
                 (apply string-append
                        (for/list ([c (in-bytes b)])
                          (case c
                            [(10) "\\n"] [(9) "\\t"] [(13) "\\r"] [(34) "\\\""] [(92) "\\\\"] [(0) "\\0"]
                            [else (if (or (< c 32) (> c 126)) (format "\\~a" (number->string c 8)) (string (integer->char c)))])))
                 "\""))

;; ------------------------------------------------------------
;; Expressions (operands)
;; ------------------------------------------------------------

(define binary-op
  (hash 'plus_expr "+" 'pointer_plus_expr "+" 'minus_expr "-" 'mult_expr "*" 'trunc_div_expr "/" 'trunc_mod_expr "%"
        'rdiv_expr "/" 'exact_div_expr "/" 'floor_div_expr "/" 'ceil_div_expr "/" 'round_div_expr "/"
        'lt_expr "<" 'le_expr "<=" 'gt_expr ">" 'ge_expr ">=" 'eq_expr "==" 'ne_expr "!="
        'bit_and_expr "&" 'bit_ior_expr "|" 'bit_xor_expr "^" 'lshift_expr "<<" 'rshift_expr ">>"
        'truth_and_expr "&&" 'truth_or_expr "||" 'truth_xor_expr "^" 'truth_andif_expr "&&" 'truth_orif_expr "||"
        'lrotate_expr "r<<" 'rrotate_expr "r>>"))
(define angle-ops '(min_expr max_expr abs_expr absu_expr))

;; op_prio (tree-pretty-print.cc)
(define (op-prio t)
  (cond
    [(not (Tree? t)) 9999]
    [else
     (case (tree-code t)
       [(tree_list compound_expr bind_expr) 1]
       [(modify_expr init_expr) 2]
       [(cond_expr) 3]
       [(truth_or_expr truth_orif_expr) 4]
       [(truth_and_expr truth_andif_expr) 5]
       [(bit_ior_expr) 6]
       [(bit_xor_expr truth_xor_expr) 7]
       [(bit_and_expr) 8]
       [(eq_expr ne_expr) 9]
       [(unlt_expr unle_expr ungt_expr unge_expr uneq_expr ltgt_expr ordered_expr unordered_expr lt_expr le_expr gt_expr ge_expr) 10]
       [(lshift_expr rshift_expr lrotate_expr rrotate_expr widen_lshift_expr) 11]
       [(widen_sum_expr plus_expr pointer_plus_expr pointer_diff_expr minus_expr) 12]
       [(widen_mult_expr dot_prod_expr widen_mult_plus_expr widen_mult_minus_expr mult_expr mult_highpart_expr
         trunc_div_expr ceil_div_expr floor_div_expr round_div_expr rdiv_expr exact_div_expr
         trunc_mod_expr ceil_mod_expr floor_mod_expr round_mod_expr) 13]
       [(truth_not_expr bit_not_expr postincrement_expr postdecrement_expr preincrement_expr predecrement_expr
         negate_expr indirect_ref addr_expr float_expr nop_expr convert_expr fix_trunc_expr target_expr) 14]
       [(save_expr) (op-prio (tree-operand t 0))]
       [else 9999])]))

(define (print-expr st e)
  (define (pe x) (print-expr st x))
  ;; an operand in parentheses when its priority is CMP the node's
  (define (pe-prio x node cmp)
    (if (cmp (op-prio x) (op-prio node)) (string-append "(" (pe x) ")") (pe x)))
  (cond
    [(not e) "<null>"]
    [else
     (define code (tree-code e))
     (case code
       [(var_decl parm_decl result_decl function_decl const_decl field_decl) (decl-print-name st e)]
       [(label_decl) (label-name st e)]
       [(ssa_name) (ssa-print-name e)]
       [(integer_cst)
        (define v (integer-cst-value e))
        (cond
          ;; TREE_INT_CST_LOW as a signed HOST_WIDE_INT, "B" for bytes
          [(pointer-type-p st (tree-type e))
           (define m (bitwise-and v (sub1 (arithmetic-shift 1 64))))
           (if (zero? v) "0B" (format "~aB" (if (bitwise-bit-set? m 63) (- m (arithmetic-shift 1 64)) m)))]
          [else (number->string v)])]
       [(real_cst) (print-real e)]
       [(string_cst) (print-string-cst (string-cst-bytes e))]
       [(addr_expr)
        (define op (tree-operand e 0))
        (if (eq? (tree-code op) 'string_cst) (pe op) (string-append "&" (pe op)))]
       [(mem_ref)
        (define ptr (tree-operand e 0)) (define off (tree-operand e 1))
        (cond
          [(and (integer-zerop off)
                (types-compatible-p st (tree-type e) (type-type st (tree-type ptr)))
                (not (eq? (tree-code ptr) 'addr_expr)))
           (string-append "*" (pe ptr))]
          [(and (integer-zerop off) (eq? (tree-code ptr) 'addr_expr)
                (types-compatible-p st (tree-type e) (tree-type (tree-operand ptr 0))))
           (pe (tree-operand ptr 0))]
          [else
           (format "MEM[(~a *)~a~a]" (print-type st (tree-type e)) (pe ptr)
                   (if (integer-zerop off) "" (format " + ~aB" (integer-cst-value off))))])]
       ;; dump_mem_ref for TARGET_MEM_REF (base offset index step index2):
       ;; MEM[(alias type)base + offset + index2 + index * step]
       [(target_mem_ref)
        (define base (tree-operand e 0)) (define off (tree-operand e 1))
        (define idx (tree-operand e 2)) (define stp (tree-operand e 3)) (define idx2 (tree-operand e 4))
        (format "MEM[(~a)~a~a~a~a]" (print-type st (type-main-variant st (tree-type off))) (pe base)
                (if (integer-zerop off) "" (format " + ~a" (pe off)))
                (if idx2 (format " + ~a" (pe idx2)) "")
                (if idx (format " + ~a * ~a" (pe idx) (if stp (pe stp) "1")) ""))]
       [(indirect_ref) (string-append "*" (pe (tree-operand e 0)))]
       [(component_ref)
        (define base (tree-operand e 0))
        (define field (tree-operand e 1))
        (cond
          [(and (eq? (tree-code base) 'mem_ref) (integer-zerop (tree-operand base 1))
                (not (eq? (tree-code (tree-operand base 0)) 'addr_expr)))
           (string-append (pe (tree-operand base 0)) "->" (decl-print-name st field))]
          [(eq? (tree-code base) 'indirect_ref)
           (string-append (pe (tree-operand base 0)) "->" (decl-print-name st field))]
          [else (string-append (pe base) "." (decl-print-name st field))])]
       [(array_ref) (format "~a[~a]" (pe (tree-operand e 0)) (pe (tree-operand e 1)))]
       [(nop_expr convert_expr float_expr fix_trunc_expr)
        (format "(~a) ~a" (print-type st (tree-type e)) (pe-prio (tree-operand e 0) e <))]
       [(view_convert_expr) (format "VIEW_CONVERT_EXPR<~a>(~a)" (print-type st (tree-type e)) (pe (tree-operand e 0)))]
       [(negate_expr) (string-append "-" (pe-prio (tree-operand e 0) e <))]
       [(bit_not_expr) (string-append "~" (pe-prio (tree-operand e 0) e <))]
       [(truth_not_expr) (string-append "!" (pe-prio (tree-operand e 0) e <))]
       [(min_expr max_expr) (format "~a <~a, ~a>" (string-upcase (symbol->string code)) (pe (tree-operand e 0)) (pe (tree-operand e 1)))]
       [(abs_expr absu_expr) (format "~a <~a>" (string-upcase (symbol->string code)) (pe (tree-operand e 0)))]
       [(constructor)
        (define elts (constructor-elts e))
        (cond
          [(tree-clobber-p e)
           ;; CLOBBER_STORAGE_END: "eos" since GCC 14, "eol" before
           (format "{CLOBBER(~a)}" (if (>= (gcc-major-version st) 14) "eos" "eol"))]
          [(pvector-empty? elts) "{}"]
          [else
           (format "{~a}" (string-join (for/list ([p (in-pvector elts)])
                                         (format "~a~a" (if (and (car p) (eq? (tree-code (car p)) 'field_decl)) (format ".~a=" (decl-name (car p))) "") (pe (cdr p))))
                                       ", "))])]
       [(cond_expr) (format "~a ? ~a : ~a" (pe (tree-operand e 0)) (pe (tree-operand e 1)) (pe (tree-operand e 2)))]
       [(call_expr) (format "~a (~a)" (pe (call-expr-fn e)) (string-join (for/list ([a (in-pvector (call-expr-args e))]) (pe a)) ", "))]
       [else
        (cond
          [(hash-ref binary-op code #f)
           => (lambda (op) (format "~a ~a ~a" (pe-prio (tree-operand e 0) e <=) op (pe-prio (tree-operand e 1) e <=)))]
          [else (format "<~a>" code)])])]))

;; ------------------------------------------------------------
;; Statements
;; ------------------------------------------------------------

(define predictor-names
  (hash 'PRED_TREE_EARLY_RETURN "early return (on trees)" 'PRED_CONTINUE "continue"
        'PRED_COLD_LABEL "cold label" 'PRED_HOT_LABEL "hot label"))

(define (print-rhs st g)
  (define code (gassign-code g))
  (define a (gassign-rhs1 g)) (define b (gassign-rhs2 g))
  (define (pe x) (print-expr st x))
  (define up (string-upcase (symbol->string code)))
  (case (gimple-rhs-class st code)
    [(single) (pe a)]
    [(unary)
     (case code
       [(nop_expr convert_expr float_expr fix_trunc_expr) (format "(~a) ~a" (print-type st (tree-type (gassign-lhs g))) (pe a))]
       [(negate_expr) (string-append "-" (pe a))]
       [(bit_not_expr) (string-append "~" (pe a))]
       [(truth_not_expr) (string-append "!" (pe a))]
       [(abs_expr absu_expr) (format "~a <~a>" up (pe a))]
       [(view_convert_expr) (format "VIEW_CONVERT_EXPR<~a>(~a)" (print-type st (tree-type (gassign-lhs g))) (pe a))]
       [(addr_expr) (string-append "&" (pe a))]
       [else (format "~a <~a>" up (pe a))])]
    [(binary)
     (cond
       [(memq code angle-ops) (format "~a <~a, ~a>" up (pe a) (pe b))]
       [(hash-ref binary-op code #f) => (lambda (op) (format "~a ~a ~a" (pe a) op (pe b)))]
       [else (format "~a <~a, ~a>" up (pe a) (pe b))])]
    [(ternary) (format "~a <~a, ~a, ~a>" up (pe a) (pe b) (pe (gassign-rhs3 g)))]
    [else "<?>"]))

(define (print-case st c)
  (define lo (case-low c)) (define hi (case-high c))
  (define lab (string-append (label-name st (case-label c))
                             (cond
                               [(not (print-cfg-mode)) ""]
                               [(current-print-block)
                                => (lambda (cb)
                                     (define dest (label-to-block (car cb) (case-label c)))
                                     (string-append " " (if dest (probability-string (car cb) (cdr cb) dest) "[INV]")))]
                               [else " [INV]"])))
  (cond
    [(not lo) (format "default: ~a" lab)]
    [hi (format "case ~a ... ~a: ~a" (print-expr st lo) (print-expr st hi) lab)]
    [else (format "case ~a: ~a" (print-expr st lo) lab)]))

;; print_gimple_stmt (flags 0) of one statement or PHI as a string, in
;; the CFG context of block BB (switch labels print edge probabilities)
(define (stmt->string st cfg bb g)
  (cond
    [(gphi? g)
     (format "~a = PHI <~a>" (print-expr st (gphi-result g))
             (string-join (for/list ([arg (in-pvector (gphi-args g))])
                            (format "~a(~a)" (if (cdr arg) (print-expr st (cdr arg)) "<null>") (car arg)))
                          ", "))]
    [else
     (define s (parameterize ([print-cfg-mode #t] [current-print-block (cons cfg bb)])
                 (with-output-to-string (lambda () (print-stmt st g 0 (current-output-port))))))
     (if (and (positive? (string-length s)) (char=? (string-ref s (sub1 (string-length s))) #\newline))
         (substring s 0 (sub1 (string-length s)))
         s)]))

;; print_gimple_expr (TDF_RHS_ONLY) of a GIMPLE_COND
(define (print-cond-rhs st g)
  (format "~a ~a ~a" (print-expr st (gcond-lhs g)) (hash-ref binary-op (gcond-code g) "?") (print-expr st (gcond-rhs g))))

(define (print-stmt st g indent out)
  (define pad (make-string indent #\space))
  (define (pe x) (print-expr st x))
  (match g
    [(gbind _ _ vars body _)
     (fprintf out "~a{\n" pad)
     (for ([v (in-pvector vars)]) (fprintf out "~a  ~a;\n" pad (print-declarator st (tree-type v) (decl-print-name st v))))
     (unless (pvector-empty? vars) (fprintf out "\n"))
     (for ([s (in-pvector body)]) (print-stmt st s (+ indent 2) out))
     (fprintf out "~a}\n" pad)]
    [(gassign _ _ lhs _ _ _ _) (fprintf out "~a~a = ~a;\n" pad (pe lhs) (print-rhs st g))]
    [(gcall _ _ lhs fn args _ _ _)
     (fprintf out "~a~a~a (~a);\n" pad (if lhs (string-append (pe lhs) " = ") "")
              (pe (if (eq? (tree-code fn) 'addr_expr) (tree-operand fn 0) fn))
              (string-join (for/list ([a (in-pvector args)]) (pe a)) ", "))]
    [(gcond _ _ code l r t f)
     (fprintf out "~aif (~a)~a\n" pad (print-cond-rhs st g)
              (if (or t f) (format " goto ~a; else goto ~a;" (label-name st t) (label-name st f)) ""))]
    [(glabel _ _ l) (fprintf out "~a~a:\n" pad (label-name st l))]
    [(ggoto _ _ l) (fprintf out "~agoto ~a;\n" pad (label-name st l))]
    [(greturn _ _ v) (fprintf out "~areturn~a;\n" pad (if v (string-append " " (pe v)) ""))]
    [(gswitch _ _ i d cs)
     (fprintf out "~aswitch (~a) <~a>\n" pad (pe i) (string-join (cons (print-case st d) (for/list ([c (in-pvector cs)]) (print-case st c))) ", "))]
    [(gpredict _ _ p o)
     (fprintf out "~a// predicted ~a by ~a predictor.\n" pad (if (eq? o 'TAKEN) "likely" "unlikely") (hash-ref predictor-names p (symbol->string p)))]
    [(gtry _ _ ev cl kind)
     (fprintf out "~atry\n~a  {\n" pad pad)
     (for ([s (in-pvector ev)]) (print-stmt st s (+ indent 4) out))
     (fprintf out "~a  }\n~a~a\n~a  {\n" pad pad (if (eq? kind 'finally) "finally" "catch") pad)
     (for ([s (in-pvector cl)]) (print-stmt st s (+ indent 4) out))
     (fprintf out "~a  }\n" pad)]
    [(gnop _ _) (void)]
    [_ (fprintf out "~a<~a>\n" pad g)]))

(define (print-header st fndecl out)
  (define ftype (tree-type fndecl))
  (define params (decl-arguments fndecl))
  (fprintf out "~a ~a (~a)\n" (print-type st (type-type st ftype)) (decl-name fndecl)
           (string-join (for/list ([p (in-pvector params)]) (print-declarator st (tree-type p) (decl-print-name st p))) ", ")))

;; dump_function_to_file: header then the body bind
(define (print-function st fndecl bind out)
  (print-header st fndecl out)
  (print-stmt st bind 0 out)
  (fprintf out "\n\n"))

;; dump_function_to_file after pass_lower_cf: local decls (FOR_EACH_LOCAL_DECL is
;; newest first), blank line, flat statement sequence at indent 2.
(define (print-lowered-function st fndecl local-decls body out)
  (print-header st fndecl out)
  (fprintf out "{\n")
  (for ([v (in-pvector (pvector-reverse local-decls))])
    (fprintf out "  ~a;\n" (print-declarator st (tree-type v) (decl-print-name st v))))
  (unless (pvector-empty? local-decls) (fprintf out "\n"))
  (for ([g (in-pvector body)]) (print-stmt st g 2 out))
  (fprintf out "}\n\n\n"))

;; ------------------------------------------------------------
;; .cfg dump: pass messages, ";; N succs { ... }" lines (flow_loops_cfg_dump),
;; then dump_function_to_file with dump_bb per block.  The loop tree lines
;; (";; N loops found" ...) are not produced; the test drops ";;" lines
;; other than the succs lines.
;; ------------------------------------------------------------
(define (print-cfg-function st fndecl local-decls cfg out)
  (for ([m (in-pvector (GCfg-messages cfg))]) (fprintf out "~a\n" m))
  (define bbs (cfg-bbs cfg))
  (for ([i (in-pvector bbs)])
    (fprintf out ";; ~a succs {~a }\n" i
             (apply string-append (for/list ([e (in-pvector (cfg-succs cfg i))]) (format " ~a" (GEdge-dest e))))))
  (print-header st fndecl out)
  (fprintf out "{\n")
  (for ([v (in-pvector (pvector-reverse local-decls))])
    (fprintf out "  ~a;\n" (print-declarator st (tree-type v) (decl-print-name st v))))
  (unless (pvector-empty? local-decls) (fprintf out "\n"))
  (print-blocks st cfg out)
  (fprintf out "}\n\n\n"))

;; dump_bb for every block: header, PHIs (dump_phi_nodes), statements,
;; implicit edges, blank line
(define (print-blocks st cfg out)
  (define bbs (cfg-bbs cfg))
  (parameterize ([print-cfg-mode #t])
    (for ([i (in-pvector bbs)])
      (define b (cfg-block cfg i))
      (fprintf out "~a" (block-header-string cfg i))
      ;; dump_phi_nodes: virtual PHIs only with -vops
      (for ([ph (in-pvector (GBlock-phis b))]
            #:when (or (print-vops?) (not (virtual-operand? (gphi-var ph)))))
        (fprintf out "  # ~a = PHI <~a>\n" (print-expr st (gphi-result ph))
                 (string-join (for/list ([arg (in-pvector (gphi-args ph))])
                                (format "~a(~a)" (if (cdr arg) (print-expr st (cdr arg)) "<null>") (car arg)))
                              ", ")))
      (for ([g (in-pvector (GBlock-stmts b))])
        (when (print-vops?)
          (define vd (stmt-vdef g)) (define vu (stmt-vuse g))
          (cond
            [vd (fprintf out "  # ~a = VDEF <~a>\n" (print-expr st vd) (print-expr st vu))]
            [vu (fprintf out "  # VUSE <~a>\n" (print-expr st vu))]))
        (parameterize ([current-print-block (cons cfg i)])
          (print-stmt st g (if (glabel? g) 0 2) out)))
      ;; dump_implicit_edges
      (define last (bb-last-stmt cfg i))
      (cond
        [(and (gcond? last) (= 2 (pvector-length (cfg-succs cfg i))))
         (define-values (t f) (extract-true-false-edges cfg i))
         (fprintf out "    goto <bb ~a>; ~a\n  else\n    goto <bb ~a>; ~a\n"
                  (GEdge-dest t) (probability-string cfg i (GEdge-dest t)) (GEdge-dest f) (probability-string cfg i (GEdge-dest f)))]
        [else
         (define e (find-fallthru-edge cfg i))
         (when (and e (not (eqv? (GEdge-dest e) (GBlock-next b))))
           (fprintf out "  goto <bb ~a>; ~a\n" (GEdge-dest e) (probability-string cfg i (GEdge-dest e))))])
      (fprintf out "\n"))))

;; .ssa dump (dump_function_to_file with PROP_ssa): local decls, then the
;; SSA names without a named variable as "type name;", in version order
(define (print-ssa-function st fndecl local-decls cfg out)
  (for ([m (in-pvector (GCfg-messages cfg))]) (fprintf out "~a\n" m))
  (print-header st fndecl out)
  (fprintf out "{\n")
  (for ([v (in-pvector (pvector-reverse local-decls))])
    (fprintf out "  ~a;\n" (print-declarator st (tree-type v) (decl-print-name st v))))
  (define anon
    (for/list ([n (in-pvector (ssa-names-in-version-order cfg))]
               #:when (let ([v (ssa-name-var n)]) (or (not v) (not (decl-name v)))))
      n))
  (for ([n (in-list anon)])
    (fprintf out "  ~a ~a;\n" (print-type st (tree-type n)) (print-expr st n)))
  (unless (and (pvector-empty? local-decls) (null? anon)) (fprintf out "\n"))
  (print-blocks st cfg out)
  (fprintf out "}\n\n\n"))
