#lang racket/base

;; ============================================================
;; Print GIMPLE in the text format of GCC's -fdump-tree-gimple
;; ============================================================
;;
;; The output is compared (after renumbering) with GCC's own dump, so
;; the formatting follows gcc/gimple-pretty-print.cc and
;; gcc/tree-pretty-print.cc for the constructs the C subset produces.
;; ============================================================

(require racket/string racket/list racket/match racket/math
         "../generic/tree.rkt"
         "../generic/gimple.rkt")

(provide print-function print-function->string print-type print-expr)

;; ------------------------------------------------------------
;; Types (dump_generic_node on types, no flags)
;; ------------------------------------------------------------

(define (quals-prefix type)
  (define q (type-quals type))
  (string-append (if (bitwise-bit-set? q 0) "const " "")
                 (if (bitwise-bit-set? q 1) "volatile " "")
                 (if (bitwise-bit-set? q 2) "restrict " "")))

(define (type-name-of type)
  (define nm (attr type 'name #f))
  (cond
    [(not nm) #f]
    [(string? nm) nm]
    [(eq? (tree-code nm) 'identifier_node) (attr nm 'id)]
    [(eq? (tree-code nm) 'type_decl) (decl-name nm)]
    [else #f]))

(define (print-type type)
  (case (tree-code type)
    [(void_type) (string-append (quals-prefix type) "void")]
    [(integer_type real_type boolean_type enumeral_type complex_type bitint_type)
     (define nm (type-name-of type))
     (string-append (quals-prefix type)
                    (cond
                      [(eq? (tree-code type) 'enumeral_type) (string-append "enum " (or nm "<anon>"))]
                      [nm nm]
                      [else (format "<unnamed-~a:~a>" (if (type-unsigned type) "unsigned" "signed") (type-precision type))]))]
    [(record_type union_type)
     (define nm (type-name-of type))
     (string-append (quals-prefix type) (if (eq? (tree-code type) 'record_type) "struct " "union ") (or nm "<anon>"))]
    [(pointer_type reference_type)
     (define pointee (tree-type type))
     (cond
       [(func-or-method-type-p pointee)
        (string-append (print-type (tree-type pointee)) " (*) (" (print-arg-types pointee) ")")]
       [else (string-append (print-type pointee) " *" (let ([q (quals-prefix type)]) (if (string=? q "") "" (string-append " " (string-trim q)))))])]
    [(array_type)
     (define dom (type-domain type))
     (define n (and dom (type-max-value dom) (eq? (tree-code (type-max-value dom)) 'integer_cst)
                    (add1 (integer-cst-value (type-max-value dom)))))
     (string-append (print-type (tree-type type)) "[" (if n (number->string n) "") "]")]
    [(function_type method_type)
     (string-append (print-type (tree-type type)) " (" (print-arg-types type) ")")]
    [else (format "<~a>" (tree-code type))]))

(define (print-arg-types ftype)
  (let loop ([p (type-arg-types ftype)] [acc '()])
    (cond
      [(not p) (string-join (reverse acc) ", ")]
      [(void-type-p (tree-value p)) (string-join (reverse acc) ", ")]
      [else (loop (tree-chain p) (cons (print-type (tree-value p)) acc))])))

;; ------------------------------------------------------------
;; Names
;; ------------------------------------------------------------

(define (decl-print-name d)
  (define nm (decl-name d))
  (cond
    [(and nm (not (decl-artificial d))) nm]
    [nm nm]                                  ; artificial but named (iftmp.0, retval.1 ...)
    [else (format "D.~a" (decl-uid d))]))

(define (label-name l)
  (define nm (decl-name l))
  (if (and nm (not (decl-artificial l))) (format "<~a>" nm) (format "<D.~a>" (decl-uid l))))

(define (ssa-print-name s)
  (define id (attr s 'identifier #f))
  (define v (ssa-name-var s))
  (cond
    [id (format "~a_~a" id (ssa-name-version s))]
    [(and v (decl-name v)) (format "~a_~a" (decl-name v) (ssa-name-version s))]
    [else (format "_~a" (ssa-name-version s))]))

;; ------------------------------------------------------------
;; Constants
;; ------------------------------------------------------------

;; real_to_decimal (gcc/real.cc) style: exact decimal expansion, "d.ddde[+-]x",
;; at least one fractional digit, at most 54 significant digits (double).
(define (print-real r type)
  (define x
    (cond
      [(attr r 'real #f) => values]
      [(real-cst-hex r) => parse-hex-float]
      [else 0.0]))
  (define ex (inexact->exact x))
  (cond
    [(zero? ex) "0.0"]
    [else
     (define neg (negative? ex))
     (define a (abs ex))
     ;; find exponent e with 1 <= a/10^e < 10
     (define e (let loop ([e (- (exact-floor (/ (log (exact->inexact a)) (log 10))) 1)])
                 (cond [(>= (/ a (expt 10 e)) 10) (loop (add1 e))]
                       [(< (/ a (expt 10 e)) 1) (loop (sub1 e))]
                       [else e])))
     (define m (/ a (expt 10 e)))                    ; exact rational in [1,10)
     ;; digits
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
  ;; "0x1.8p+1" / "-0x1p-3"
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
  ;; GCC prints the string without the trailing NUL, C-escaped
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

(define (print-expr e)
  (cond
    [(not e) "<null>"]
    [else
     (define code (tree-code e))
     (case code
       [(var_decl parm_decl result_decl function_decl const_decl field_decl) (decl-print-name e)]
       [(label_decl) (label-name e)]
       [(ssa_name) (ssa-print-name e)]
       [(integer_cst)
        (define v (integer-cst-value e))
        (define type (tree-type e))
        (cond
          [(and type (pointer-type-p type)) (if (zero? v) "0B" (format "~aB" v))]
          [else (number->string v)])]
       [(real_cst) (print-real e (tree-type e))]
       [(string_cst) (print-string-cst (string-cst-bytes e))]
       [(addr_expr)
        (define op (tree-operand e 0))
        (if (eq? (tree-code op) 'string_cst) (print-expr op) (string-append "&" (print-expr op)))]
       [(mem_ref)
        (define ptr (tree-operand e 0)) (define off (tree-operand e 1))
        (cond
          [(and (integer-zerop off)
                (types-compatible-p (tree-type e) (tree-type (tree-type ptr)))
                (not (eq? (tree-code ptr) 'addr_expr)))
           (string-append "*" (print-expr ptr))]
          [(and (integer-zerop off) (eq? (tree-code ptr) 'addr_expr)
                (types-compatible-p (tree-type e) (tree-type (tree-operand ptr 0))))
           (print-expr (tree-operand ptr 0))]
          [else
           (format "MEM[(~a *)~a~a]" (print-type (tree-type e)) (print-expr ptr)
                   (if (integer-zerop off) "" (format " + ~aB" (integer-cst-value off))))])]
       [(indirect_ref) (string-append "*" (print-expr (tree-operand e 0)))]
       [(component_ref)
        (define base (tree-operand e 0))
        (define field (tree-operand e 1))
        (cond
          [(and (eq? (tree-code base) 'mem_ref) (integer-zerop (tree-operand base 1))
                (not (eq? (tree-code (tree-operand base 0)) 'addr_expr)))
           (string-append (print-expr (tree-operand base 0)) "->" (decl-print-name field))]
          [(eq? (tree-code base) 'indirect_ref)
           (string-append (print-expr (tree-operand base 0)) "->" (decl-print-name field))]
          [else (string-append (print-expr base) "." (decl-print-name field))])]
       [(array_ref) (format "~a[~a]" (print-expr (tree-operand e 0)) (print-expr (tree-operand e 1)))]
       [(nop_expr convert_expr float_expr fix_trunc_expr)
        (format "(~a) ~a" (print-type (tree-type e)) (print-expr (tree-operand e 0)))]
       [(view_convert_expr) (format "VIEW_CONVERT_EXPR<~a>(~a)" (print-type (tree-type e)) (print-expr (tree-operand e 0)))]
       [(negate_expr) (string-append "-" (print-expr (tree-operand e 0)))]
       [(bit_not_expr) (string-append "~" (print-expr (tree-operand e 0)))]
       [(truth_not_expr) (string-append "!" (print-expr (tree-operand e 0)))]
       [(min_expr max_expr) (format "~a <~a, ~a>" (string-upcase (symbol->string code)) (print-expr (tree-operand e 0)) (print-expr (tree-operand e 1)))]
       [(abs_expr absu_expr) (format "~a <~a>" (string-upcase (symbol->string code)) (print-expr (tree-operand e 0)))]
       [(constructor)
        (define elts (constructor-elts e))
        (if (null? elts) "{}"
            (format "{~a}" (string-join (for/list ([p elts]) (format "~a~a" (if (and (car p) (eq? (tree-code (car p)) 'field_decl)) (format ".~a=" (decl-name (car p))) "") (print-expr (cdr p)))) ", ")))]
       [(cond_expr) (format "~a ? ~a : ~a" (print-expr (tree-operand e 0)) (print-expr (tree-operand e 1)) (print-expr (tree-operand e 2)))]
       [(call_expr) (format "~a (~a)" (print-expr (call-expr-fn e)) (string-join (map print-expr (call-expr-args e)) ", "))]
       [else
        (cond
          [(hash-ref binary-op code #f)
           => (lambda (op) (format "~a ~a ~a" (print-expr (tree-operand e 0)) op (print-expr (tree-operand e 1))))]
          [else (format "<~a>" code)])])]))

;; ------------------------------------------------------------
;; Statements
;; ------------------------------------------------------------

(define predictor-names
  (hash 'PRED_TREE_EARLY_RETURN "early return (on trees)" 'PRED_CONTINUE "continue"
        'PRED_COLD_LABEL "cold label" 'PRED_HOT_LABEL "hot label"))

(define (print-rhs g)
  (define code (gassign-code g))
  (define a (gassign-rhs1 g)) (define b (gassign-rhs2 g))
  (case (gimple-rhs-class code)
    [(single) (print-expr a)]
    [(unary)
     (case code
       [(nop_expr convert_expr float_expr fix_trunc_expr) (format "(~a) ~a" (print-type (tree-type (gassign-lhs g))) (print-expr a))]
       [(negate_expr) (string-append "-" (print-expr a))]
       [(bit_not_expr) (string-append "~" (print-expr a))]
       [(truth_not_expr) (string-append "!" (print-expr a))]
       [(abs_expr absu_expr) (format "~a <~a>" (string-upcase (symbol->string code)) (print-expr a))]
       [(view_convert_expr) (format "VIEW_CONVERT_EXPR<~a>(~a)" (print-type (tree-type (gassign-lhs g))) (print-expr a))]
       [(addr_expr) (string-append "&" (print-expr a))]
       [else (format "~a <~a>" (string-upcase (symbol->string code)) (print-expr a))])]
    [(binary)
     (cond
       [(memq code angle-ops) (format "~a <~a, ~a>" (string-upcase (symbol->string code)) (print-expr a) (print-expr b))]
       [(hash-ref binary-op code #f) => (lambda (op) (format "~a ~a ~a" (print-expr a) op (print-expr b)))]
       [else (format "~a <~a, ~a>" (string-upcase (symbol->string code)) (print-expr a) (print-expr b))])]
    [(ternary) (format "~a <~a, ~a, ~a>" (string-upcase (symbol->string code)) (print-expr a) (print-expr b) (print-expr (gassign-rhs3 g)))]
    [else "<?>"]))

(define (print-case c)
  (define lo (case-low c)) (define hi (case-high c))
  (cond
    [(not lo) (format "default: ~a" (label-name (case-label c)))]
    [hi (format "case ~a ... ~a: ~a" (print-expr lo) (print-expr hi) (label-name (case-label c)))]
    [else (format "case ~a: ~a" (print-expr lo) (label-name (case-label c)))]))

(define (print-stmt g indent out)
  (define pad (make-string indent #\space))
  (match g
    [(gbind vars body _)
     (fprintf out "~a{\n" pad)
     (for ([v vars]) (fprintf out "~a  ~a ~a;\n" pad (print-type (tree-type v)) (decl-print-name v)))
     (unless (null? vars) (fprintf out "\n"))
     (for ([s (seq->list body)]) (print-stmt s (+ indent 2) out))
     (fprintf out "~a}\n" pad)]
    [(gassign lhs _ _ _ _) (fprintf out "~a~a = ~a;\n" pad (print-expr lhs) (print-rhs g))]
    [(gcall lhs fn args _ _)
     (fprintf out "~a~a~a (~a);\n" pad (if lhs (string-append (print-expr lhs) " = ") "")
              (print-expr (if (eq? (tree-code fn) 'addr_expr) (tree-operand fn 0) fn))
              (string-join (map print-expr args) ", "))]
    [(gcond code l r t f)
     (fprintf out "~aif (~a ~a ~a) goto ~a; else goto ~a;\n" pad (print-expr l) (hash-ref binary-op code "?") (print-expr r) (label-name t) (label-name f))]
    [(glabel l) (fprintf out "~a~a:\n" pad (label-name l))]
    [(ggoto l) (fprintf out "~agoto ~a;\n" pad (label-name l))]
    [(greturn v) (fprintf out "~areturn~a;\n" pad (if v (string-append " " (print-expr v)) ""))]
    [(gswitch i d cs)
     (fprintf out "~aswitch (~a) <~a>\n" pad (print-expr i) (string-join (cons (print-case d) (map print-case cs)) ", "))]
    [(gpredict p o)
     (fprintf out "~a// predicted ~a by ~a predictor.\n" pad (if (eq? o 'TAKEN) "likely" "unlikely") (hash-ref predictor-names p (symbol->string p)))]
    [(gnop) (void)]
    [_ (fprintf out "~a<~a>\n" pad g)]))

;; dump_function_to_file header: "rettype name (type param, ...)" then the body bind
(define (print-function fndecl bind out)
  (define ftype (tree-type fndecl))
  (define params (decl-arguments fndecl))
  (fprintf out "~a ~a (~a)\n" (print-type (tree-type ftype)) (decl-name fndecl)
           (string-join (for/list ([p params]) (format "~a ~a" (print-type (tree-type p)) (decl-print-name p))) ", "))
  (print-stmt bind 0 out)
  (fprintf out "\n\n"))

(define (print-function->string fndecl bind)
  (define o (open-output-string))
  (print-function fndecl bind o)
  (get-output-string o))
