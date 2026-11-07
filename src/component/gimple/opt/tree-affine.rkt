#lang racket/base

;; ============================================================
;; opt: affine combinations of trees (gcc/tree-affine.cc)
;; ============================================================
;;
;; An affine combination is OFFSET + sum (COEF_i * VAL_i) [+ REST] in
;; TYPE; offsets and coefficients are exact integers sign-extended to
;; the precision of TYPE (wide_int_ext_for_comb), at most MAX_AFF_ELTS
;; elements, the overflow folded into REST.  The functions run inside
;; with-gs (generic-fold.rkt) because the conversions may register
;; types.
;;
;;   (aff type offset elts rest)  elts: list of (cons val coef)
;;   (aff-zero t) (aff-const t n) (aff-elt t e)
;;   (aff-scale c n) (aff-add-elt c e n) (aff-add-cst c n) (aff-add c1 c2)
;;   (aff-convert c t) (aff-mult c1 c2) (aff-remove-elt c m)
;;   (tree->aff e t)            tree_to_aff_combination
;;   (tree->aff-expand e t cache)  tree_to_aff_combination_expand
;;                              (CACHE: a mutable hash, def-of: ssa -> stmt)
;;   (aff->tree c)              aff_combination_to_tree
;;   (aff-constant-multiple c div) -> integer or #f
;;   (aff-zero? c) (aff-const? c) (aff-singleton-var? c)
;;   (get-inner-reference ref) -> (values core bitpos toffset)  (subset)
;;   (fold-build-pointer-plus base off)
;; ============================================================

(require racket/list
         "../../../kernel/data/data.rkt"
         "../../../kernel/ir/gimple/types.rkt"
         "../state.rkt"
         "../tree.rkt"
         "generic-fold.rkt")

(provide (struct-out aff) MAX-AFF-ELTS
         aff-zero aff-const aff-elt aff-scale aff-add-elt aff-add-cst aff-add aff-convert aff-mult
         aff-remove-elt tree->aff tree->aff-expand expr->aff aff->tree aff-constant-multiple
         aff-zero? aff-const? aff-singleton-var? aff-n
         get-inner-reference fold-build-pointer-plus sext-to)

(define MAX-AFF-ELTS 8)
(struct aff (type offset elts rest) #:transparent)
(define (aff-n c) (length (aff-elts c)))

(define (sext-to n prec)
  (define m (bitwise-and n (sub1 (arithmetic-shift 1 prec))))
  (if (bitwise-bit-set? m (sub1 prec)) (- m (arithmetic-shift 1 prec)) m))
;; wide_int_ext_for_comb
(define (ext t n) (sext-to n (ty-prec t)))

(define (sizetype*) (sizetype (gs)))
(define (wide->tree t n) (icst t n))

(define (aff-zero t) (aff t 0 '() #f))
(define (aff-const t n) (aff t (ext t n) '() #f))
(define (aff-elt t e) (aff t 0 (list (cons e 1)) #f))

(define (aff-zero? c) (or (not c) (and (null? (aff-elts c)) (zero? (aff-offset c)))))
(define (aff-const? c) (or (not c) (null? (aff-elts c))))
(define (aff-singleton-var? c)
  (and (= 1 (aff-n c)) (zero? (aff-offset c)) (memv (cdar (aff-elts c)) '(1 -1)) #t))

(define (rest-type t) (if (ty-ptr? t) (sizetype*) t))

;; aff_combination_scale
(define (aff-scale c scale-in)
  (define t (aff-type c))
  (define scale (ext t scale-in))
  (cond
    [(= scale 1) c]
    [(= scale 0) (aff-zero t)]
    [else
     (define elts
       (for*/list ([p (in-list (aff-elts c))]
                   [nc (in-value (ext t (* scale (cdr p))))]
                   #:unless (zero? nc))
         (cons (car p) nc)))
     (define off (ext t (* scale (aff-offset c))))
     (cond
       [(not (aff-rest c)) (aff t off elts #f)]
       [(< (length elts) MAX-AFF-ELTS) (aff t off (append elts (list (cons (aff-rest c) scale))) #f)]
       [else (let ([rt (rest-type t)])
               (aff t off elts (gf-build2 'mult_expr rt (aff-rest c) (wide->tree rt scale))))])]))

;; aff_combination_add_elt
(define (aff-add-elt c e scale-in)
  (define t (aff-type c))
  (define scale (ext t scale-in))
  (define elts (aff-elts c))
  (define i (index-where elts (lambda (p) (operand-equal? (car p) e))))
  (cond
    [(zero? scale) c]
    [i
     (define nc (ext t (+ (cdr (list-ref elts i)) scale)))
     (cond
       [(not (zero? nc)) (aff t (aff-offset c) (list-set elts i (cons e nc)) (aff-rest c))]
       [else
        ;; the last element moves into the hole; the rest becomes an element
        (define n1 (sub1 (length elts)))
        (define moved (if (= i n1) (take elts n1) (list-set (take elts n1) i (list-ref elts n1))))
        (if (aff-rest c)
            (aff t (aff-offset c) (append moved (list (cons (aff-rest c) 1))) #f)
            (aff t (aff-offset c) moved #f))])]
    [(< (length elts) MAX-AFF-ELTS) (aff t (aff-offset c) (append elts (list (cons e scale))) (aff-rest c))]
    [else
     (define rt (rest-type t))
     (define e2 (if (= scale 1) (gf-convert rt e) (gf-build2 'mult_expr rt (gf-convert rt e) (wide->tree rt scale))))
     (aff t (aff-offset c) elts (if (aff-rest c) (gf-build2 'plus_expr rt (aff-rest c) e2) e2))]))

(define (aff-add-cst c n) (struct-copy aff c [offset (ext (aff-type c) (+ (aff-offset c) n))]))

;; aff_combination_add
(define (aff-add c1 c2)
  (define c (for/fold ([c (aff-add-cst c1 (aff-offset c2))]) ([p (in-list (aff-elts c2))])
              (aff-add-elt c (car p) (cdr p))))
  (if (aff-rest c2) (aff-add-elt c (aff-rest c2) 1) c))

;; aff_combination_convert
(define (aff-convert c t)
  (define ct (aff-type c))
  (cond
    [(> (ty-prec t) (ty-prec ct)) (tree->aff (gf-convert t (aff->tree c)) t)]
    [else
     (define rest (if (and (aff-rest c) (not (ty-ptr? t))) (gf-convert t (aff-rest c)) (aff-rest c)))
     (cond
       [(= (ty-prec t) (ty-prec ct)) (aff t (aff-offset c) (aff-elts c) rest)]
       [else
        (define elts (for/list ([p (in-list (aff-elts c))] #:unless (zero? (cdr p)))
                       (cons (gf-convert t (car p)) (cdr p))))
        (if (and (< (length elts) MAX-AFF-ELTS) rest)
            (aff t (ext t (aff-offset c)) (append elts (list (cons rest 1))) #f)
            (aff t (ext t (aff-offset c)) elts rest))])]))

(define (convert-code? code) (memq code '(nop_expr convert_expr)))

;; expr_to_aff_combination: an aff or #f
(define (expr->aff code t op0 [op1 #f])
  (case code
    [(pointer_plus_expr)
     (aff-add (tree->aff op0 t) (tree->aff op1 (sizetype*)))]
    [(plus_expr minus_expr)
     (define b (tree->aff op1 t))
     (aff-add (tree->aff op0 t) (if (eq? code 'minus_expr) (aff-scale b -1) b))]
    [(mult_expr)
     (and (icst? op1) (aff-scale (tree->aff op0 t) (icst-val op1)))]
    [(negate_expr) (aff-scale (tree->aff op0 t) -1)]
    [(bit_not_expr) (aff-add-cst (aff-scale (tree->aff op0 t) -1) -1)]
    [(nop_expr convert_expr)
     (define otype t) (define inner op0) (define itype (tree-type inner))
     (define icode (tree-code inner))
     (cond
       [(tree-nop-conversion-p otype itype) (tree->aff op0 t)]
       [(and (memq icode '(plus_expr minus_expr mult_expr))
             (ty-int? itype) (not (ty-ptr? itype)) (ty-int? otype) (not (ty-ptr? otype))
             (> (ty-prec otype) (ty-prec itype)))
        (define i0 (tree-operand inner 0)) (define i1 (tree-operand inner 1))
        (cond
          ;; (T1)(X *+- CST) -> (T1)X *+- (T1)CST, (T1)(X + X) -> (T1)X + (T1)X
          [(and (ty-undef? itype) (or (icst? i1) (and (eq? icode 'plus_expr) (operand-equal? i0 i1))))
           (expr->aff icode otype (gf-convert otype i0) (gf-convert otype i1))]
          ;; the wrapping case needs a range of X (no ranges here)
          [else #f])]
       [else #f])]
    [else #f]))

;; tree_to_aff_combination
(define (tree->aff expr0 t)
  (define expr (gf-strip-nops expr0))
  (define code (tree-code expr))
  (define (n i) (tree-operand expr i))
  (or
   (case code
     [(pointer_plus_expr plus_expr minus_expr mult_expr) (expr->aff code t (n 0) (n 1))]
     [(negate_expr bit_not_expr) (expr->aff code t (n 0))]
     [(nop_expr convert_expr)
      (define c (expr->aff code (tree-type expr) (n 0)))
      (and c (aff-convert c t))]
     [(addr_expr)
      (define ref (n 0))
      (cond
        [(eq? (tree-code ref) 'mem_ref)
         (aff-add (tree->aff (tree-operand ref 0) t) (tree->aff (tree-operand ref 1) (sizetype*)))]
        [else
         (define-values (core0 bitpos toffset) (get-inner-reference ref))
         (cond
           [(not (zero? (remainder bitpos 8))) #f]
           [else
            (define c0 (aff-const t (quotient bitpos 8)))
            (define-values (c1 core)
              (if (eq? (tree-code core0) 'mem_ref)
                  (values (aff-add-cst c0 (icst-val (tree-operand core0 1))) (tree-operand core0 0))
                  (values c0 (addr-of core0))))
            (define c2 (if (eq? (tree-code core) 'addr_expr)
                           (aff-add-elt c1 core 1)
                           (aff-add c1 (tree->aff core t))))
            (if toffset (aff-add c2 (tree->aff toffset t)) c2)])])]
     [(integer_cst) (aff-const t (icst-val expr))]
     [else #f])
   (aff-elt t expr)))

(define (addr-of t)
  (let-values ([(pt st) (pointer-to (gs) (tree-type t))])
    (gs! st)
    (make-tree 'addr_expr #:type pt #:ops (list t) #:attrs (list (cons 'constant #f)))))

;; add_elt_to_tree: EXPR + ELT * SCALE in TYPE
(define (add-elt-to-tree expr t elt0 scale-in)
  (define scale (ext t scale-in))
  (define elt (gf-convert t elt0))
  (cond
    [(= scale 1) (if expr (gf-build2 'plus_expr t expr elt) elt)]
    [(= scale -1) (if expr (gf-build2 'minus_expr t expr elt) (gf-build1 'negate_expr t elt))]
    [(not expr) (gf-build2 'mult_expr t elt (wide->tree t scale))]
    [else
     (define-values (code s) (if (< scale 0) (values 'minus_expr (- scale)) (values 'plus_expr scale)))
     (gf-build2 code t expr (gf-build2 'mult_expr t elt (wide->tree t s)))]))

(define (fold-build-pointer-plus base off)
  (gf-build2 'pointer_plus_expr (tree-type base) base (gf-convert (sizetype*) off)))

;; aff_combination_to_tree
(define (aff->tree c)
  (define ctype (aff-type c))
  (define elts (aff-elts c))
  (define-values (t base elts1)
    (cond
      [(ty-ptr? ctype)
       (if (and (pair? elts) (= 1 (cdar elts)) (ty-ptr? (tree-type (caar elts))))
           (values (sizetype*) (caar elts) (cdr elts))
           (values (sizetype*) #f elts))]
      [else (values ctype #f elts)]))
  (define e1 (for/fold ([e #f]) ([p (in-list elts1)]) (add-elt-to-tree e t (car p) (cdr p))))
  (define e2 (if (aff-rest c) (add-elt-to-tree e1 t (aff-rest c) 1) e1))
  (define off (aff-offset c))
  (define e3 (if (< off 0)
                 (add-elt-to-tree e2 t (wide->tree t (- off)) -1)
                 (add-elt-to-tree e2 t (wide->tree t off) 1)))
  (if base (fold-build-pointer-plus base e3) (gf-convert ctype e3)))

;; aff_combination_remove_elt
(define (aff-remove-elt c m)
  (define elts (aff-elts c))
  (define n1 (sub1 (length elts)))
  (define moved (if (= m n1) (take elts n1) (list-set (take elts n1) m (list-ref elts n1))))
  (if (aff-rest c)
      (aff (aff-type c) (aff-offset c) (append moved (list (cons (aff-rest c) 1))) #f)
      (aff (aff-type c) (aff-offset c) moved #f)))

;; aff_combination_add_product: R + C * COEF * VAL (VAL #f: only C * COEF)
(define (aff-add-product c coef val r)
  (define r1
    (for/fold ([r r]) ([p (in-list (aff-elts c))])
      (define aval (if val (let ([ty (tree-type (car p))]) (gf-build2 'mult_expr ty (car p) (gf-convert ty val))) (car p)))
      (aff-add-elt r aval (* coef (cdr p)))))
  (define r2
    (if (aff-rest c)
        (aff-add-elt r1 (if val (let ([ty (tree-type (aff-rest c))]) (gf-build2 'mult_expr ty (aff-rest c) (gf-convert ty val))) (aff-rest c)) coef)
        r1))
  (if val
      (aff-add-elt r2 val (* coef (aff-offset c)))
      (aff-add-cst r2 (* coef (aff-offset c)))))

;; aff_combination_mult
(define (aff-mult c1 c2)
  (define r0 (aff-zero (aff-type c1)))
  (define r1 (for/fold ([r r0]) ([p (in-list (aff-elts c2))]) (aff-add-product c1 (cdr p) (car p) r)))
  (define r2 (if (aff-rest c2) (aff-add-product c1 1 (aff-rest c2) r1) r1))
  (aff-add-product c1 (aff-offset c2) #f r2))

;; aff_combination_expand / tree_to_aff_combination_expand; DEF-OF maps
;; an SSA name to its defining statement (a gassign) or #f
(define (tree->aff-expand expr t def-of cache)
  (aff-expand (tree->aff expr t) def-of cache))

(define (aff-expand comb def-of cache)
  (define to-add
    (for/fold ([to-add (aff-zero (aff-type comb))]) ([p (in-list (aff-elts comb))])
      (define e (car p))
      (define type (tree-type e))
      (define name (if (and (convert-code? (tree-code e)) (>= (ty-prec type) (ty-prec (tree-type (tree-operand e 0)))))
                       (tree-operand e 0) e))
      (define def (and (eq? (tree-code name) 'ssa_name) (def-of name)))
      (define current
        (and def
             (let ([code (gassign-code def)])
               (or (hash-ref cache (ssa-name-version name) #f)
                   (let ([cur
                          (case code
                            [(pointer_plus_expr plus_expr minus_expr mult_expr)
                             (expr->aff code (tree-type name) (gassign-rhs1 def) (gassign-rhs2 def))]
                            [(negate_expr bit_not_expr) (expr->aff code (tree-type name) (gassign-rhs1 def))]
                            [(nop_expr convert_expr)
                             (or (expr->aff code (tree-type name) (gassign-rhs1 def))
                                 (aff-elt (tree-type name) (gf-convert (tree-type name) (gassign-rhs1 def))))]
                            [(addr_expr integer_cst) (tree->aff (gassign-rhs1 def) (tree-type name))]
                            [else #f])])
                     (and cur
                          (let ([x (aff-expand cur def-of cache)])
                            (hash-set! cache (ssa-name-version name) x)
                            x)))))))
      (cond
        [(not current) to-add]
        [else
         (define cur1 (if (useless-type-conversion-p (gs) (aff-type comb) (aff-type current)) current (aff-convert current (aff-type comb))))
         (define curre (aff-add-elt (aff-zero (aff-type comb)) e (- (cdr p))))
         (aff-add (aff-add to-add (aff-scale cur1 (cdr p))) curre)])))
  (aff-add comb to-add))

;; aff_combination_constant_multiple_p: the multiplier or #f
(define (aff-constant-multiple val div)
  (define mult-set #f) (define mult 0)
  (define (wcm v d)
    (cond
      [(zero? v)
       (cond [(zero? d) #t]
             [(and mult-set (not (zero? mult))) #f]
             [else (set! mult-set #t) (set! mult 0) #t])]
      [(zero? d) #f]
      [(not (zero? (remainder v d))) #f]
      [(and mult-set (not (= mult (quotient v d)))) #f]
      [else (set! mult-set #t) (set! mult (quotient v d)) #t]))
  (cond
    [(and (null? (aff-elts val)) (zero? (aff-offset val))) 0]
    [(not (= (aff-n val) (aff-n div))) #f]
    [(or (aff-rest val) (aff-rest div)) #f]
    [(not (wcm (aff-offset val) (aff-offset div))) #f]
    [(for/and ([p (in-list (aff-elts div))])
       (define elt (findf (lambda (q) (operand-equal? (car q) (car p))) (aff-elts val)))
       (and elt (wcm (cdr elt) (cdr p))))
     mult]
    [else #f]))

;; get_inner_reference for the references ivopts sees: COMPONENT_REFs
;; with constant field offsets, ARRAY_REFs, MEM_REF bases (MEM[&decl]
;; hands back the decl).  (values core bitpos toffset)
(define (get-inner-reference ref)
  (let loop ([e ref] [bitpos 0] [toff #f])
    (define (add-off o) (if toff (gf-build2 'plus_expr (sizetype*) toff o) o))
    (case (tree-code e)
      [(component_ref)
       (define field (tree-operand e 1))
       (define foff (tree-attr field 'offset #f))
       (define boff (tree-attr field 'bit-offset #f))
       (loop (tree-operand e 0) (+ bitpos (* 8 (icst-val foff)) (icst-val boff)) toff)]
      [(array_ref)
       (define idx (tree-operand e 1))
       (define esize (array-elt-size e))
       (define low (array-low-bound e))
       (cond
         [(and (icst? idx) (icst? esize))
          (loop (tree-operand e 0) (+ bitpos (* 8 (- (icst-val idx) low) (icst-val esize))) toff)]
         [else
          (define st (sizetype*))
          (define i (gf-convert st (if (zero? low) idx (gf-build2 'minus_expr (tree-type idx) idx (icst (tree-type idx) low)))))
          (loop (tree-operand e 0) bitpos (add-off (gf-build2 'mult_expr st i (gf-convert st esize))))])]
      [(mem_ref)
       (define p (tree-operand e 0))
       (if (eq? (tree-code p) 'addr_expr)
           (values (tree-operand p 0) (+ bitpos (* 8 (icst-val (tree-operand e 1)))) toff)
           (values e bitpos toff))]
      [else (values e bitpos toff)])))

;; array_ref_element_size / array_ref_low_bound (constant domains)
(define (array-elt-size e)
  (define et (type-type (gs) (tree-type (tree-operand e 0))))
  (or (type-attr (gs) et 'size-unit #f) (icst (sizetype*) (quotient (icst-val (type-attr (gs) et 'size)) 8))))
(define (array-low-bound e)
  (define dom (type-attr (gs) (tree-type (tree-operand e 0)) 'domain #f))
  (define lo (and dom (type-attr (gs) dom 'min #f)))
  (if (and lo (icst? lo)) (icst-val lo) 0))
