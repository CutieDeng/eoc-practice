#lang racket/base

;; ============================================================
;; opt: the alias oracle (gcc/tree-ssa-alias.cc, tree-dfa.cc), the
;; subset the -O1 passes use, without type-based aliasing
;; ============================================================
;;
;; -fstrict-aliasing is off at -O1, so every alias set is 0 and the
;; TBAA tails of the oracle are not taken; points-to information does
;; not exist (pass_build_ealias is not ported), so a pointer may point
;; to any variable whose address is taken (ptr_deref_may_alias_decl_p
;; without SSA_NAME_PTR_INFO), as GCC does without it.
;;
;;   ao-ref            : (ref base offset size max-size volatile?) in bits,
;;                       -1 = unknown (ao_ref with ao_ref_base applied)
;;   (make-ao-ref st t), (ao-ref-of-decl st d)
;;   (get-ref-base-and-extent st exp) -> (values base offset size max-size)
;;   (refs-may-alias? st r1 r2)
;;   (stmt-may-clobber-ref? st m g r)       (stmt_may_clobber_ref_p_1)
;;   (ref-maybe-used-by-stmt? st m g r)     (ref_maybe_used_by_stmt_p)
;;   (walk-non-aliased-vuses st m dom r vuse walker translate valueize limit)
;;
;; Not ported: TARGET_MEM_REF, bit fields with variable position,
;; variable array indices with ranges, modref summaries, fnspecs,
;; MR_DEPENDENCE cliques, nonoverlapping_component_refs refinements
;; (a conservative "may alias" is returned where they would decide).
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
         "ssa-ops.rkt"
         "fold.rkt"
         "mfn.rkt"
         "simplify.rkt")

(provide (struct-out ao-ref) make-ao-ref ao-ref-of-decl get-ref-base-and-extent
         ranges-maybe-overlap? ranges-known-overlap? known-subrange?
         refs-may-alias? stmt-may-clobber-ref? ref-maybe-used-by-stmt?
         walk-non-aliased-vuses may-be-aliased? is-global-var? type-size-bits)

(define (icst? t) (and t (eq? (tree-code t) 'integer_cst)))
(define (cst-value t) (and (icst? t) (integer-cst-value t)))

;; TYPE_SIZE in bits or #f
(define (type-size-bits st ty) (and ty (cst-value (type-attr st ty 'size #f))))

;; ------------------------------------------------------------
;; ranges (bits); size -1 = unknown
;; ------------------------------------------------------------

(define (ranges-maybe-overlap? pos1 size1 pos2 size2)
  (cond
    [(and (= size1 -1) (= size2 -1)) #t]
    [(= size1 -1) (< pos2 (+ pos1 1)) #t]      ; unknown size: everything from pos1 on
    [else
     (cond
       [(= size1 -1) (<= pos1 (+ pos2 (max size2 0)))]
       [(= size2 -1) (<= pos2 (+ pos1 size1))]
       [else (and (< pos1 (+ pos2 size2)) (< pos2 (+ pos1 size1)))])]))
(define (ranges-known-overlap? pos1 size1 pos2 size2)
  (and (>= size1 0) (>= size2 0) (> size1 0) (> size2 0)
       (< pos1 (+ pos2 size2)) (< pos2 (+ pos1 size1))))
(define (known-subrange? pos1 size1 pos2 size2)
  (and (>= size1 0) (>= size2 0) (>= pos1 pos2) (<= (+ pos1 size1) (+ pos2 size2))))

;; ------------------------------------------------------------
;; get_ref_base_and_extent
;; ------------------------------------------------------------

(struct ao-ref (ref base offset size max-size volatile?) #:transparent)

;; -> (values base offset size max-size), bits
(define (get-ref-base-and-extent st exp0)
  (define (sz-of t) (type-size-bits st (tree-type t)))
  (define bitsize
    (cond
      [(eq? (tree-code exp0) 'component_ref) (cst-value (tree-attr (tree-operand exp0 1) 'size #f))]
      [(eq? (tree-code exp0) 'bit_field_ref) (cst-value (tree-operand exp0 1))]
      [(void-type-p st (tree-type exp0)) #f]
      [else (sz-of exp0)]))
  (let loop ([exp exp0] [bit-offset 0] [maxsize bitsize] [seen-var-array #f])
    (define (finish exp bit-offset maxsize)
      (define maxsize1
        (cond
          [(and (not maxsize) (decl-p st exp))
           (define ds (cst-value (tree-attr exp 'size #f)))
           (and ds (- ds bit-offset))]
          [(and (not maxsize) (constant-class-p st exp))
           (define ts (sz-of exp))
           (and ts (- ts bit-offset))]
          [else maxsize]))
      (values exp bit-offset (or bitsize -1) (if (and maxsize1 (>= maxsize1 0)) maxsize1 -1)))
    (case (tree-code exp)
      [(bit_field_ref)
       (define o (cst-value (tree-operand exp 2)))
       (if o (loop (tree-operand exp 0) (+ bit-offset o) maxsize seen-var-array) (finish exp 0 #f))]
      [(component_ref)
       (define field (tree-operand exp 1))
       (define foff (cst-value (tree-attr field 'offset #f)))
       (define boff (cst-value (tree-attr field 'bit-offset #f)))
       (cond
         [(and foff boff)
          (define woff (+ (* foff 8) boff))
          ;; a variable array ref below a field: the field's extent may not bound it (trailing arrays)
          (define maxsize1
            (cond
              [(not seen-var-array) maxsize]
              [else #f]))
          (loop (tree-operand exp 0) (+ bit-offset woff) maxsize1 #f)]
         [else
          (define csize (sz-of (tree-operand exp 0)))
          (loop (tree-operand exp 0) bit-offset (and maxsize csize (- csize bit-offset)) seen-var-array)])]
      [(array_ref array_range_ref)
       (define index (tree-operand exp 1))
       (define arr-ty (tree-type (tree-operand exp 0)))
       (define domain (type-attr st arr-ty 'domain #f))
       (define low (if domain (type-attr st domain 'min #f) #f))
       (define low-v (if low (cst-value low) 0))
       (define unit (cst-value (type-attr st (tree-type exp) 'size-unit #f)))
       (cond
         [(and (icst? index) low-v unit)
          (loop (tree-operand exp 0) (+ bit-offset (* (- (integer-cst-value index) low-v) unit 8)) maxsize #f)]
         [else
          (define asize (type-size-bits st arr-ty))
          (loop (tree-operand exp 0) bit-offset (and maxsize asize (- asize bit-offset)) #t)])]
      [(realpart_expr view_convert_expr) (loop (tree-operand exp 0) bit-offset maxsize seen-var-array)]
      [(imagpart_expr) (loop (tree-operand exp 0) (+ bit-offset (or bitsize 0)) maxsize seen-var-array)]
      [(mem_ref)
       (define maxsize1
         (if (and seen-var-array maxsize
                  (let ([ts (sz-of exp)]) (or (not ts) (= (+ bit-offset maxsize) ts))))
             #f maxsize))
       (define base (tree-operand exp 0))
       (cond
         [(eq? (tree-code base) 'addr_expr)
          (define off (mem-ref-offset exp))
          (finish (tree-operand base 0) (+ bit-offset (* off 8)) maxsize1)]
         [else (finish exp bit-offset maxsize1)])]
      [else (finish exp bit-offset maxsize)])))

(define (make-ao-ref st t)
  (define-values (base off size max-size) (get-ref-base-and-extent st t))
  (ao-ref t base off size max-size (and (tree-this-volatile t) #t)))

;; a whole declaration
(define (ao-ref-of-decl st d)
  (define sz (or (cst-value (tree-attr d 'size #f)) -1))
  (ao-ref d d 0 sz sz #f))

;; ------------------------------------------------------------
;; refs_may_alias_p_2 without TBAA
;; ------------------------------------------------------------

(define (is-global-var? d) (or (tree-static d) (decl-external d)))
;; may_be_aliased
(define (may-be-aliased? st d)
  (or (decl-flag st d 'addressable #f) (is-global-var? d)))

;; get_base_address: through handled components and MEM_REF[&x]
(define (get-base-address t)
  (let loop ([t t])
    (cond
      [(handled-component-p t) (loop (tree-operand t 0))]
      [(and (eq? (tree-code t) 'mem_ref) (eq? (tree-code (tree-operand t 0)) 'addr_expr)) (loop (tree-operand (tree-operand t 0) 0))]
      [else t])))

;; ptr_deref_may_alias_decl_p
(define (ptr-deref-may-alias-decl? st ptr decl)
  (define p (strip-nops st ptr))
  (cond
    [(not (memq (tree-code decl) '(var_decl parm_decl result_decl))) #t]
    [(eq? (tree-code p) 'pointer_plus_expr) (ptr-deref-may-alias-decl? st (tree-operand p 0) decl)]
    [(eq? (tree-code p) 'addr_expr)
     (define base (get-base-address (tree-operand p 0)))
     (cond
       [(and base (eq? (tree-code base) 'mem_ref)) (ptr-deref-may-alias-decl? st (tree-operand base 0) decl)]
       [(and base (decl-p st base)) (decl-eq? base decl)]
       [(and base (constant-class-p st base)) #f]
       [else #t])]
    [(not (may-be-aliased? st decl)) #f]
    [else #t]))     ; no points-to information

;; ptr_derefs_may_alias_p
(define (ptr-derefs-may-alias? st ptr1 ptr2)
  (define p1 (strip-nops st ptr1)) (define p2 (strip-nops st ptr2))
  (cond
    [(eq? (tree-code p1) 'pointer_plus_expr) (ptr-derefs-may-alias? st (tree-operand p1 0) p2)]
    [(eq? (tree-code p2) 'pointer_plus_expr) (ptr-derefs-may-alias? st p1 (tree-operand p2 0))]
    [(eq? (tree-code p1) 'addr_expr)
     (define base (get-base-address (tree-operand p1 0)))
     (cond
       [(and base (eq? (tree-code base) 'mem_ref)) (ptr-derefs-may-alias? st (tree-operand base 0) p2)]
       [(and base (decl-p st base)) (ptr-deref-may-alias-decl? st p2 base)]
       [(and base (not (constant-class-p st base))) #t]
       [else (ptr-derefs-may-alias-2 st p1 p2)])]
    [else (ptr-derefs-may-alias-2 st p1 p2)]))
(define (ptr-derefs-may-alias-2 st p1 p2)
  (cond
    [(eq? (tree-code p2) 'addr_expr)
     (define base (get-base-address (tree-operand p2 0)))
     (cond
       [(and base (eq? (tree-code base) 'mem_ref)) (ptr-derefs-may-alias? st p1 (tree-operand base 0))]
       [(and base (decl-p st base)) (ptr-deref-may-alias-decl? st p1 base)]
       [else #t])]
    [else #t]))     ; two SSA pointers: no points-to information

(define (decl-refs-may-alias? st base1 off1 max1 size1 base2 off2 max2 size2)
  (cond
    [(not (decl-eq? base1 base2)) #f]
    [(not (ranges-maybe-overlap? off1 max1 off2 max2)) #f]
    [else #t]))

;; indirect_ref_may_alias_decl_p (base1 a MEM_REF, base2 a decl), no TBAA
(define (indirect-ref-may-alias-decl? st base1 off1 max1 size1 base2 off2 max2 size2)
  (define ptr1 (tree-operand base1 0))
  (define moff (* 8 (mem-ref-offset base1)))
  (cond
    [(not (ranges-maybe-overlap? (+ off1 moff) -1 off2 max2)) #f]
    [(let ([dsize2 (cst-value (tree-attr base2 'size #f))]) (and (>= size1 0) dsize2 (< dsize2 size1))) #f]
    [(not (ptr-deref-may-alias-decl? st ptr1 base2)) #f]
    [else #t]))

;; indirect_refs_may_alias_p, no TBAA
(define (indirect-refs-may-alias? st base1 off1 max1 size1 base2 off2 max2 size2)
  (define ptr1 (tree-operand base1 0)) (define ptr2 (tree-operand base2 0))
  (cond
    [(equal? ptr1 ptr2)
     (define moff1 (* 8 (mem-ref-offset base1))) (define moff2 (* 8 (mem-ref-offset base2)))
     (cond
       [(not (ranges-maybe-overlap? (+ off1 moff1) max1 (+ off2 moff2) max2)) #f]
       [else #t])]
    [(not (ptr-derefs-may-alias? st ptr1 ptr2)) #f]
    [else #t]))

(define (refs-may-alias? st r1 r2)
  (define base1 (ao-ref-base r1)) (define base2 (ao-ref-base r2))
  (define (const-ish? b) (or (ssa-name? b) (memq (tree-code b) '(const_decl constructor addr_expr)) (constant-class-p st b)))
  (cond
    [(or (const-ish? base1) (const-ish? base2)) #f]
    [(and (ao-ref-volatile? r1) (ao-ref-volatile? r2)) #t]
    [else
     (define var1? (decl-p st base1)) (define var2? (decl-p st base2))
     (cond
       [(and var1? var2?)
        (decl-refs-may-alias? st base1 (ao-ref-offset r1) (ao-ref-max-size r1) (ao-ref-size r1)
                              base2 (ao-ref-offset r2) (ao-ref-max-size r2) (ao-ref-size r2))]
       [(or (memq (tree-code base1) '(function_decl label_decl)) (memq (tree-code base2) '(function_decl label_decl))) #t]
       [(and var1? (eq? (tree-code base2) 'mem_ref))
        (indirect-ref-may-alias-decl? st base2 (ao-ref-offset r2) (ao-ref-max-size r2) (ao-ref-size r2)
                                      base1 (ao-ref-offset r1) (ao-ref-max-size r1) (ao-ref-size r1))]
       [(and var2? (eq? (tree-code base1) 'mem_ref))
        (indirect-ref-may-alias-decl? st base1 (ao-ref-offset r1) (ao-ref-max-size r1) (ao-ref-size r1)
                                      base2 (ao-ref-offset r2) (ao-ref-max-size r2) (ao-ref-size r2))]
       [(and (eq? (tree-code base1) 'mem_ref) (eq? (tree-code base2) 'mem_ref))
        (indirect-refs-may-alias? st base1 (ao-ref-offset r1) (ao-ref-max-size r1) (ao-ref-size r1)
                                  base2 (ao-ref-offset r2) (ao-ref-max-size r2) (ao-ref-size r2))]
       [else #t])]))

;; ------------------------------------------------------------
;; statements
;; ------------------------------------------------------------

;; call_may_clobber_ref_p_1 without modref / fnspec / points-to
(define (call-may-clobber-ref? st g r)
  (define ecf (gcall-ecf g))
  (cond
    [(not (zero? (bitwise-and ecf (bitwise-ior ECF-CONST ECF-PURE ECF-LOOPING)))) #f]
    [else
     (define base (ao-ref-base r))
     (cond
       [(not base) #t]
       [(or (ssa-name? base) (constant-class-p st base)) #f]
       [(ao-ref-volatile? r) #t]
       [(and (decl-p st base) (not (may-be-aliased? st base))
             (or (decl-flag st base 'readonly #f) (not (is-global-var? base))))
        #f]
       [else #t])]))

;; stmt_may_clobber_ref_p_1
(define (stmt-may-clobber-ref? st g r)
  (cond
    [(gcall? g)
     (define lhs (gcall-lhs g))
     (or (and lhs (not (ssa-name? lhs)) (refs-may-alias? st r (make-ao-ref st lhs)))
         (call-may-clobber-ref? st g r))]
    [(and (gassign? g) (gassign-single-p st g))
     (define lhs (gassign-lhs g))
     (and (not (ssa-name? lhs)) (refs-may-alias? st r (make-ao-ref st lhs)))]
    [else #f]))

;; ref_maybe_used_by_stmt_p (no modref / fnspec / points-to)
(define (ref-maybe-used-by-stmt? st g r)
  (define (tree-uses? t)
    (and t (not (ssa-name? t)) (not (is-gimple-min-invariant st t))
         (refs-may-alias? st (make-ao-ref st t) r)))
  (cond
    [(gassign? g)
     (or (tree-uses? (gassign-rhs1 g)) (tree-uses? (gassign-rhs2 g)) (tree-uses? (gassign-rhs3 g))
         ;; a store to an aggregate lhs reads the aggregate rhs; a memory lhs is not a use
         #f)]
    [(gcall? g)
     (define ecf (gcall-ecf g))
     (cond
       [(not (zero? (bitwise-and ecf ECF-CONST))) (for/or ([a (in-pvector (gcall-args g))]) (tree-uses? a))]
       [(ao-ref-volatile? r) #t]
       [else
        (define base (ao-ref-base r))
        (cond
          [(not base) #t]
          [(and (decl-p st base) (not (may-be-aliased? st base)) (not (is-global-var? base)))
           (for/or ([a (in-pvector (gcall-args g))]) (tree-uses? a))]
          [else #t])])]
    [(greturn? g)
     (define v (greturn-retval g))
     (and v (not (ssa-name? v)) (not (is-gimple-min-invariant st v)) (refs-may-alias? st (make-ao-ref st v) r))]
    [else #f]))

;; ------------------------------------------------------------
;; walk_non_aliased_vuses
;; ------------------------------------------------------------
;;
;; walker    : vuse -> result | 'stop | #f
;; translate : vuse -> result | 'stop | #f   (called on a clobbering definition)
;; valueize  : vuse -> vuse | #f
;; returns the result or #f; LIMIT is a box with the remaining alias queries

(define (walk-non-aliased-vuses st m dom r vuse0 walker translate valueize limit)
  (define visited (make-hasheqv))
  (define (def-of v) (let ([id (mfn-def m v)]) (and id id)))
  (define (bb-of-def v) (let ([id (mfn-def m v)]) (and id (mfn-bb m id))))
  (define (dominates? a b) (dom-dominates? dom (dom-index dom a) (dom-index dom b)))

  ;; maybe_skip_until: #t when TARGET (a box) was reached / set, #f on a clobber or limit
  (define (maybe-skip-until phi-id target target-bb vuse)
    (hash-set! visited (ssa-name-version (gphi-result (mfn-stmt m phi-id))) #t)
    (let loop ([vuse vuse] [bb (mfn-bb m phi-id)])
      (cond
        [(and (unbox target) (ssa-name=? vuse (unbox target))) #t]
        [else
         (define did (def-of vuse))
         (cond
           [(and (not (unbox target)) (or (not did) (dominates? target-bb (mfn-bb m did))))
            (set-box! target vuse) #t]
           [(and did (eqv? did phi-id)) #t]
           [(and did (gphi? (mfn-stmt m did)))
            (cond
              [(hash-ref visited (ssa-name-version (gphi-result (mfn-stmt m did))) #f) #t]
              [else
               (define v2 (get-continuation-for-phi did))
               (and v2 (loop v2 bb))])]
           [(not did) #f]
           [else
            (define g (mfn-stmt m did))
            (cond
              [(<= (unbox limit) 0) #f]
              [else
               (set-box! limit (sub1 (unbox limit)))
               (cond
                 [(and (stmt-may-clobber-ref? st g r)
                       (not (and translate (not (translate vuse)))))
                  #f]
                 [else
                  (define bb2 (mfn-bb m did))
                  (cond
                    [(and (not (= bb2 bb)) (hash-ref visited (ssa-name-version vuse) #f)) #t]
                    [else
                     (unless (= bb2 bb) (hash-set! visited (ssa-name-version vuse) #t))
                     (loop (stmt-vuse g) bb2)])])])])])))

  ;; get_continuation_for_phi
  (define (get-continuation-for-phi phi-id)
    (define ph (mfn-stmt m phi-id))
    (define args (map cdr (pvector->list (gphi-args ph))))
    (define phi-bb (mfn-bb m phi-id))
    (cond
      [(= 1 (length args)) (car args)]
      [else
       ;; an argument defined in a block dominating the PHI is the candidate target
       (define arg0
         (for/first ([a (in-list args)]
                     #:when (or (ssa-default-def? a)
                                (let ([db (bb-of-def a)]) (and db (not (= db phi-bb)) (dominates? db phi-bb)))))
           a))
       (define idom (dom-node dom (dom-idom dom (dom-index dom phi-bb))))
       (define target (box arg0))
       (define sorted (sort (remove-duplicates args ssa-name=?) < #:key ssa-name-version))
       (and (for/and ([a (in-list sorted)])
              (or (and (unbox target) (ssa-name=? a (unbox target)))
                  (maybe-skip-until phi-id target idom a)))
            (unbox target))]))

  (let loop ([vuse vuse0])
    (define res (walker vuse))
    (cond
      [(eq? res 'stop) #f]
      [res res]
      [else
       (define v (if valueize (valueize vuse) vuse))
       (cond
         [(not v) #f]
         [else
          (define did (def-of v))
          (cond
            [(not did) #f]
            [(gphi? (mfn-stmt m did))
             (define v2 (get-continuation-for-phi did))
             (and v2 (loop v2))]
            [(<= (unbox limit) 0) #f]
            [else
             (set-box! limit (sub1 (unbox limit)))
             (define g (mfn-stmt m did))
             (cond
               [(stmt-may-clobber-ref? st g r)
                (cond
                  [(not translate) #f]
                  [else
                   (define tr (translate v))
                   (cond
                     [(eq? tr 'stop) #f]
                     [tr tr]
                     [else
                      (hash-clear! visited)
                      (let ([nv (stmt-vuse g)]) (and nv (loop nv)))])])]
               [else (let ([nv (stmt-vuse g)]) (and nv (loop nv)))])])])])))
