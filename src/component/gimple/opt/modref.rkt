#lang racket/base

;; ============================================================
;; opt: pass_modref (gcc/ipa-modref.cc, local mode), dump "modref1"
;; ============================================================
;;
;; 1. analyze_parms: EAF flags of every parameter's default definition
;;    by a DFS over the SSA use graph (modref_eaf_analysis: lattice
;;    flags per SSA name, cycles and deep recursion deferred to an
;;    iterative dataflow over the propagate_to edges).  The dump follows
;;    GCC's immediate-use order (mfn-imm-uses: use-list stamps).
;; 2. modref_access_analysis: loads and stores recorded in two modref
;;    trees (base alias set 0, ref alias set 0 without strict aliasing;
;;    accesses relative to parameters with offsets and ranges, bounded
;;    by --param modref-max-{bases,refs,accesses}), kills of always
;;    executed stores, calls through the callee's summary, its ECF
;;    flags or (no fnspec) unknown / global memory accesses; side
;;    effects, nondeterminism, finiteness (finite_function_p).
;; 3. ipa_make_function_const / pure from the trees, useful_p pruning,
;;    finalize, dump of the summary; the summary is kept as the callee's
;;    'modref declaration flag for later callers (gimple_call_arg_flags,
;;    merge_call_side_effects).
;;
;; Version gates: always executed blocks by find_always_executed_bbs
;; (dumped) from GCC 13, the entry successor only before; GCC 15:
;; useful_p no longer accepts nondeterministic-only summaries of
;; looping const/pure functions, ignore_nondeterminism_p also looks at
;; the caller's const/pure flags.
;;
;; Not ported: fnspec strings and builtins, static chains and return
;; slots, exceptions, IPA mode (escape points, LTO summaries), the
;; past-summary comparison, points-to (pure-const.rkt's local-pointer
;; walk instead), interposable_eaf_flags.
;; ============================================================

(require racket/list racket/string
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
         "mfn.rkt"
         "alias.rkt"
         "simplify.rkt"
         "loops.rkt"
         "pure-const.rkt")

(provide modref)

;; ------------------------------------------------------------
;; flags
;; ------------------------------------------------------------

(define EAF-UNUSED 2) (define EAF-NDC 4) (define EAF-NIC 8) (define EAF-NDE 16) (define EAF-NIE 32)
(define EAF-NRD 64) (define EAF-NRI 128) (define EAF-NDR 256) (define EAF-NIR 512)
(define EAF-ALL (bitwise-ior EAF-UNUSED EAF-NDC EAF-NIC EAF-NDE EAF-NIE EAF-NRD EAF-NRI EAF-NDR EAF-NIR))
(define implicit-const-eaf (bitwise-ior EAF-NDC EAF-NIC EAF-NDE EAF-NIE EAF-NDR EAF-NIR EAF-NRI))
(define implicit-pure-eaf (bitwise-ior EAF-NDC EAF-NIC EAF-NDE EAF-NIE))
(define ignore-stores-eaf implicit-pure-eaf)
(define ERF-RETURN-ARG-MASK 3) (define ERF-RETURNS-ARG 4) (define ERF-NOALIAS 8)
(define ECF-MALLOC 16)
(define (f? flags b) (not (zero? (bitwise-and flags b))))
(define (fnot x) (bitwise-and EAF-ALL (bitwise-not x)))

(define (eaf-flags-string flags)
  (string-append
   (if (f? flags EAF-UNUSED) " unused" "")
   (if (f? flags EAF-NDC) " no_direct_clobber" "")
   (if (f? flags EAF-NIC) " no_indirect_clobber" "")
   (if (f? flags EAF-NDE) " no_direct_escape" "")
   (if (f? flags EAF-NIE) " no_indirect_escape" "")
   (if (f? flags EAF-NRD) " not_returned_directly" "")
   (if (f? flags EAF-NRI) " not_returned_indirectly" "")
   (if (f? flags EAF-NDR) " no_direct_read" "")
   (if (f? flags EAF-NIR) " no_indirect_read" "")))

;; remove_useless_eaf_flags
(define (remove-useless-eaf flags ecf returns-void?)
  (cond
    [(f? ecf ECF-CONST) (bitwise-and flags (bitwise-not implicit-const-eaf))]
    [(f? ecf ECF-PURE) (bitwise-and flags (bitwise-not implicit-pure-eaf))]
    [(or (f? ecf ECF-NORETURN) returns-void?) (bitwise-and flags (bitwise-not (bitwise-ior EAF-NRD EAF-NRI)))]
    [else flags]))

;; parameter indices
(define UNKNOWN-PARM -1) (define STATIC-CHAIN-PARM -2) (define RETSLOT-PARM -3) (define GLOBAL-PARM -4) (define LOCAL-PARM -5)

;; ------------------------------------------------------------
;; a growable vector with GCC's vec semantics (unordered_remove)
;; ------------------------------------------------------------

(struct dyn ([v #:mutable] [n #:mutable]))
(define (make-dyn) (dyn (make-vector 4 #f) 0))
(define (dyn-length d) (dyn-n d))
(define (dyn-ref d i) (vector-ref (dyn-v d) i))
(define (dyn-set! d i x) (vector-set! (dyn-v d) i x))
(define (dyn-push! d x)
  (when (= (dyn-n d) (vector-length (dyn-v d)))
    (define nv (make-vector (* 2 (vector-length (dyn-v d))) #f))
    (vector-copy! nv 0 (dyn-v d))
    (set-dyn-v! d nv))
  (vector-set! (dyn-v d) (dyn-n d) x)
  (set-dyn-n! d (add1 (dyn-n d))))
(define (dyn-unordered-remove! d i)
  (dyn-set! d i (dyn-ref d (sub1 (dyn-n d))))
  (set-dyn-n! d (sub1 (dyn-n d))))
(define (dyn->list d) (for/list ([i (in-range (dyn-n d))]) (dyn-ref d i)))
(define (dyn-clear! d) (set-dyn-n! d 0))
(define (dyn-copy d) (let ([c (make-dyn)]) (for ([x (in-list (dyn->list d))]) (dyn-push! c x)) c))

;; ------------------------------------------------------------
;; modref_access_node
;; ------------------------------------------------------------

(struct acc (offset size max-size parm-offset parm-index parm-offset-known? adjustments) #:mutable #:transparent)
(define (copy-acc a) (acc (acc-offset a) (acc-size a) (acc-max-size a) (acc-parm-offset a) (acc-parm-index a) (acc-parm-offset-known? a) (acc-adjustments a)))
(define (known-size? x) (not (= x -1)))
(define (acc-useful? a) (not (= (acc-parm-index a) UNKNOWN-PARM)))
(define (acc-useful-for-kill? a)
  (and (acc-parm-offset-known? a)
       (not (memv (acc-parm-index a) (list UNKNOWN-PARM GLOBAL-PARM RETSLOT-PARM)))
       (known-size? (acc-size a)) (= (acc-max-size a) (acc-size a)) (> (acc-size a) 0)))
(define (acc-range-useful? a)
  (and (not (memv (acc-parm-index a) (list UNKNOWN-PARM GLOBAL-PARM)))
       (acc-parm-offset-known? a)
       (or (known-size? (acc-size a)) (known-size? (acc-max-size a)) (>= (acc-offset a) 0))))
(define (acc->string a)
  (string-append
   (cond
     [(= (acc-parm-index a) UNKNOWN-PARM) ""]
     [else
      (string-append
       (cond
         [(= (acc-parm-index a) GLOBAL-PARM) " Base in global memory"]
         [(>= (acc-parm-index a) 0) (format " Parm ~a" (acc-parm-index a))]
         [(= (acc-parm-index a) STATIC-CHAIN-PARM) " Static chain"]
         [else " ?"])
       (if (acc-parm-offset-known? a) (format " param offset:~a" (acc-parm-offset a)) ""))])
   (if (acc-range-useful? a)
       (string-append (format " offset:~a size:~a max_size:~a" (acc-offset a) (acc-size a) (acc-max-size a))
                      (if (zero? (acc-adjustments a)) "" (format " adjusted ~a times" (acc-adjustments a))))
       "")))

(define param-modref-max-bases 32)
(define param-modref-max-refs 16)
(define param-modref-max-accesses 16)
(define param-modref-max-adjustments 8)
(define param-modref-max-tests 64)
(define param-modref-max-depth 256)

(define current-msg! (make-parameter (lambda (s) (void))))
(define (msg s) ((current-msg!) s))

;; contains
(define (acc-contains? this a)
  (let/ec k
    (define adj 0)
    (unless (= (acc-parm-index this) UNKNOWN-PARM)
      (unless (= (acc-parm-index this) (acc-parm-index a)) (k #f))
      (when (acc-parm-offset-known? this)
        (unless (acc-parm-offset-known? a) (k #f))
        (when (and (not (<= (acc-parm-offset this) (acc-parm-offset a))) (not (acc-range-useful? this))) (k #f))
        (set! adj (* 8 (- (acc-parm-offset a) (acc-parm-offset this))))))
    (cond
      [(acc-range-useful? this)
       (unless (acc-range-useful? a) (k #f))
       (when (and (known-size? (acc-size this))
                  (or (not (known-size? (acc-size a))) (not (<= (acc-size this) (acc-size a)))))
         (k #f))
       (if (known-size? (acc-max-size this))
           (known-subrange? (+ (acc-offset a) adj) (acc-max-size a) (acc-offset this) (acc-max-size this))
           (<= (acc-offset this) (+ (acc-offset a) adj)))]
      [else #t])))

;; update
(define (acc-update! this parm-offset1 offset1 size1 max-size1 record?)
  (unless (and (= (acc-parm-offset this) parm-offset1) (= (acc-offset this) offset1)
               (= (acc-size this) size1) (= (acc-max-size this) max-size1))
    (cond
      [(or (not record?) (begin (set-acc-adjustments! this (add1 (acc-adjustments this)))
                                (< (acc-adjustments this) param-modref-max-adjustments)))
       (set-acc-parm-offset! this parm-offset1) (set-acc-offset! this offset1)
       (set-acc-size! this size1) (set-acc-max-size! this max-size1)]
      [else
       (define parts '())
       (unless (= (acc-parm-offset this) parm-offset1) (set! parts (cons " parm_offset cleared" parts)) (set-acc-parm-offset-known?! this #f))
       (unless (= (acc-size this) size1) (set-acc-size! this -1) (set! parts (cons " size cleared" parts)))
       (unless (= (acc-max-size this) max-size1) (set-acc-max-size! this -1) (set! parts (cons " max_size cleared" parts)))
       (unless (= (acc-offset this) offset1) (set-acc-offset! this 0) (set! parts (cons " offset cleared" parts)))
       (msg (string-append "--param modref-max-adjustments limit reached:" (apply string-append (reverse parts))))])))

;; combined_offsets: (values ok? new-parm-offset new-offset new-aoffset)
(define (combined-offsets this a)
  (cond
    [(<= (acc-parm-offset a) (acc-parm-offset this))
     (values #t (acc-parm-offset a) (+ (acc-offset this) (* 8 (- (acc-parm-offset this) (acc-parm-offset a)))) (acc-offset a))]
    [(<= (acc-parm-offset this) (acc-parm-offset a))
     (values #t (acc-parm-offset this) (acc-offset this) (+ (acc-offset a) (* 8 (- (acc-parm-offset a) (acc-parm-offset this)))))]
    [else (values #f 0 0 0)]))

;; update2
(define (acc-update2! this parm-offset1 offset1 size1 max-size1 offset2 size2 max-size2 record?)
  (define new-size (if (or (not (known-size? size2)) (<= size2 size1)) size2 size1))
  (define-values (o1 m1 o2 m2) (if (<= offset1 offset2) (values offset1 max-size1 offset2 max-size2) (values offset2 max-size2 offset1 max-size1)))
  (define new-max
    (cond
      [(not (known-size? m1)) m1]
      [(not (known-size? m2)) m2]
      [else (define s (- (+ m2 o2) o1)) (if (<= s m1) m1 s)]))
  (acc-update! this parm-offset1 o1 new-size new-max record?))

;; merge
(define (acc-merge! this a record?)
  (let/ec k
    (define-values (new-parm-offset offset1 aoffset1) (values 0 0 0))
    (unless (= (acc-parm-index this) UNKNOWN-PARM)
      (unless (= (acc-parm-index this) (acc-parm-index a)) (k #f))
      (when (acc-parm-offset-known? this)
        (unless (acc-parm-offset-known? a) (k #f))
        (define-values (ok np o1 ao1) (combined-offsets this a))
        (unless ok (k #f))
        (set! new-parm-offset np) (set! offset1 o1) (set! aoffset1 ao1)))
    (cond
      [(acc-range-useful? this)
       (cond
         [(and (known-size? (acc-size this)) (or (not (known-size? (acc-size a))) (< (acc-size a) (acc-size this))))
          (when (or (and (or (known-size? (acc-max-size this)) (known-size? (acc-max-size a)))
                         (not (= (acc-max-size this) (acc-max-size a))))
                    (not (= offset1 aoffset1)))
            (k #f))
          (acc-update! this new-parm-offset offset1 (acc-size a) (acc-max-size this) record?)
          #t]
         [(and (or (known-size? (acc-size this)) (known-size? (acc-size a))) (not (= (acc-size this) (acc-size a)))) #f]
         [(<= offset1 aoffset1)
          (cond
            [(or (not (known-size? (acc-max-size this))) (>= (+ offset1 (acc-max-size this)) aoffset1))
             (acc-update2! this new-parm-offset offset1 (acc-size this) (acc-max-size this) aoffset1 (acc-size a) (acc-max-size a) record?)
             #t]
            [else #f])]
         [(<= aoffset1 offset1)
          (cond
            [(or (not (known-size? (acc-max-size a))) (>= (+ aoffset1 (acc-max-size a)) offset1))
             (acc-update2! this new-parm-offset offset1 (acc-size this) (acc-max-size this) aoffset1 (acc-size a) (acc-max-size a) record?)
             #t]
            [else #f])]
         [else #f])]
      [else
       (acc-update! this new-parm-offset offset1 (acc-size this) (acc-max-size this) record?)
       #t])))

;; closer_pair_p
(define (closer-pair? a1 b1 a2 b2)
  (cond
    [(not (= (acc-parm-index a1) (acc-parm-index b1))) #f]
    [(not (= (acc-parm-index a2) (acc-parm-index b2))) #t]
    [else
     (define-values (ok1 np1 oa1 ob1) (combined-offsets a1 b1))
     (define-values (ok2 np2 oa2 ob2) (combined-offsets a2 b2))
     (define dist1 (if (<= oa1 ob1)
                       (if (known-size? (acc-max-size a1)) (- ob1 oa1 (acc-max-size a1)) 0)
                       (if (known-size? (acc-max-size b1)) (- oa1 ob1 (acc-max-size b1)) 0)))
     (define dist2 (if (<= oa2 ob2)
                       (if (known-size? (acc-max-size a2)) (- ob2 oa2 (acc-max-size a2)) 0)
                       (if (known-size? (acc-max-size b2)) (- oa2 ob2 (acc-max-size b2)) 0)))
     (cond
       [(and (< dist1 0) (>= dist2 0)) #t]
       [(and (< dist2 0) (>= dist1 0)) #f]
       [(< dist1 0) (<= dist2 dist1)]
       [else (<= dist1 dist2)])]))

;; forced_merge
(define (acc-forced-merge! this a record?)
  (cond
    [(not (= (acc-parm-index this) (acc-parm-index a))) (set-acc-parm-index! this UNKNOWN-PARM)]
    [else
     (define-values (ok np o1 ao1) (combined-offsets this a))
     (cond
       [(not ok) (set-acc-parm-offset-known?! this #f)]
       [else
        (when record? (set-acc-adjustments! this (+ (acc-adjustments this) (acc-adjustments a))))
        (acc-update2! this np o1 (acc-size this) (acc-max-size this) ao1 (acc-size a) (acc-max-size a) record?)])]))

;; try_merge_with
(define (try-merge-with! accesses index)
  (let loop ([i 0] [index index])
    (when (< i (dyn-length accesses))
      (cond
        [(= i index) (loop (add1 i) index)]
        [else
         (define a (dyn-ref accesses i))
         (define n (dyn-ref accesses index))
         (define found (acc-contains? n a))
         (define restart (and (not found) (acc-merge! n a #f)))
         (cond
           [(or found restart)
            (dyn-unordered-remove! accesses i)
            (define-values (index* i*) (if (= index (dyn-length accesses)) (values i (add1 i)) (values index i)))
            (loop (if restart 0 i*) index*)]
           [else (loop (add1 i) index)])]))))

;; insert: 0 nothing changed, 1 inserted, -1 failed
(define (accesses-insert! accesses a max-accesses record?)
  (let/ec k
    (for ([i (in-range (dyn-length accesses))])
      (define a2 (dyn-ref accesses i))
      (when (acc-contains? a2 a) (k 0))
      (when (acc-contains? a a2)
        (set-acc-adjustments! a 0)
        (set-acc-parm-index! a2 (acc-parm-index a))
        (set-acc-parm-offset-known?! a2 (acc-parm-offset-known? a))
        (acc-update! a2 (acc-parm-offset a) (acc-offset a) (acc-size a) (acc-max-size a) record?)
        (try-merge-with! accesses i)
        (k 1))
      (when (acc-merge! a2 a record?)
        (try-merge-with! accesses i)
        (k 1)))
    (cond
      [(>= (dyn-length accesses) max-accesses)
       (when (< max-accesses 2) (k -1))
       (define best1 -1) (define best2 -1)
       (for ([i (in-range (dyn-length accesses))])
         (define a2 (dyn-ref accesses i))
         (for ([j (in-range (add1 i) (dyn-length accesses))])
           (when (or (< best1 0)
                     (closer-pair? a2 (dyn-ref accesses j) (dyn-ref accesses best1) (if (< best2 0) a (dyn-ref accesses best2))))
             (set! best1 i) (set! best2 j)))
         (when (closer-pair? a2 a (dyn-ref accesses best1) (if (< best2 0) a (dyn-ref accesses best2)))
           (set! best1 i) (set! best2 -1)))
       (acc-forced-merge! (dyn-ref accesses best1) (if (< best2 0) a (dyn-ref accesses best2)) record?)
       (unless (acc-useful? (dyn-ref accesses best1)) (k -1))
       (if (>= best2 0)
           (msg (format "--param modref-max-accesses limit reached; merging ~a and ~a" best1 best2))
           (msg (format "--param modref-max-accesses limit reached; merging with ~a" best1)))
       (try-merge-with! accesses best1)
       (when (>= best2 0) (accesses-insert! accesses a max-accesses record?))
       1]
      [else
       (set-acc-adjustments! a 0)
       (dyn-push! accesses a)
       1])))

;; ---- kills
(define (acc-contains-for-kills? this a)
  (and (= (acc-parm-index this) (acc-parm-index a))
       (let ([adj (* 8 (- (acc-parm-offset a) (acc-parm-offset this)))])
         (known-subrange? (+ (acc-offset a) adj) (acc-max-size a) (acc-offset this) (acc-max-size this)))))
(define (acc-update-for-kills! this parm-offset1 offset1 max-size1 offset2 max-size2 record?)
  (define-values (o1 m1 o2 m2) (if (<= offset1 offset2) (values offset1 max-size1 offset2 max-size2) (values offset2 max-size2 offset1 max-size1)))
  (define new-max (let ([s (- (+ m2 o2) o1)]) (if (<= s m1) m1 s)))
  (cond
    [(and (= (acc-parm-offset this) parm-offset1) (= (acc-offset this) o1) (= (acc-size this) new-max) (= (acc-max-size this) new-max)) #f]
    [(or (not record?) (begin (set-acc-adjustments! this (add1 (acc-adjustments this))) (< (acc-adjustments this) param-modref-max-adjustments)))
     (set-acc-parm-offset! this parm-offset1) (set-acc-offset! this o1) (set-acc-max-size! this new-max) (set-acc-size! this new-max)
     #t]
    [else #f]))
(define (acc-merge-for-kills! this a record?)
  (define-values (ok np o1 ao1) (combined-offsets this a))
  (cond
    [(or (not (= (acc-parm-index this) (acc-parm-index a))) (not ok)) #f]
    [(<= o1 ao1)
     (if (or (not (known-size? (acc-max-size this))) (>= (+ o1 (acc-max-size this)) ao1))
         (acc-update-for-kills! this np o1 (acc-max-size this) ao1 (acc-max-size a) record?)
         #f)]
    [(<= ao1 o1)
     (if (or (not (known-size? (acc-max-size a))) (>= (+ ao1 (acc-max-size a)) o1))
         (acc-update-for-kills! this np o1 (acc-max-size this) ao1 (acc-max-size a) record?)
         #f)]
    [else #f]))
(define (insert-kill! kills a record?)
  (let/ec k
    (define index
      (for/first ([i (in-range (dyn-length kills))]
                  #:when (let ([a2 (dyn-ref kills i)])
                           (cond
                             [(acc-contains-for-kills? a2 a) (k #f)]
                             [(acc-contains-for-kills? a a2) (set-acc-adjustments! a 0) (dyn-set! kills i a) #t]
                             [(acc-merge-for-kills! a2 a record?) #t]
                             [else #f])))
        i))
    (cond
      [(not index)
       (cond
         [(>= (dyn-length kills) param-modref-max-accesses)
          (msg "--param modref-max-accesses limit reached:")
          #f]
         [else (set-acc-adjustments! a 0) (dyn-push! kills a) #t])]
      [else
       (let loop ([i 0] [index index])
         (when (< i (dyn-length kills))
           (cond
             [(= i index) (loop (add1 i) index)]
             [else
              (define a (dyn-ref kills i)) (define n (dyn-ref kills index))
              (define found (acc-contains-for-kills? n a))
              (define restart (and (not found) (acc-merge-for-kills! n a #f)))
              (cond
                [(or found restart)
                 (dyn-unordered-remove! kills i)
                 (define-values (index* i*) (if (= index (dyn-length kills)) (values i (add1 i)) (values index i)))
                 (loop (if restart 0 i*) index*)]
                [else (loop (add1 i) index)])])))
       #t])))

;; ------------------------------------------------------------
;; modref_tree (bases -> refs -> accesses)
;; ------------------------------------------------------------

(struct ref-node (ref [accesses #:mutable] [every-access? #:mutable]))
(struct base-node (base [refs #:mutable] [every-ref? #:mutable]))
(struct mtree ([bases #:mutable] [every-base? #:mutable]))
(define (make-mtree) (mtree #f #f))
(define (ref-collapse! r) (set-ref-node-accesses! r #f) (set-ref-node-every-access?! r #t))
(define (base-collapse! b) (set-base-node-refs! b #f) (set-base-node-every-ref?! b #t))
(define (tree-collapse! t) (set-mtree-bases! t #f) (set-mtree-every-base?! t #t))
(define (tree-search t base) (and (mtree-bases t) (for/first ([b (in-list (dyn->list (mtree-bases t)))] #:when (= (base-node-base b) base)) b)))
(define (base-search b ref) (and (base-node-refs b) (for/first ([r (in-list (dyn->list (base-node-refs b)))] #:when (= (ref-node-ref r) ref)) r)))

(define (ref-insert-access! r a max-accesses record?)
  (cond
    [(ref-node-every-access? r) #f]
    [(not (acc-useful? a)) (ref-collapse! r) #t]
    [else
     (unless (ref-node-accesses r) (set-ref-node-accesses! r (make-dyn)))
     (define ret (accesses-insert! (ref-node-accesses r) a max-accesses record?))
     (when (= ret -1)
       (msg "--param modref-max-accesses limit reached; collapsing")
       (ref-collapse! r))
     (not (= ret 0))]))

(define (base-insert-ref! b ref max-refs changed)
  (cond
    [(base-node-every-ref? b) #f]
    [else
     (or (base-search b ref)
         (let ()
           (define ref*
             (cond
               [(and (not (zero? ref)) (base-node-refs b) (>= (dyn-length (base-node-refs b)) max-refs))
                (msg "--param modref-max-refs limit reached; using 0")
                0]
               [else ref]))
           (or (and (not (= ref* ref)) (base-search b ref*))
               (let ([r (ref-node ref* #f #f)])
                 (set-box! changed #t)
                 (unless (base-node-refs b) (set-base-node-refs! b (make-dyn)))
                 (dyn-push! (base-node-refs b) r)
                 r))))]))

(define (tree-insert-base! t base ref max-bases changed)
  (cond
    [(mtree-every-base? t) #f]
    [else
     (or (tree-search t base)
         (let ()
           (define base*
             (cond
               [(and (not (zero? base)) (mtree-bases t) (>= (dyn-length (mtree-bases t)) max-bases))
                (cond
                  [(tree-search t ref) (msg "--param modref-max-bases limit reached; using ref") ref]
                  [else (msg "--param modref-max-bases limit reached; using 0") 0])]
               [else base]))
           (or (and (not (= base* base)) (tree-search t base*))
               (let ([b (base-node base* #f #f)])
                 (set-box! changed #t)
                 (unless (mtree-bases t) (set-mtree-bases! t (make-dyn)))
                 (dyn-push! (mtree-bases t) b)
                 b))))]))

(define (tree-insert! t base ref a0 record? [max-bases param-modref-max-bases] [max-refs param-modref-max-refs] [max-accesses param-modref-max-accesses])
  (define a (copy-acc a0))
  (let/ec k
    (when (mtree-every-base? t) (k #f))
    (define changed (box #f))
    (when (and (acc-range-useful? a) (known-size? (acc-size a)) (known-size? (acc-max-size a)) (< (acc-max-size a) (acc-size a)))
      (msg "   - Paradoxical range. Ignoring") (k #f))
    (when (and (known-size? (acc-size a)) (zero? (acc-size a))) (msg "   - Zero size. Ignoring") (k #f))
    (when (and (known-size? (acc-max-size a)) (zero? (acc-max-size a))) (msg "   - Zero max_size. Ignoring") (k #f))
    (when (and (zero? base) (zero? ref) (not (acc-useful? a))) (tree-collapse! t) (k #t))
    (define b (tree-insert-base! t base ref max-bases changed))
    (define base* (base-node-base b))
    (when (and (zero? base*) (zero? ref) (not (acc-useful? a))) (tree-collapse! t) (k #t))
    (when (base-node-every-ref? b) (k (unbox changed)))
    (when (and (zero? ref) (not (acc-useful? a))) (base-collapse! b) (k #t))
    (define r (base-insert-ref! b ref max-refs changed))
    (define ref* (ref-node-ref r))
    (when (ref-node-every-access? r) (k (unbox changed)))
    (define ch (or (ref-insert-access! r a max-accesses record?) (unbox changed)))
    (when (ref-node-every-access? r)
      (cond
        [(and (zero? base*) (zero? ref*)) (tree-collapse! t)]
        [(zero? ref*) (base-collapse! b)]))
    ch))

(define (tree-cleanup! t)
  (when (mtree-bases t)
    (let loop ([i 0])
      (when (< i (dyn-length (mtree-bases t)))
        (define b (dyn-ref (mtree-bases t) i))
        (when (base-node-refs b)
          (let rloop ([j 0])
            (when (< j (dyn-length (base-node-refs b)))
              (define r (dyn-ref (base-node-refs b) j))
              (cond
                [(and (not (ref-node-every-access? r)) (or (not (ref-node-accesses r)) (zero? (dyn-length (ref-node-accesses r)))))
                 (dyn-unordered-remove! (base-node-refs b) j) (rloop j)]
                [else (rloop (add1 j))]))))
        (cond
          [(and (not (base-node-every-ref? b)) (or (not (base-node-refs b)) (zero? (dyn-length (base-node-refs b)))))
           (dyn-unordered-remove! (mtree-bases t) i) (loop i)]
          [else (loop (add1 i))])))
    (when (zero? (dyn-length (mtree-bases t))) (set-mtree-bases! t #f))))

(define unspecified-acc (acc 0 -1 -1 0 UNKNOWN-PARM #f 0))

;; merge OTHER into T; PARM-MAP: vector of (vector index known? offset) or #f
(define (tree-merge! t other parm-map chain-map record? promote-unknown?)
  (let/ec k
    (when (or (not other) (mtree-every-base? t)) (k #f))
    (when (mtree-every-base? other) (tree-collapse! t) (k #t))
    (define changed #f)
    (define other* (if (eq? other t) (tree-copy other) other))
    (for ([b (in-list (if (mtree-bases other*) (dyn->list (mtree-bases other*)) '()))])
      (cond
        [(base-node-every-ref? b)
         (define cb (box #f))
         (define mb (tree-insert-base! t (base-node-base b) 0 param-modref-max-bases cb))
         (when (unbox cb) (set! changed #t))
         (when (and mb (not (base-node-every-ref? mb)))
           (base-collapse! mb) (tree-cleanup! t) (set! changed #t))]
        [else
         (for ([r (in-list (if (base-node-refs b) (dyn->list (base-node-refs b)) '()))])
           (cond
             [(ref-node-every-access? r)
              (when (tree-insert! t (base-node-base b) (ref-node-ref r) unspecified-acc record?) (set! changed #t))]
             [else
              (for ([a0 (in-list (if (ref-node-accesses r) (dyn->list (ref-node-accesses r)) '()))])
                (let/ec next
                  (define a (copy-acc a0))
                  (when (and (not (memv (acc-parm-index a) (list UNKNOWN-PARM GLOBAL-PARM))) parm-map)
                    (cond
                      [(>= (acc-parm-index a) (vector-length parm-map)) (set-acc-parm-index! a UNKNOWN-PARM)]
                      [else
                       (define m (if (= (acc-parm-index a) STATIC-CHAIN-PARM) chain-map (vector-ref parm-map (acc-parm-index a))))
                       (when (= (vector-ref m 0) LOCAL-PARM) (next (void)))
                       (set-acc-parm-offset! a (+ (acc-parm-offset a) (vector-ref m 2)))
                       (set-acc-parm-offset-known?! a (and (acc-parm-offset-known? a) (vector-ref m 1)))
                       (set-acc-parm-index! a (vector-ref m 0))]))
                  (when (and (= (acc-parm-index a) UNKNOWN-PARM) promote-unknown?) (set-acc-parm-index! a GLOBAL-PARM))
                  (when (tree-insert! t (base-node-base b) (ref-node-ref r) a record?) (set! changed #t))))]))]))
    changed))

(define (tree-copy other)
  (define t (make-mtree))
  (tree-merge! t other #f #f #f #f)
  t)

(define (tree-global-access? t)
  (or (mtree-every-base? t)
      (for/or ([b (in-list (if (mtree-bases t) (dyn->list (mtree-bases t)) '()))])
        (or (base-node-every-ref? b)
            (for/or ([r (in-list (if (base-node-refs b) (dyn->list (base-node-refs b)) '()))])
              (or (ref-node-every-access? r)
                  (for/or ([a (in-list (if (ref-node-accesses r) (dyn->list (ref-node-accesses r)) '()))])
                    (memv (acc-parm-index a) (list UNKNOWN-PARM GLOBAL-PARM)))))))))

(define (dump-records t)
  (cond
    [(mtree-every-base? t) (msg "    Every base")]
    [else
     (for ([b (in-list (if (mtree-bases t) (dyn->list (mtree-bases t)) '()))] [i (in-naturals)])
       (msg (format "      Base ~a: alias set ~a" i (base-node-base b)))
       (cond
         [(base-node-every-ref? b) (msg "      Every ref")]
         [else
          (for ([r (in-list (if (base-node-refs b) (dyn->list (base-node-refs b)) '()))] [j (in-naturals)])
            (msg (format "        Ref ~a: alias set ~a" j (ref-node-ref r)))
            (cond
              [(ref-node-every-access? r) (msg "          Every access")]
              [else
               (for ([a (in-list (if (ref-node-accesses r) (dyn->list (ref-node-accesses r)) '()))])
                 (msg (string-append "          access:" (acc->string a))))]))]))]))

;; ------------------------------------------------------------
;; modref_summary
;; ------------------------------------------------------------

(struct msum (loads stores kills arg-flags retslot-flags static-chain-flags writes-errno side-effects nondeterministic
                    calls-interposable global-read global-written try-dse
                    ;; IPA propagation (modref_propagate_in_scc) collapses the
                    ;; summary for a call left to an unavailable callee:
                    ;; #f, 'loads (the callee's stores are ignored) or 'all
                    ipa-collapse)
  #:mutable)
(define (make-msum) (msum (make-mtree) (make-mtree) (make-dyn) #f 0 0 #f #f #f #f #f #f #f #f))

(define (eaf-flags-useful? flags ecf)
  (and flags (for/or ([f (in-vector flags)]) (not (zero? (remove-useless-eaf f ecf #f))))))

(define (sum-useful? s ecf check-flags? major)
  (let/ec k
    (when (and (msum-arg-flags s) (not check-flags?)) (k #t))
    (when (and check-flags? (eaf-flags-useful? (msum-arg-flags s) ecf)) (k #t))
    (set-msum-arg-flags! s #f)
    (when (and check-flags? (not (zero? (remove-useless-eaf (msum-retslot-flags s) ecf #f)))) (k #t))
    (when (and check-flags? (not (zero? (remove-useless-eaf (msum-static-chain-flags s) ecf #f)))) (k #t))
    (define (looping-ok?)
      (and (if (>= major 15)
               (not (msum-side-effects s))
               (or (not (msum-side-effects s)) (not (msum-nondeterministic s))))
           (f? ecf ECF-LOOPING)))
    (when (f? ecf ECF-CONST) (k (looping-ok?)))
    (if (and (msum-loads s) (not (mtree-every-base? (msum-loads s))))
        (k #t)
        (dyn-clear! (msum-kills s)))
    (when (f? ecf ECF-PURE) (k (looping-ok?)))
    (and (msum-stores s) (not (mtree-every-base? (msum-stores s))))))

(define (sum-finalize! s)
  (set-msum-global-read! s (or (not (msum-loads s)) (tree-global-access? (msum-loads s))))
  (set-msum-global-written! s (or (not (msum-stores s)) (tree-global-access? (msum-stores s))))
  (cond
    [(or (msum-side-effects s) (msum-global-written s) (msum-writes-errno s)) (set-msum-try-dse! s #f)]
    [else
     (set-msum-try-dse! s #t)
     (define num-tests 0)
     (let/ec k
       (for ([b (in-list (if (mtree-bases (msum-stores s)) (dyn->list (mtree-bases (msum-stores s))) '()))])
         (when (base-node-every-ref? b) (set-msum-try-dse! s #f) (k (void)))
         (for ([r (in-list (if (base-node-refs b) (dyn->list (base-node-refs b)) '()))])
           (for ([a (in-list (if (ref-node-accesses r) (dyn->list (ref-node-accesses r)) '()))])
             (define n num-tests)
             (set! num-tests (add1 num-tests))
             (when (or (> n param-modref-max-tests) (not (acc-parm-offset-known? a)))
               (set-msum-try-dse! s #f) (k (void)))))))]))

(define (dump-summary s)
  (when (msum-loads s) (msg "  loads:") (dump-records (msum-loads s)))
  (when (msum-stores s) (msg "  stores:") (dump-records (msum-stores s)))
  (when (positive? (dyn-length (msum-kills s)))
    (msg "  kills:")
    (for ([a (in-list (dyn->list (msum-kills s)))]) (msg (string-append "    " (acc->string a)))))
  (when (msum-writes-errno s) (msg "  Writes errno"))
  (when (msum-side-effects s) (msg "  Side effects"))
  (when (msum-nondeterministic s) (msg "  Nondeterministic"))
  (when (msum-calls-interposable s) (msg "  Calls interposable"))
  (when (msum-global-read s) (msg "  Global memory read"))
  (when (msum-global-written s) (msg "  Global memory written"))
  (when (msum-try-dse s) (msg "  Try dse"))
  (when (msum-arg-flags s)
    (for ([fl (in-vector (msum-arg-flags s))] [i (in-naturals)] #:unless (zero? fl))
      (msg (string-append (format "  parm ~a flags:" i) (eaf-flags-string fl)))))
  (unless (zero? (msum-retslot-flags s)) (msg (string-append "  Retslot flags:" (eaf-flags-string (msum-retslot-flags s)))))
  (unless (zero? (msum-static-chain-flags s)) (msg (string-append "  Static chain flags:" (eaf-flags-string (msum-static-chain-flags s))))))

;; the summary after IPA propagation: a call left to an unavailable
;; callee collapses the loads (and the stores unless that callee's stores
;; are ignored) to every base; the global memory flags follow
(define (ipa-propagated s)
  (case (msum-ipa-collapse s)
    [(#f) s]
    [else
     (define t (struct-copy msum s))
     (set-msum-loads! t (let ([x (make-mtree)]) (set-mtree-every-base?! x #t) x))
     (when (eq? (msum-ipa-collapse s) 'all)
       (set-msum-stores! t (let ([x (make-mtree)]) (set-mtree-every-base?! x #t) x)))
     (sum-finalize! t)
     t]))

;; the callee's summary (get_modref_function_summary)
(define (callee-summary st d) (and d (decl-flag st d 'modref #f)))

;; ------------------------------------------------------------
;; the pass
;; ------------------------------------------------------------

(define (modref st fndecl cfg0)
  (define st0 (gs-set-fndecl st fndecl))
  (define cfg (cfg-clear-messages cfg0))
  (define m (gcfg->mfn cfg))
  (define major (gcc-major-version st0))
  (define msgs '())
  (define pending "")                       ; dump text without a newline, prefixed to the next line
  (define (msg! fmt . args)
    (set! msgs (cons (string-append pending (apply format fmt args)) msgs))
    (set! pending ""))
  (define (msg-part! s) (set! pending (string-append pending s)))
  (define ecf-flags (decl-ecf-flags st0 fndecl))
  (define returns-void? (void-type-p st0 (type-type st0 (tree-type fndecl))))
  (define params (pvector->list (decl-arguments fndecl)))
  (define default-defs (decl-flag st0 fndecl 'default-defs '()))
  (define (default-def-of parm) (let ([p (assv (decl-uid parm) default-defs)]) (and p (cdr p))))
  (define summary (make-msum))

  (msg! "modref analyzing '~a' (ipa=0)~a~a" (dump-name st0 fndecl)
        (if (decl-flag st0 fndecl 'readonly #f) " (const)" "")
        (if (decl-flag st0 fndecl 'pure #f) " (pure)" ""))
  ;; a summary kept from an earlier run (the late instance): the IPA
  ;; pass's summary, i.e. this function's early summary as propagated
  (define past (let ([s (callee-summary st0 fndecl)]) (and s (msum-loads s) (ipa-propagated s))))
  (when past
    (msg! "Past summary:")
    (parameterize ([current-msg! (lambda (s) (msg! "~a" s))]) (dump-summary past)))

  ;; ---------------- helpers on calls
  (define (ignore-nondeterminism? flags)
    (or (f? (if (>= major 15) (bitwise-ior flags ecf-flags) flags) (bitwise-ior ECF-CONST ECF-PURE))
        (f? flags ECF-NORETURN)))                       ; !flag_exceptions
  (define (ignore-retval? flags) (f? flags ECF-NORETURN))
  (define (ignore-stores? flags) (or (f? flags (bitwise-ior ECF-PURE ECF-CONST)) (f? flags ECF-NORETURN)))
  (define (call-arg-flags g i)
    (define s (callee-summary st0 (call-fndecl g)))
    (if (and s (msum-arg-flags s) (< i (vector-length (msum-arg-flags s)))) (vector-ref (msum-arg-flags s) i) 0))
  (define (call-return-flags g) (if (f? (call-ecf st0 g) ECF-MALLOC) ERF-NOALIAS 0))
  (define (recursive-call? g) (let ([d (call-fndecl g)]) (and d (decl-eq? d fndecl))))

  ;; memory_access_to
  (define (memory-access-to? op name)
    (define b (and op (get-base-address* op)))
    (and (Tree? b) (memq (tree-code b) '(mem_ref target_mem_ref))
         (ssa-name? (tree-operand b 0)) (ssa-name=? (tree-operand b 0) name)))
  (define (assign-load? g)
    (and (gassign? g) (gassign-single-p st0 g)
         (let* ([rhs (gassign-rhs1 g)] [rhs (if (handled-component-p rhs) (tree-operand rhs 0) rhs)])
           (and (or (eq? (tree-code rhs) 'with_size_expr) (handled-component-p rhs) (decl-p st0 rhs)
                    (memq (tree-code rhs) '(mem_ref target_mem_ref)))
                #t))))
  (define (store? g) (let ([lhs (stmt-get-lhs g)]) (and lhs (not (is-gimple-reg st0 lhs)))))

  ;; ---------------- EAF analysis
  (struct lat ([flags #:mutable] [known #:mutable] [open #:mutable] [do-dataflow #:mutable] [changed #:mutable] [propagate-to #:mutable]))
  (define lattice (make-vector (add1 (GS-ssa-version st0)) #f))
  (define (lat-of v) (or (vector-ref lattice v) (let ([l (lat 0 #f #f #f #f '())]) (vector-set! lattice v l) l)))
  (define depth 0)
  (define deferred-names '())
  (define names-to-propagate '())           ; in push order (reversed list)
  (define (indent n) (make-string n #\space))
  (define (lat-init! l) (set-lat-flags! l EAF-ALL) (set-lat-open! l #t) (set-lat-known! l #f))
  (define (lat-merge! l f)
    (cond
      [(f? f EAF-UNUSED) #f]
      [(not (= (bitwise-and (lat-flags l) f) (lat-flags l)))
       (set-lat-flags! l (remove-useless-eaf (bitwise-and (lat-flags l) f) 0 #f))
       #t]
      [else #f]))
  (define (lat-merge-lat! l with)
    (unless (lat-known with) (set-lat-do-dataflow! l #t))
    (lat-merge! l (lat-flags with)))
  (define (deref-flags flags ignore-stores?)
    (define ret (bitwise-ior EAF-NDC EAF-NDE EAF-NRD))
    (cond
      [(f? flags EAF-UNUSED) (bitwise-ior ret EAF-NIR EAF-NIC EAF-NIE)]
      [else
       (let* ([ret (if (or (and (f? flags EAF-NDC) (f? flags EAF-NIC)) ignore-stores?) (bitwise-ior ret EAF-NIC) ret)]
              [ret (if (or (and (f? flags EAF-NDE) (f? flags EAF-NIE)) ignore-stores?) (bitwise-ior ret EAF-NIE) ret)]
              [ret (if (and (f? flags EAF-NDR) (f? flags EAF-NIR)) (bitwise-ior ret EAF-NIR) ret)]
              [ret (if (and (f? flags EAF-NRD) (f? flags EAF-NRI)) (bitwise-ior ret EAF-NRI) ret)])
         ret)]))
  (define (lat-merge-deref! l with ignore-stores?)
    (unless (lat-known with) (set-lat-do-dataflow! l #t))
    (lat-merge! l (deref-flags (lat-flags with) ignore-stores?)))
  (define (lat-merge-direct-load! l) (lat-merge! l (fnot (bitwise-ior EAF-UNUSED EAF-NDR))))
  (define (lat-merge-direct-store! l) (lat-merge! l (fnot (bitwise-ior EAF-UNUSED EAF-NDC))))
  (define (callee-to-caller-flags cf ignore-stores? l)
    (define cf1 (bitwise-ior cf EAF-NRD EAF-NRI))
    (cond
      [(and (not ignore-stores?) (not (f? cf1 EAF-UNUSED)))
       (cond
         [(not (f? cf1 EAF-NDE)) (lat-merge! l 0)]
         [(not (f? cf1 EAF-NIE)) (lat-merge! l (fnot (bitwise-ior EAF-NRI EAF-NDR EAF-NIR EAF-NIC EAF-UNUSED)))])
       cf1]
      [else (bitwise-ior cf1 ignore-stores-eaf)]))

  (define (merge-with-ssa-name! dest src deref?)
    (define index (ssa-name-version dest)) (define src-index (ssa-name-version src))
    (unless (and (not deref?) (= index src-index))
      (set! depth (add1 depth))
      (analyze-ssa-name! src #f)
      (set! depth (sub1 depth))
      (define l (lat-of index)) (define sl (lat-of src-index))
      (if deref? (lat-merge-deref! l sl #f) (lat-merge-lat! l sl))
      (unless (lat-known sl)
        (when (null? (lat-propagate-to sl)) (set! names-to-propagate (cons src-index names-to-propagate)))
        (set-lat-propagate-to! sl (append (lat-propagate-to sl) (list (cons index deref?))))
        (set-lat-changed! sl #t)
        (set-lat-do-dataflow! sl #t)
        (msg! "~aWill propgate from ssa_name ~a to ~a~a" (indent (+ (* depth 4) 4)) src-index index (if deref? " (deref)" "")))))

  (define (merge-call-lhs-flags! g arg name direct? indirect?)
    (define index (ssa-name-version name))
    (define lhs (gcall-lhs g))
    (let/ec k
      (unless lhs (k (void)))
      (define returned-directly? #f)
      (when (>= arg 0)
        (define rf (call-return-flags g))
        (when (f? rf ERF-RETURNS-ARG)
          (if (= (bitwise-and rf ERF-RETURN-ARG-MASK) arg) (set! returned-directly? #t) (k (void)))))
      (define-values (d i)
        (cond [returned-directly? (values #t #f)]
              [(and (not direct?) (not indirect?)) (k (void))]
              [else (values direct? indirect?)]))
      (cond
        [(ssa-name? lhs)
         (when d (merge-with-ssa-name! name lhs #f))
         (when i (merge-with-ssa-name! name lhs #t))]
        [(not d) (lat-merge! (lat-of index) (deref-flags 0 #f))]
        [else (lat-merge! (lat-of index) 0)])))

  (define (analyze-ssa-name! name deferred?)
    (define index (ssa-name-version name))
    (define l (lat-of index))
    (let/ec k
      (unless deferred?
        (when (or (lat-known l) (lat-do-dataflow l)) (k (void)))
        (when (lat-open l) (msg! "~aCycle in SSA graph" (indent (* depth 4))) (k (void)))
        (lat-init! l)
        (when (= depth param-modref-max-depth)
          (msg! "~aMax recursion depth reached; postponing" (indent (* depth 4)))
          (set! deferred-names (cons name deferred-names))
          (k (void))))
      (msg! "~aAnalyzing flags of ssa name: ~a" (indent (* depth 4)) (print-expr st0 name))
      (for ([uid (in-list (mfn-imm-uses m name))])
        #:break (zero? (lat-flags l))
        (define g (mfn-stmt m uid))
        (msg! "~a  Analyzing stmt: ~a" (indent (* depth 4)) (stmt->string st0 cfg (mfn-bb m uid) g))
        (lat-merge! l (fnot EAF-UNUSED))
        (cond
          [(greturn? g)
           (define rv (greturn-retval g))
           (cond
             [(and rv (ssa-name? rv) (ssa-name=? rv name)) (lat-merge! l (fnot (bitwise-ior EAF-UNUSED EAF-NRD)))]
             [(memory-access-to? rv name)
              (lat-merge-direct-load! l)
              (lat-merge! l (fnot (bitwise-ior EAF-UNUSED EAF-NRI)))])]
          [(gcall? g)
           (define callee (call-fndecl g))
           (when (and (ssa-name? (gcall-fn g)) (ssa-name=? (gcall-fn g) name))
             (lat-merge! l (fnot (bitwise-ior EAF-NDC EAF-UNUSED))))
           (cond
             [(and callee (recursive-call? g)) (lat-merge! l 0)]
             [else
              (define ecf (call-ecf st0 g))
              (define ign-st (ignore-stores? ecf))
              (define ign-rv (ignore-retval? ecf))
              (when (and (gcall-lhs g) (memory-access-to? (gcall-lhs g) name))
                (lat-merge-direct-store! l))
              (for ([arg (in-pvector (gcall-args g))] [i (in-naturals)])
                #:break (zero? (lat-flags l))
                (cond
                  [(and (ssa-name? arg) (ssa-name=? arg name))
                   (define cf (call-arg-flags g i))
                   (unless ign-rv
                     (merge-call-lhs-flags! g i name (not (f? cf (bitwise-ior EAF-NRD EAF-UNUSED))) (not (f? cf (bitwise-ior EAF-NRI EAF-UNUSED)))))
                   (unless (f? ecf ECF-CONST)
                     (lat-merge! l (callee-to-caller-flags cf ign-st l)))]
                  [(memory-access-to? arg name)
                   (define cf (deref-flags (call-arg-flags g i) ign-st))
                   (when (and (not ign-rv) (not (f? cf EAF-UNUSED))
                              (not (= (bitwise-and cf (bitwise-ior EAF-NRD EAF-NRI)) (bitwise-ior EAF-NRD EAF-NRI))))
                     (merge-call-lhs-flags! g i name #f #t))
                   (if (f? ecf ECF-CONST)
                       (lat-merge-direct-load! l)
                       (lat-merge! l (callee-to-caller-flags cf ign-st l)))]
                  [else (void)]))])]
          [(assign-load? g)
           (cond
             [(store? g)
              (when (memory-access-to? (gassign-rhs1 g) name) (lat-merge! l (deref-flags 0 #f)))
              (when (memory-access-to? (gassign-lhs g) name) (lat-merge-direct-store! l))]
             [(memory-access-to? (gassign-rhs1 g) name)
              (merge-with-ssa-name! name (gassign-lhs g) #t)])]
          [(store? g)
           (cond
             [(and (gassign? g) (ssa-name? (gassign-rhs1 g)) (ssa-name=? (gassign-rhs1 g) name))
              (msg! "~a  ssa name saved to memory" (indent (* depth 4)))
              (lat-merge! l 0)]
             [(and (gassign? g) (memory-access-to? (gassign-lhs g) name))
              (lat-merge-direct-store! l)]
             [(not (gassign? g))
              (msg! "~a  Unhandled store" (indent (* depth 4)))
              (lat-merge! l 0)])]
          [(gassign? g)
           (define code (gassign-code g))
           (unless (or (memq (tree-code-class st0 code) '(tcc_comparison))
                       (memq code '(truth_and_expr truth_andif_expr truth_or_expr truth_orif_expr truth_xor_expr truth_not_expr pointer_diff_expr))
                       (and (eq? code 'pointer_plus_expr) (not (and (ssa-name? (gassign-rhs1 g)) (ssa-name=? (gassign-rhs1 g) name)))))
             (merge-with-ssa-name! name (gassign-lhs g) #f))]
          [(gphi? g) (merge-with-ssa-name! name (gphi-result g) #f)]
          [(gcond? g) (void)]
          [else
           (msg! "~a  Unhandled stmt" (indent (* depth 4)))
           (lat-merge! l 0)])
        (msg! "~a  current flags of ~a~a" (indent (* depth 4)) (print-expr st0 name) (eaf-flags-string (lat-flags l))))
      (msg! "~aflags of ssa name ~a~a" (indent (* depth 4)) (print-expr st0 name) (eaf-flags-string (lat-flags l)))
      (set-lat-open! l #f)
      (unless (lat-do-dataflow l) (set-lat-known! l #t))))

  (define (eaf-propagate!)
    (let loop ()
      (unless (null? deferred-names)
        (define name (car deferred-names))
        (set! deferred-names (cdr deferred-names))
        (msg! "Analyzing deferred SSA name")
        (analyze-ssa-name! name #t)
        (loop)))
    (define names (reverse names-to-propagate))
    (unless (null? names)
      (msg! "Propagating EAF flags")
      ;; reverse postorder over the propagation graph
      (define n (length names))
      (define rpo (make-vector n #f))
      (define pos (sub1 n))
      (define stack '())                    ; list of mutable (vector name pos)
      (for ([index (in-list names)])
        (unless (lat-known (lat-of index))
          (set! stack (list (vector index 0)))
          (set-lat-known! (lat-of index) #t))
        (let wl ()
          (unless (null? stack)
            (define top (car stack))
            (define index1 (vector-ref top 0))
            (define edges (lat-propagate-to (lat-of index1)))
            (define found
              (let scan ()
                (cond
                  [(< (vector-ref top 1) (length edges))
                   (define index2 (car (list-ref edges (vector-ref top 1))))
                   (vector-set! top 1 (add1 (vector-ref top 1)))
                   (cond
                     [(and (not (lat-known (lat-of index2))) (pair? (lat-propagate-to (lat-of index2))))
                      (set! stack (cons (vector index2 0) stack))
                      (set-lat-known! (lat-of index2) #t)
                      #t]
                     [else (scan)])]
                  [else #f])))
            (when (and (not found) (= (vector-ref top 1) (length edges)))
              (vector-set! rpo pos index1)
              (set! pos (sub1 pos))
              (set! stack (cdr stack)))
            (wl))))
      (define iterations 0)
      (let iterate ([changed #t])
        (when changed
          (set! iterations (add1 iterations))
          (msg! " iteration ~a" iterations)
          (define any #f)
          (for ([index (in-vector rpo)] #:when index)
            (define l (lat-of index))
            (when (lat-changed l)
              (set-lat-changed! l #f)
              (msg! "  Visiting ssa name ~a" index)
              (for ([e (in-list (lat-propagate-to l))])
                (define target (car e)) (define deref? (cdr e))
                (msg! "   Propagating flags of ssa name ~a to ~a~a" index target (if deref? " (deref)" ""))
                (define tl (lat-of target))
                (set-lat-known! tl #t)
                (define ch (if deref? (lat-merge-deref! tl l #f) (lat-merge-lat! tl l)))
                (when ch
                  (msg! "   New lattice: ~a" (eaf-flags-string (lat-flags tl)))
                  (set! any #t)
                  (set-lat-changed! tl #t)))))
          (iterate any)))
      (msg! "EAF flags propagated in ~a iterations" iterations)))

  (define (analyze-parms!)
    (define count (length params))
    (unless (zero? count)
      (for ([p (in-list params)])
        (define name (default-def-of p))
        (when name (analyze-ssa-name! name #f)))
      (eaf-propagate!)
      (define (grow!) (unless (msum-arg-flags summary) (set-msum-arg-flags! summary (make-vector count 0))))
      (for ([p (in-list params)] [i (in-naturals)])
        (define name (default-def-of p))
        (cond
          [(or (not name) (null? (mfn-use-ids m name)))
           (when (is-gimple-reg st0 p)
             (grow!)
             (vector-set! (msum-arg-flags summary) i EAF-UNUSED))]
          [else
           (define flags0 (lat-flags (lat-of (ssa-name-version name))))
           (define flags (remove-useless-eaf flags0 ecf-flags returns-void?))
           (unless (zero? flags)
             (grow!)
             (vector-set! (msum-arg-flags summary) i flags))]))))

  ;; ---------------- access analysis
  (define recursive-calls '())
  (define always-executed #f)
  (define (set-side-effects!)
    (cond [(msum-side-effects summary) #f] [else (set-msum-side-effects! summary #t) #t]))
  (define (set-nondeterministic!)
    (cond [(msum-nondeterministic summary) #f]
          [else (set-msum-side-effects! summary #t) (set-msum-nondeterministic! summary #t) #t]))

  ;; parm_map_for_ptr: (vector index known? offset)
  (define (unadjusted-ptr-and-unit-offset op0)
    (let loop ([op op0] [offset 0] [known #t] [i 0])
      (cond
        [(>= i 8) (values op offset known)]
        [(eq? (tree-code op) 'addr_expr)
         (define-values (base extra) (get-addr-base-and-unit-offset st0 (tree-operand op 0)))
         (define-values (base* known* offset*)
           (cond
             [(not base)
              (define b (get-base-address* (tree-operand op 0)))
              (values b #f offset)]
             [else (values base known (+ offset extra))]))
         (cond
           [(not (and (Tree? base*) (eq? (tree-code base*) 'mem_ref))) (values op offset known)]
           [else (loop (tree-operand base* 0) (+ offset* (mem-ref-offset base*)) known* (add1 i))])]
        [(and (ssa-name? op) (not (ssa-default-def? op)))
         (define g (mfn-def-stmt m op))
         (cond
           [(and g (gassign? g) (gassign-single-p st0 g)) (loop (gassign-rhs1 g) offset known (add1 i))]
           [(and g (gassign? g) (eq? (gassign-code g) 'pointer_plus_expr))
            (define r2 (gassign-rhs2 g))
            (if (and (Tree? r2) (eq? (tree-code r2) 'integer_cst))
                (loop (gassign-rhs1 g) (+ offset (integer-cst-value r2)) known (add1 i))
                (loop (gassign-rhs1 g) offset #f (add1 i)))]
           [else (values op offset known)])]
        [else (values op offset known)])))
  (define (parm-map-for-ptr op0)
    (define-values (op offset known) (unadjusted-ptr-and-unit-offset op0))
    (cond
      [(and (ssa-name? op) (ssa-default-def? op) (ssa-name-var op) (eq? (tree-code (ssa-name-var op)) 'parm_decl))
       (define idx (for/first ([p (in-list params)] [i (in-naturals)] #:when (decl-eq? p (ssa-name-var op))) i))
       (vector (or idx UNKNOWN-PARM) known offset)]
      [(points-to-local-or-readonly? st0 m op) (vector LOCAL-PARM #f 0)]
      [(and (ssa-name? op) (let ([g (mfn-def-stmt m op)]) (and g (gcall? g) (f? (call-ecf st0 g) ECF-MALLOC)))) (vector LOCAL-PARM #f 0)]
      [else (vector UNKNOWN-PARM #f 0)]))

  (define (get-access r)
    (define base (ao-ref-base r))
    (define a (acc (ao-ref-offset r) (ao-ref-size r) (ao-ref-max-size r) 0 UNKNOWN-PARM #f 0))
    (when (and (Tree? base) (memq (tree-code base) '(mem_ref target_mem_ref)))
      (define pm (parm-map-for-ptr (tree-operand base 0)))
      (set-acc-parm-index! a (vector-ref pm 0))
      (when (and (not (= (acc-parm-index a) UNKNOWN-PARM)) (eq? (tree-code base) 'mem_ref))
        (set-acc-parm-offset-known?! a #t)
        (set-acc-parm-offset! a (mem-ref-offset base))
        (if (vector-ref pm 1)
            (set-acc-parm-offset! a (+ (acc-parm-offset a) (vector-ref pm 2)))
            (set-acc-parm-offset-known?! a #f))))
    a)

  (define (record-access! t r a)
    (msg! "   - Recording base_set=0 ref_set=0 ~a" (acc->string a))
    (tree-insert! t 0 0 a #f))
  (define (record-access-p? expr)
    (when (and (tree-this-volatile expr) (or (< major 15) (not (ignore-nondeterminism? 0))))
      (msg-part! " (volatile; marking nondeterministic) ")
      (set-nondeterministic!))
    (cond
      [(refs-local-or-readonly? st0 m expr) (msg! "   - Read-only or local, ignoring.") #f]
      [else #t]))
  (define (record-unknown-load!) (cond [(mtree-every-base? (msum-loads summary)) #f] [else (tree-collapse! (msum-loads summary)) #t]))
  (define (record-unknown-store!) (cond [(mtree-every-base? (msum-stores summary)) #f] [else (tree-collapse! (msum-stores summary)) #t]))
  (define (record-global-memory-load!)
    (and (not (mtree-every-base? (msum-loads summary)))
         (tree-insert! (msum-loads summary) 0 0 (acc 0 -1 -1 0 GLOBAL-PARM #f 0) #f)))
  (define (record-global-memory-store!)
    (and (not (mtree-every-base? (msum-stores summary)))
         (tree-insert! (msum-stores summary) 0 0 (acc 0 -1 -1 0 GLOBAL-PARM #f 0) #f)))

  (define (analyze-load! b op)
    (msg! " - Analyzing load: ~a" (print-expr st0 op))
    (when (record-access-p? op)
      (define r (make-ao-ref st0 op))
      (define a (get-access r))
      (unless (= (acc-parm-index a) LOCAL-PARM)
        (record-access! (msum-loads summary) r a))))
  (define (analyze-store! b op)
    (msg! " - Analyzing store: ~a" (print-expr st0 op))
    (when (record-access-p? op)
      (define r (make-ao-ref st0 op))
      (define a (get-access r))
      (unless (= (acc-parm-index a) LOCAL-PARM)
        (record-access! (msum-stores summary) r a)
        (when (and always-executed (acc-useful-for-kill? a))
          (msg! "   - Recording kill")
          (insert-kill! (msum-kills summary) (copy-acc a) #f)))))

  (define (verify-arg arg flags load?)
    (cond
      [(f? flags EAF-UNUSED) #t]
      [(and load? (f? flags EAF-NDR)) #t]
      [(and (not load?) (f? flags EAF-NDC) (f? flags EAF-NIC)) #t]
      [(is-gimple-constant arg) #t]
      [(and (decl-p st0 arg) (decl-flag st0 arg 'readonly #f)) #t]
      [(eq? (tree-code arg) 'addr_expr)
       (define t (get-base-address* (tree-operand arg 0)))
       (or (is-gimple-constant t)
           (and (Tree? t) (decl-p st0 t) (or (decl-flag st0 t 'readonly #f) (eq? (tree-code t) 'function_decl))))]
      [else #f]))
  (define (may-access-nonescaping-parm? g callee-ecf load?)
    (define implicit
      (bitwise-ior (if (ignore-stores? callee-ecf) ignore-stores-eaf 0)
                   (if (f? callee-ecf ECF-PURE) implicit-pure-eaf 0)
                   (if (f? callee-ecf ECF-CONST) implicit-const-eaf 0)))
    (for/or ([arg (in-pvector (gcall-args g))] [i (in-naturals)])
      (not (verify-arg arg (bitwise-ior (call-arg-flags g i) implicit) load?))))

  (define (process-fnspec! g)
    (define flags (call-ecf st0 g))
    (when (or (not (f? flags (bitwise-ior ECF-CONST ECF-PURE))) (f? flags ECF-LOOPING))
      (set-side-effects!)
      (unless (ignore-nondeterminism? flags) (set-nondeterministic!)))
    (unless (f? flags ECF-CONST)
      ;; no fnspec known
      (when (call-fndecl-builtin? (call-fndecl g))
        (msg! "      Builtin with no fnspec: ~a" (decl-print-name st0 (call-fndecl g))))
      (cond
        [(not (ignore-stores? flags))
         (if (not (may-access-nonescaping-parm? g flags #f)) (record-global-memory-store!) (record-unknown-store!))
         (if (not (may-access-nonescaping-parm? g flags #t)) (record-global-memory-load!) (record-unknown-load!))]
        [else
         (if (not (may-access-nonescaping-parm? g flags #t)) (record-global-memory-load!) (record-unknown-load!))])))

  (define (merge-call-side-effects! g callee-sum callee record?)
    (define flags (call-ecf st0 g))
    (let/ec k
      (when (and (f? flags ECF-CONST) (not (f? flags ECF-LOOPING))) (k #f))
      (define changed #f)
      (msg! " - Merging side effects of ~a" (dump-name st0 callee))
      (when (or (not (f? flags (bitwise-ior ECF-CONST ECF-PURE))) (f? flags ECF-LOOPING))
        (when (and (not (msum-side-effects summary)) (msum-side-effects callee-sum))
          (msg! " - merging side effects.") (set-msum-side-effects! summary #t) (set! changed #t))
        (when (and (not (msum-nondeterministic summary)) (msum-nondeterministic callee-sum) (not (ignore-nondeterminism? flags)))
          (msg! " - merging nondeterministic.") (set-msum-nondeterministic! summary #t) (set! changed #t)))
      (when (f? flags ECF-CONST) (k changed))
      (when (and (not (msum-calls-interposable summary)) (msum-calls-interposable callee-sum))
        (msg! " - merging calls interposable.") (set-msum-calls-interposable! summary #t) (set! changed #t))
      (when (and (not (binds-to-current-def? st0 callee)) (not (msum-calls-interposable summary)))
        (msg! " - May be interposed.") (set-msum-calls-interposable! summary #t) (set! changed #t))
      (define parm-map (for/vector ([arg (in-pvector (gcall-args g))]) (parm-map-for-ptr arg)))
      (msg! "   Parm map:~a"
            (apply string-append
                   (for/list ([pm (in-vector parm-map)])
                     (string-append (format " ~a" (vector-ref pm 0)) (if (vector-ref pm 1) (format " offset:~a" (vector-ref pm 2)) "")))))
      (define chain-map (vector UNKNOWN-PARM #f 0))
      (when (and always-executed (positive? (dyn-length (msum-kills callee-sum))))
        (for ([kill (in-list (dyn->list (dyn-copy (msum-kills callee-sum))))])
          (when (< (acc-parm-index kill) (vector-length parm-map))
            (define pm (if (= (acc-parm-index kill) STATIC-CHAIN-PARM) chain-map (vector-ref parm-map (acc-parm-index kill))))
            (unless (or (memv (vector-ref pm 0) (list LOCAL-PARM UNKNOWN-PARM RETSLOT-PARM)) (not (vector-ref pm 1)))
              (define n (copy-acc kill))
              (set-acc-parm-index! n (vector-ref pm 0))
              (set-acc-parm-offset! n (+ (acc-parm-offset n) (vector-ref pm 2)))
              (when (insert-kill! (msum-kills summary) n record?) (set! changed #t))))))
      (when (tree-merge! (msum-loads summary) (msum-loads callee-sum) parm-map chain-map record? (not (may-access-nonescaping-parm? g flags #t)))
        (set! changed #t))
      (unless (ignore-stores? flags)
        (when (tree-merge! (msum-stores summary) (msum-stores callee-sum) parm-map chain-map record? (not (may-access-nonescaping-parm? g flags #f)))
          (set! changed #t))
        (when (and (not (msum-writes-errno summary)) (msum-writes-errno callee-sum))
          (set-msum-writes-errno! summary #t) (set! changed #t)))
      changed))

  (define (analyze-call! g bb)
    (define flags (call-ecf st0 g))
    (msg! " - Analyzing call:~a" (stmt->string st0 cfg bb g))
    (let/ec k
      (when (and (f? flags ECF-CONST) (not (f? flags ECF-LOOPING)))
        (msg! " - ECF_CONST, ignoring all stores and all loads except for args.")
        (k (void)))
      (define callee (call-fndecl g))
      (unless callee
        (msg! (if (gcall-internal? g) " - Internal call" " - Indirect call."))
        (process-fnspec! g)
        (k (void)))
      (when (recursive-call? g)
        (set! recursive-calls (append recursive-calls (list (cons g bb))))
        (set-side-effects!)
        (msg! " - Skipping recursive call.")
        (k (void)))
      (unless (function-available? st0 callee)
        (msg! " - Function availability <= AVAIL_INTERPOSABLE.")
        (set-msum-ipa-collapse! summary (if (or (eq? (msum-ipa-collapse summary) 'all) (not (ignore-stores? flags))) 'all 'loads))
        (process-fnspec! g)
        (k (void)))
      (define cs (callee-summary st0 callee))
      (unless cs
        (msg! " - No modref summary available for callee.")
        (process-fnspec! g)
        (k (void)))
      (merge-call-side-effects! g cs callee #f)))

  (define (null-deref? g)
    (define found #f)
    (walk-stmt-loads-stores st0 g
                            (lambda (b t) (when (and (memq (tree-code b) '(mem_ref target_mem_ref)) (integer-zerop (tree-operand b 0))) (set! found #t)))
                            (lambda (b t) (when (and (memq (tree-code b) '(mem_ref target_mem_ref)) (integer-zerop (tree-operand b 0))) (set! found #t))))
    found)

  (define (analyze-stmt! g bb)
    (walk-stmt-loads-stores st0 g analyze-load! analyze-store!)
    (when (gcall? g) (analyze-call! g bb)))

  ;; find_always_executed_bbs (assume_return_or_eh = true)
  (define (always-executed-bbs)
    (define visited (make-hasheqv))
    (let walk ([stack (list ENTRY)])
      (unless (null? stack)
        (define bb (car stack))
        (cond
          [(= bb EXIT) (walk (cdr stack))]
          [else
           (define new
             (for/fold ([acc '()]) ([e (in-pvector (cfg-succs cfg bb))])
               (define d (GEdge-dest e))
               (cond [(hash-ref visited d #f) acc]
                     [else (hash-set! visited d #t) (append acc (list d))])))
           ;; stack.safe_push in edge order: the last pushed is popped first
           (walk (append (reverse new) (cdr stack)))])))
    (cond
      [(not (hash-ref visited EXIT #f)) (list (GEdge-dest (pvector-first (cfg-succs cfg ENTRY))))]
      [else
       (define state (make-hasheqv))        ; bb -> (vector pre post low high)
       (define next-dfs 1)
       (define post-list '())               ; postorder, reversed
       (let wl ([worklist (list (mcons EXIT #f))])
         (unless (null? worklist)
           (define w (car worklist))
           (define bb (mcar w))
           (cond
             [(not (mcdr w))
              (cond
                [(hash-ref state bb #f) (wl (cdr worklist))]
                [else
                 (define cs (vector next-dfs 0 next-dfs next-dfs))
                 (set! next-dfs (add1 next-dfs))
                 (hash-set! state bb cs)
                 (set-mcdr! w cs)
                 (define pushes
                   (for/fold ([acc '()]) ([src (in-pvector (cfg-preds cfg bb))])
                     (if (hash-ref visited src #f) (cons (mcons src #f) acc) acc)))
                 (wl (append pushes worklist))])]
             [else
              (define cs (mcdr w))
              (vector-set! cs 1 next-dfs) (vector-set! cs 3 next-dfs)
              (set! next-dfs (add1 next-dfs))
              (set! post-list (cons bb post-list))
              (wl (cdr worklist))])))
       (define ret '())
       (for ([bb (in-list (reverse post-list))])
         (define cs (hash-ref state bb))
         (for ([src (in-pvector (cfg-preds cfg bb))])
           (define c2 (hash-ref state src #f))
           (cond
             [(not c2) (unless (= src ENTRY) (vector-set! cs 2 0))]
             [else (vector-set! cs 2 (min (vector-ref cs 2) (vector-ref c2 2)))
                   (vector-set! cs 3 (max (vector-ref cs 3) (vector-ref c2 3)))]))
         (when (and (= (vector-ref cs 2) (vector-ref cs 0)) (= (vector-ref cs 3) (vector-ref cs 1)) (not (= bb EXIT)))
           (set! ret (cons bb ret)))
         (for ([e (in-pvector (cfg-succs cfg bb))])
           (define c2 (hash-ref state (GEdge-dest e) #f))
           (when c2
             (vector-set! cs 2 (min (vector-ref cs 2) (vector-ref c2 2)))
             (vector-set! cs 3 (max (vector-ref cs 3) (vector-ref c2 3))))))
       (sort ret <)]))

  (define (access-analyze!)
    (define ae
      (cond
        [(>= major 13)
         (define bbs (always-executed-bbs))
         (msg! "Always executed bbbs (assuming return or EH): ~a" (string-join (map number->string bbs) ", "))
         bbs]
        [else (list (GEdge-dest (pvector-first (cfg-succs cfg ENTRY))))]))
    (define summary-useful
      (let/ec k
        (for ([bb (in-pvector (cfg-bbs cfg))])
          (set! always-executed (and (memv bb ae) #t))
          (for ([id (in-list (mfn-stmts m bb))])
            (define g (mfn-stmt m id))
            (unless (glabel? g)
              (when (null-deref? g)
                (msg! " - NULL memory access; terminating BB")
                (k #t))                    ; break out of this block only
              (analyze-stmt! g bb)
              (unless (sum-useful? summary ecf-flags #f major) (k #f)))))
        #t))
    (define st1 st0)
    (when summary-useful
      ;; propagate over recursive calls
      (set! always-executed #f)
      (let loop ([changed #t] [first #t])
        (when (and changed (sum-useful? summary ecf-flags #f major))
          (define ch
            (for/fold ([ch #f]) ([rc (in-list recursive-calls)])
              (or (merge-call-side-effects! (car rc) summary fndecl (not first)) ch)))
          (loop ch #f)))
      (when (not (msum-side-effects summary))
        (define-values (finite? st*) (finite-function? st0 m cfg fndecl msg!))
        (set! st1 st*)
        (unless finite? (set-msum-side-effects! summary #t))))
    st1)

  ;; NB: the NULL-deref break above leaves the per-block loop; GCC breaks only the block
  (parameterize ([current-msg! (lambda (s) (msg! "~a" s))])
    (analyze-parms!)
    (define st1 (access-analyze!))
    ;; ipa_make_function_const / pure
    (define-values (fixup? st2)
      (cond
        [(and (not (mtree-every-base? (msum-stores summary))) (not (mtree-bases (msum-stores summary))) (not (msum-nondeterministic summary)))
         (if (and (not (mtree-every-base? (msum-loads summary))) (not (mtree-bases (msum-loads summary))) (not (msum-calls-interposable summary)))
             (ipa-make-function-const st1 fndecl (msum-side-effects summary) msg!)
             (ipa-make-function-pure st1 fndecl (msum-side-effects summary) msg!))]
        [else (values #f st1)]))
    (define ecf2 (decl-ecf-flags st2 fndecl))
    (define keep? (sum-useful? summary ecf2 #t major))
    (when keep? (sum-finalize! summary))
    (msg! " - modref done with result: tracked.")
    (when keep? (dump-summary summary))
    ;; compare the IPA and local solutions
    (when past
      (define old-flags (or (msum-arg-flags past) (vector)))
      (cond
        [keep?
         (define new-flags (or (msum-arg-flags summary) (vector)))
         (for ([i (in-range (max (vector-length old-flags) (vector-length new-flags)))])
           (define o (remove-useless-eaf (if (< i (vector-length old-flags)) (vector-ref old-flags i) 0) ecf2 returns-void?))
           (define n (if (< i (vector-length new-flags)) (vector-ref new-flags i) 0))
           (unless (= o n)
             (msg! "  Flags for param ~a ~a:~a -> ~a" i
                   (if (or (zero? (bitwise-and o (bitwise-not n))) (f? n EAF-UNUSED)) "improved" "changed")
                   (eaf-flags-string o) (eaf-flags-string n))))]
        [else
         (for ([i (in-range (vector-length old-flags))])
           (define o (remove-useless-eaf (vector-ref old-flags i) ecf2 returns-void?))
           (unless (zero? o) (msg! "  Flags for param ~a worsened:~a -> " i (eaf-flags-string o))))]))
    (define st3 (if keep? (set-decl-flag st2 fndecl 'modref summary) st2))
    (define cfg1 (for/fold ([c cfg]) ([mm (in-list (reverse msgs))]) (cfg-add-message c mm)))
    (if fixup? (fixup-cfg st3 cfg1) (values cfg1 st3))))
