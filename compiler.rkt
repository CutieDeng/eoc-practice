#lang racket

(require racket/set)
(require racket/fixnum)
(require "interp.rkt")
(require "utilities.rkt")

(require "interp-Lvec-prime.rkt") (require "type-check-Lvec.rkt")
(require "interp-Cvec.rkt")
(require "type-check-Cvec.rkt")

(require "compiler/graph-core.rkt")
(require "graph-printing.rkt")
(require "priority_queue.rkt")

(require data/queue)

(require racket/pretty)

(require "compiler/x86abi.rkt")

(define pass-abstract
  (class object%
    (super-new)
    (abstract pass)
  ))

(require "compiler/shrink.rkt")
(define shrink (λ (p) (send (new pass-shrink) pass p)))

(require "compiler/uniquify.rkt")
(define uniquify (λ (p) (send (new pass-uniquify) pass p)))

(require "compiler/rco.rkt")
(define remove-complex-opera* (λ (p) (send (new pass-rco) pass p)))

(require "compiler/explicate-control.rkt")
(define explicate-control (λ (p) (send (new pass-explicate-control) pass p)))

(require "compiler/select-instructions.rkt")
(define select-instructions (λ (p) (send (new pass-select-instructions) pass p)))

(define (pass-read-write-mixin super-class)
  (class super-class
    (super-new)
    (define/public (get-write instr) (match instr
      [(Instr 'movq (list _ dst)) (set dst)]
      [(or (Instr 'addq (list _ dst)) (Instr 'subq (list _ dst))) (set dst)]
      [(Callq _ _) (list->set (map Reg caller-save-regs))]
      [(Instr 'negq (list dst)) (set dst)]
      [(Instr 'cmpq (list _ _)) (set )]
      [(Jmp _) (set )]
      [(JmpIf _ _) (set )]
    ))
    (define/public (get-read instr)
      (define raw-set (get-read-with-imm instr))
      (define l (filter (λ (i) (not (or (Imm? i) (Bool? i)))) (set->list raw-set)))
      (list->set l)
    )
    (define/public (get-read-with-imm instr) (match instr
      [(Instr (or 'addq 'subq) (list src dst)) (set src dst)]
      [(Instr 'movq (list src _)) (set src)]
      [(Instr 'negq (list src)) (set src)]
      [(Callq _ count) 
        (list->set (map Reg (take pass-args-regs count)))]
      [(Jmp _) (set )]
      [(JmpIf _ _) (set )]
      [(Instr 'cmpq (list a b)) (set a b)]
    ))
  ))

(define (pass-default-set-to-read-mixin super-class)
  (class super-class
    (super-new)
    (define/public (default-set-to-read)
      (set (Reg 'rax))
    )
  ))

(define (interference-cmp-fn lhs rhs)
  (match* (lhs rhs)
    [((Var _) (Reg _)) 'lt]
    [((Reg _) (Var _)) 'gt]
    [((Var l) (Var r)) (symbol-compare l r)]
    [((Reg l) (Reg r)) (symbol-compare l r)]
  )
)

(define (pass-build-interference-mixin super-class)
  (class super-class
    (super-new) 
    (inherit get-read get-write)
    (define/override (pass p) (match p
      [(X86Program info blocks)
        (define init-graph (init-build-interference-from-blocks blocks (graph-make-empty-raw interference-cmp-fn)))
        (define init-graph-var (box init-graph))
        ; (add-vertex (add-vertex init-graph (Var 'ignore)) (Var 'ignore2))
        (define pass-block^ (pass-block init-graph-var))
        (define blocks^ (for/fold ([a (ordl-make-empty symbol-compare)]) ([(tag block-value) (in-dict blocks)]) 
          (define block^ (pass-block^ block-value))
          (ordl-insert a tag block^ #f)
        ))
        (X86Program (dict-set info 'interference (unbox init-graph-var)) blocks^)
      ]
    ))
    (define/public (init-build-interference-from-blocks blocks graph)
      (for ([(_ block) (in-dict blocks)])
        (set! graph (init-build-interference (Block-instr* block) graph))
      )
      graph
    )
    (define/public (init-build-interference b graph) (match b
      [(? ral-empty?) graph]
      [_ (define-values (instr instr*) (ral-dropl b))
        (for ([w (in-set (get-write instr))])
          ; (add-vertex! graph w)
          (set! graph (add-vertex graph w))
        )
        (set! graph (init-build-interference instr* graph))
        graph
      ]
    ))
    (define/public ((pass-block interference) block) (match block
      [(Block info instr*)
        (define live (dict-ref info 'live))
        (match-define-values (_ l) (ral-dropl live))
        (pass-instr* instr* l interference)
        block
      ]
    ))
    (define/public (pass-instr* instr* live graph) (match* (instr* live)
      [(_ (? ral-empty?)) graph]
      [(_ _)
        (define-values (instr rest) (ral-dropl instr*))
        (define-values (l live-rest) (ral-dropl live))
        (define writes (get-write instr))
        (define reads l)
        (for ([w (in-set writes)])
          (for ([r (in-set reads)])
            ; (add-edge! graph w r)
            (set-box! graph (add-edge (unbox graph) w r))
          )
        )
        (pass-instr* rest live-rest graph)
      ]
    ))
  ))

(define pass-color-graph
  (class pass-abstract
    (super-new)
    (define/override (pass p) (match p
      [(X86Program info blocks)
        (define interference (dict-ref info 'interference))
        (define interference-var (box interference))
        (pre-interference-handle interference-var)
        (define int-graph (build-int-color-graph (unbox interference-var) (match-lambda 
          [_ #t]
        )))
        (define vec-graph (build-vec-color-graph (unbox interference-var) (match-lambda
          [_ #f]
        )))
        (X86Program (dict-set (dict-set info 'color-graph int-graph) 'vec-color-graph vec-graph) blocks)
      ]
    ))
    (define/public (pre-interference-handle interference-graph)
      (define graph (unbox interference-graph))
      (for ([r (in-vertices graph)])
        ; (add-edge! interference-graph r (Reg 'r15))
        ; (add-edge! interference-graph r (Reg 'rsp))
        ; (add-edge! interference-graph r (Reg 'rbp))
        (set! graph (add-edge (add-vertex graph (Reg 'r15)) r (Reg 'r15)))
        (set! graph (add-edge (add-vertex graph (Reg 'rsp)) r (Reg 'rsp)))
        (set! graph (add-edge (add-vertex graph (Reg 'rbp)) r (Reg 'rbp)))
      )
      (set-box! interference-graph graph)
    )
    (define/public (build-int-color-graph interference-graph int-filter)
      (define q (make-pqueue (λ (a b) (>= (cdr a) (cdr b)))))
      (define (make-pqueue-raw) (make-pqueue >=))
      (for ([n (sequence-filter int-filter (in-vertices interference-graph))])
        (pqueue-push! q (cons n 0))
      )
      (define neighbors-set 
        (for/fold ([nei-set '()]) ([i (range (length caller-and-callee-regs))] [r caller-and-callee-regs])
          (with-handlers ([exn:fail? (λ (_e) nei-set)])
            (for/fold ([nei-set^ nei-set]) ([n (sequence-filter int-filter (in-neighbors interference-graph (Reg r)))])
              (define inner (dict-ref nei-set^ n '()))
              (define inner-2 (set-add inner i))
              (pqueue-push! q (cons n (length inner-2)))
              (dict-set nei-set^ n inner-2)
            ))
        )
      )
      (define selections (for/fold ([select '()]) ([i (range (length caller-and-callee-regs))] [r caller-and-callee-regs])
        (dict-set select (Reg r) i)
      ))
      (define color-graph-value (let color-find ([neighbors-set neighbors-set] [selections selections])
        (cond
          [(= (pqueue-count q) 0) selections]
          [else
            (define pop (pqueue-pop! q))
            (cond
              [(dict-has-key? selections (car pop)) (color-find neighbors-set selections)]
              [else
                (define color (let color-find-2 ([color 0])
                  (if (set-member? (dict-ref neighbors-set (car pop) '()) color)
                    (color-find-2 (+ color 1))
                    color
                    )))
                (define neighbors-set-2 (for/fold ([n neighbors-set]) ([v (sequence-filter int-filter (in-neighbors interference-graph (car pop)))])
                  (define inner (dict-ref n v '()))
                  (define inner-2 (set-add inner color))
                  (pqueue-push! q (cons v (length inner-2)))
                  (dict-set n v inner-2)
                ))
                (color-find neighbors-set-2 (dict-set selections (car pop) color))
              ]
            )
          ]
        )
      ))
      color-graph-value
    )
    (define/public (build-vec-color-graph interference-graph vertice-filter)
      (define pq (make-pqueue (λ (a b) (> (cdr a) (cdr b)))))
      (for ([v (sequence-filter vertice-filter (in-vertices interference-graph))])
        (pqueue-push! pq (cons v 0))
      )
      (define neighbors-set '())
      (let color-find ([neighbors-set neighbors-set] [selections '()]) (cond
        [(= (pqueue-count pq) 0) selections]
        [else
          (define p (pqueue-pop! pq))
          (cond
            [(dict-has-key? selections (car p)) (color-find neighbors-set selections)]
            [else 
              (define select-color (let select-color ([n 0]) 
                (cond
                  [(set-member? (dict-ref neighbors-set (car p) '()) n) (select-color (+ n 1))]
                  [else n]
                )))
              (define neighbors-set^ (for/fold ([neighbors-set neighbors-set]) ([v (sequence-filter vertice-filter (in-vertices interference-graph))])
                (define set^ (set-add (dict-ref neighbors-set v '()) select-color))
                (pqueue-push! pq (cons v (length set^)))
                (dict-set neighbors-set v set^)
              ))
              (color-find neighbors-set^ (dict-set selections (car p) select-color))
            ]
          )
        ]
      ))
    )
  ))

(define color-graph (λ (p) (send (new pass-color-graph) pass p)))

(require cutie-ftree)

(require "compiler/prelude-conclusion.rkt")
(define prelude-and-conclusion (λ (p) (send (new pass-prelude-and-conclusion) pass p)))

(require "compiler/connect-component.rkt")
(define connect-component (λ (p) (send (new pass-uncover-live) pass p)))

(define (pass-read-write-mixin2 super-class)
  (class super-class
    (super-new)
    (define/public (get-write-raw instr) (match instr
      [(Instr 'movq (list _ dst)) (set dst)]
      [(or (Instr 'addq (list _ dst)) (Instr 'subq (list _ dst))) (set dst)]
      [(Callq _ _) (list->set (map Reg caller-save-regs))]
      [(Instr 'negq (list dst)) (set dst)]
      [(Instr 'cmpq (list _ _)) (set )]
      [(Jmp _) (set )]
      [(JmpIf _ _) (set )]
    ))
    (define/public (get-write instr)
      (define s (get-write-raw instr))
      (list->set (filter (λ (i) (not (or (Global? i) (Deref? i)))) (set->list s)))
    )
    (define/public (get-read instr)
      (define raw-set (get-read-with-imm instr))
      (define l (filter (λ (i) (not (or (Imm? i) (Bool? i) (Global? i) (Deref? i)))) (set->list raw-set)))
      (list->set l)
    )
    (define/public (get-read-with-imm instr) (match instr
      [(Instr (or 'addq 'subq) (list src dst)) (set src dst)]
      [(Instr 'movq (list src _)) (set src)]
      [(Instr 'negq (list src)) (set src)]
      [(Callq _ count) 
        (list->set (map Reg (take pass-args-regs count)))]
      [(Jmp _) (set )]
      [(JmpIf _ _) (set )]
      [(Instr 'cmpq (list a b)) (set a b)]
    ))
  ))

(define pass-block-uncover-live-enhanced
  (class (pass-read-write-mixin2 object%)
    (super-new)
    (inherit get-read get-write)
    (define/public pass-block (match-lambda [(Block info instr*)
      (define a (mutable-set))
      (define d (mutable-set))
      (pass-instr* instr* a d) 
      (set! info (dict-set info 'live-change (cons a d)))
      (Block info instr*)
    ]))
    (define/public (pass-instr* instr* add-set drop-set) (match instr*
      [(? ral-empty?) (void)]
      [_ (=> eh)
        (define-values (instr rest) (ral-dropl instr*))
        (pass-instr* rest add-set drop-set)
        (define r (get-read instr))
        (define w (get-write instr))
        (set-union! drop-set w)
        (set-subtract! add-set w)
        (set-union! add-set r)
        (set-subtract! drop-set r)
      ]
    ))
    (define/public pass (match-lambda [(X86Program info blocks)
      (define blocks^ (for/fold ([a (ordl-make-empty symbol-compare)]) ([(tag block) (in-dict blocks)])
        (ordl-insert a tag (pass-block block) #f)
      ))
      (X86Program info blocks^)
    ]))
  )
)

(define block-uncover-live-enhanced (λ (p) (send (new pass-block-uncover-live-enhanced) pass p)))

(define subqueue (make-parameter #f))

(define (pass-uncover-live-mixin clz)
  (class clz
    (super-new)
    (inherit get-read get-write)
    (define/public (analyze-dataflow graph transfer bottom join id2block)
      (define s (subqueue))
      (define mapping (box (ordl-make-empty symbol-compare)))
      (define change-able (mutable-set))
      (define worklist (make-queue))
      (define graph-t (transpose graph))
      (for ([sij (in-vertices graph-t)]) (set-box! mapping (dict-set (unbox mapping) sij bottom)))
      (for ([si (in-ral0 s)])
        (set-clear! change-able)
        (for ([sij (in-dict-keys si)]) (set-add! change-able (dict-ref id2block sij)))
        (for ([sij (in-dict-keys si)]) (enqueue! worklist (dict-ref id2block sij)))
        (while (not (queue-empty? worklist))
          (define node (dequeue! worklist))
          (define preds (get-neighbors graph-t node))
          (define input
            (cond
              [(ral-empty? preds) (set (Reg 'rax))]
              [else
                (for/fold ([state bottom]) ([pred (in-ral0 preds)])
                  (join state (dict-ref (unbox mapping) pred))
                )
              ]
            )
            ; (match preds
            ;   ['() (set (Reg 'rax))]
            ;   [(cons _ _)
            ;     (for/fold ([state bottom]) ([pred (in-neighbors graph-t node)]) 
            ;       (join state (dict-ref (unbox mapping) pred)))
            ;   ]
            ; )
          )
          (define output (transfer node input))
          (cond [(not (equal? output (dict-ref (unbox mapping) node))) 
            (set-box! mapping (dict-set (unbox mapping) node output))
            (for ([s (in-neighbors graph node)])
              (when (set-member? change-able s)
                (enqueue! worklist s))
            )]))
      )
      (unbox mapping)
    )
    (define/public (pass-instr* instr* end) (match instr*
      [(? ral-empty?) (vector->ral (vector end))]
      [_ (=> eh)
        (define l (ral-viewl instr*))
        (match l [(or (JmpIf _ _) (Jmp _)) (vector->ral (vector end))] [_ (eh)])
      ]
      [_
        (define-values (instr rest) (ral-dropl instr*))
        (define instr-write (get-write instr))
        (define instr-read (get-read instr))
        (define rest-set (pass-instr* rest end))
        (define s-m (set-subtract (ral-viewl rest-set) instr-write))
        (define s (set-union s-m instr-read))
        (ral-consl rest-set s)
      ]
    ))
    (define/override (pass p) (match p [(X86Program info blocks)
      (define graph (dict-ref info 'graph))
      (define components (dict-ref info 'connect-component))
      (define id2block (dict-ref info 'id2block))
      (define live-map
        (parameterize ([subqueue components])
          (analyze-dataflow graph 
            (lambda (node input)
              (define block (dict-ref blocks node))
              (match-define (Block info _) block)
              (match-define (cons add drop) (dict-ref info 'live-change))
              (define input^ (set-subtract input drop))
              (define input^^ (set-union input^ add))
              input^^
            )
            (set)
            set-union
            id2block
          )
        )
      )
      (define blocks^ (for/fold ([a (ordl-make-empty symbol-compare)]) ([(b-tag b) (in-dict blocks)])
        (match-define (Block info instr*) b)
        (define tos (in-neighbors graph b-tag))
        (define end (for/fold ([end-set (set)]) ([to tos])
          (set-union (dict-ref live-map to) end-set)))
        (ordl-insert a b-tag (Block (dict-set info 'live (pass-instr* instr* end)) instr*) #f)
      ))
      (X86Program info blocks^)
    ]))
  )
)

(require "compiler/collect-set.rkt")
(define collect-set! (lambda (p) (send (new pass-collect-set!) pass p)))

(require "compiler/uncover-get.rkt")
(define uncover-get!-exp (λ (p) (send (new pass-uncover-get!-exp) pass p)))

(require "compiler/patch-instructions.rkt")
(define patch-instructions (λ (p) (send (new pass-patch-instructions) pass p)))

(require "compiler/expose-allocation.rkt")
(define expose-allocation (λ (p) (send (new pass-expose-allocation) pass p)))

(require "compiler/build-interference.rkt")
(define build-interference (λ (p) (send (new pass-build-interference) pass p)))

(define (pass-read-write-Cwhile-mixin super-class)
  (class (pass-read-write-mixin super-class)
    (super-new)
    (inherit get-read-with-imm)
    (define/override (get-write instr) 
      (define set^ (super get-write instr))
      (list->set (filter (λ (i) (not (or (Global? i) (Deref? i)))) (set->list set^)))
    )
    (define/override (get-read instr)
      (define set^ (get-read-with-imm instr))
      (define l (filter (λ (i) (not (or (Imm? i) (Bool? i) (Global? i) (Deref? i)))) (set->list set^)))
      (list->set l)
    )
  ))

(define uncover-live (λ (p) (send (new 
  (pass-uncover-live-mixin
    (pass-read-write-Cwhile-mixin
      (pass-default-set-to-read-mixin pass-abstract)))) pass p)))

(require "compiler/allocate-registers.rkt")
(define allocate-registers (λ (p) (send (new pass-allocate-registers) pass p)))

(define dominance-map (make-parameter #f))

(define dominance
  (class pass-abstract
    (super-new)
    (define/override pass (match-lambda [(and x (X86Program info blocks))
      (parameterize ([dominance-map (box (ordl-make-empty symbol-compare))])
        (define full-set (list->set (dict-keys blocks)))
        (for ([block full-set])
          (set-box! (dominance-map) (dict-set (unbox (dominance-map)) block full-set))
        )
        (define connect-component (dict-ref info 'connect-component))
        (set-box! (dominance-map) (dict-set (unbox (dominance-map)) 'start (set 'start)))
        (define graph (dict-ref info 'graph))
        (define id2block (dict-ref info 'id2block))
        (define graph-rev (transpose graph))
        (for ([component (in-ral0 connect-component)]) 
          (define (loop hint)
            (for ([blockid (in-dict-keys component)])
              (define block (dict-ref id2block blockid))
              (define dominance^
                (for/fold ([collect (dict-ref (unbox (dominance-map)) block)]) ([src (in-neighbors graph-rev block)])
                  (set-intersect collect (dict-ref (unbox (dominance-map)) src))
                ))
              (set! dominance^ (set-union dominance^ (set block)))
              (define dominance-old (dict-ref (unbox (dominance-map)) block))
              (unless (equal? dominance^ dominance-old)
                (set! hint #t)
                (set-box! (dominance-map) (dict-set (unbox (dominance-map)) block dominance^))
              )
            )
            (when hint (loop #f))
          )
          (loop #f)
        )
        ; (print-graph graph)
        ; (displayln (dominance-map))
        ; (displayln x)
        x
      )
    ]))
  ))

(define pass-dominace (λ (x) (send (new dominance) pass x)))

(define init-program-property (match-lambda 
  ([Program _ x] [Program (ordl-make-empty symbol-compare) x])))

; (debug-level 2)
(debug-level 0)
(define compiler-passes
  `(
    ("init" ,init-program-property ,interp-Lvec-prime ,type-check-Lvec)
    ("shrink" ,shrink ,interp-Lvec-prime ,type-check-Lvec)
    ("uniquify" ,uniquify ,interp-Lvec-prime ,type-check-Lvec)
    ("collect-set!" ,collect-set! ,interp-Lvec-prime ,type-check-Lvec)
    ("uncover-get!-exp" ,uncover-get!-exp ,interp-Lvec-prime ,type-check-Lvec-has-type #;,type-check-Lvec)
    ("expose-allocation" ,expose-allocation ,interp-Lvec-prime ,type-check-Lvec)
    ("remove complex opera*" ,remove-complex-opera* ,interp-Lvec-prime ,type-check-Lvec)
    ("explicate control" ,explicate-control ,interp-Cvec ,type-check-Cvec)
    ("instruction selection" ,select-instructions ,interp-pseudo-x86-2)
    ("connect component preparation" ,connect-component ,interp-pseudo-x86-2)
    ("ssa-calc" ,pass-dominace ,interp-pseudo-x86-2)
    ("block uncover live enhanced" ,block-uncover-live-enhanced ,interp-pseudo-x86-2)
    ("uncover live" ,uncover-live ,interp-pseudo-x86-2)
    ("build interference graph" ,build-interference ,interp-pseudo-x86-2)
    ("build color graph" ,color-graph ,interp-pseudo-x86-2)
    ("allocate registers" ,allocate-registers ,interp-x86-2)
    ("patch instructions" ,patch-instructions ,interp-x86-2)
    ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-2)
    ("patch instructions" ,patch-instructions ,interp-x86-2)
  ))

(provide compiler-passes)
