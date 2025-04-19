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

(define caller-save-regs '(rax rcx rdx rsi rdi r8 r9 r10 r11))
(define callee-save-regs '(rsp rbp rbx r12 r13 r14 r15))
(define pass-args-regs '(rdi rsi rdx rcx r8 r9))

(define caller-and-callee-regs (append caller-save-regs callee-save-regs))

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

(define pass-allocate-registers
  (class pass-abstract
    (super-new)
    (define/override (pass p) (match p
      [(X86Program info blocks)
        (define table (dict-ref info 'color-graph))
        (define allo (allocate-registers-block table))
        (define slot-num (+ 1 (foldl max -1 (map cdr table))))
        (define blocks^ 
          (for/fold ([a (ordl-make-empty symbol-compare)]) ([(tag block-inner) (in-dict blocks)]) 
            (ordl-insert a tag (allo block-inner) #f))
        )
        (define stack-size (max 0 (* (- slot-num (length caller-and-callee-regs)) 8)))
        ; (printf "\n")
        ; (printf "\n")
        ; (printf "\n")
        ; (for ([(k v) (in-dict blocks)]) (printf "~a:\n" k)
        ;   (for ([vi (in-ral0 (Block-instr* v))]) (printf "\t~a\n" vi))
        ;   (define live (dict-ref (Block-info v) 'live))
        ;   (for ([li (in-ral0 live)]) (printf "\t\t~a\n" li))
        ; )
        ; (printf "\n")
        ; (for ([(k v) (in-dict blocks^)]) (printf "~a:\n" k)
        ;   (for ([vi (in-ral0 (Block-instr* v))]) (printf "\t~a\n" vi))
        ; )
        ; (printf "\n")
        ; (define interference (dict-ref info 'interference))
        ; (for ([(k v) (in-dict (Graph-out interference))])
        ;   (for ([vi (in-dict-keys v)])
        ;     (printf "inter: ~a - ~a\n" k vi)
        ;   )
        ; )
        (X86Program (dict-set info 'stack-size stack-size) blocks^)
      ]
    ))
    (define/public ((allocate-instr table) instr) (match instr
      [(Instr i list-value) (Instr i (map (allocate-register table) list-value))]
      [(or (Jmp _) (JmpIf _ _)) instr]
      [(Callq _ _) instr]
    ))
    (define/public ((allocate-register table) value) (match value
      [(or (Imm _) (Bool _)) value]
      [(Reg _) value]
      [_
        (define order (dict-ref table value))
        (match order
          [_ #:when (< order (length caller-and-callee-regs)) 
            (Reg (list-ref caller-and-callee-regs order))] 
          [_ (Deref 'rbp (- (* 8 (- order (length caller-and-callee-regs))) 8))]
        )
      ]
    ))
    (define/public ((allocate-registers-block table) block) (match block
      [(Block info instr*)
        (define m (allocate-instr table))
        (define instr*^ (vector->ral (for/vector #:length (ral-length instr*)
          ([i (in-ral0 instr*)]) (m i))))
        (Block info instr*^)
      ]
    ))
  )
)

(require cutie-ftree)
  
(define pass-prelude-and-conclusion
  (class pass-abstract
    (super-new)
    (define/public (get-prelude p) (match p [(X86Program info _)
      (define stack-size (align (dict-ref info 'stack-size) 16))
      (define insts (vector
        (Instr 'pushq (list (Reg 'rbp)))
        (Instr 'movq (list (Reg 'rsp) (Reg 'rbp))) 
        (Instr 'subq (list (Imm stack-size) (Reg 'rsp)))
        (Instr 'movq (list (Imm 65536) (Reg 'rdi)))
        (Instr 'movq (list (Imm 65536) (Reg 'rsi)))
        (Callq 'initialize 2)
        (Instr 'movq (list (Global 'rootstack_begin) (Reg 'r15)))
        (Instr 'movq (list (Imm 0) (Deref 'r15 0)))
        (Instr 'addq (list (Imm 8) (Deref 'r15 0)))
        (Jmp 'start)
      ))
      (Block (ordl-make-empty symbol-compare) (vector->ral insts))
    ]))
    (define/public (get-conclusion p) (match p [(X86Program info _)
      (define stack-size (dict-ref info 'stack-size))
      (define insts (vector
        (Instr 'subq (list (Imm 8) (Reg 'r15)))
        (Instr 'addq (list (Imm stack-size) (Reg 'rsp)))
        (Instr 'popq (list (Reg 'rbp)))
        (Retq )
      ))
      (Block (ordl-make-empty symbol-compare) (vector->ral insts))
    ]))
    (define/override (pass p) (match p [(X86Program info blocks)
      (define prelude (get-prelude p))
      (define conclusion (get-conclusion p))
      (define blocks^
        (for/fold ([a (ordl-make-empty symbol-compare)]) ([(tag block) (in-dict blocks)]) 
          (match-define (Block info instr*) block)
          (define instr*^
            (cond
              [(ral-empty? instr*) (ral-consr instr* (Jmp 'conclusion))]
              [else (define last-instr (ral-viewr instr*))
                (if (Jmp? last-instr) instr* (ral-consr instr* (Jmp 'conclusion)))
              ]
            ))
          (ordl-insert a tag (Block info instr*^) #f)
        )
      )
      (X86Program info
        (dict-set 
          (dict-set blocks^ 'main prelude)
          'conclusion conclusion))
    ]))
  ))


(define prelude-and-conclusion (λ (p) (send (new pass-prelude-and-conclusion) pass p)))

(define cnt (make-parameter #f))
(define block-visited (make-parameter #f))
(define id-map (make-parameter #f))
(define graph (make-parameter #f))
(define id-inv-map (make-parameter #f))
(define simple-graph (make-parameter #f))
(define visit-temporary-set (make-parameter #f))

(define (pass-uncover-live-mixin2 clz)
  (class clz
    (super-new)
    (define/public pass (match-lambda [(and p (X86Program info blocks))
      (search-blocks p (get-blocks-graph p))
    ]))
    (define/public get-block-tail (match-lambda
      [(? ral-empty?) (ral-empty)]
      [seq (=> eh)
        (define-values (l heads) (ral-dropr seq))
        (when (ral-empty? heads) (eh))
        (define l2 (ral-viewr heads))
        (match* (l l2)
          [((Jmp t) (JmpIf _ t2)) (vector->ral (vector t2 t))]
          [((Jmp t) _) (vector->ral (vector t))]
          [(_ _) (eh)]
        )
      ]
      [_ (ral-empty)]
    ))
    (define/public get-blocks-tail (match-lambda [(X86Program info blocks)
      (for/fold ([a (ordl-make-empty symbol-compare)]) ([(tag block) (in-dict blocks)]) 
        (match-define (Block _ instr*) block)
        (ordl-insert a tag (get-block-tail instr*) #f)
      )
    ]))
    (define/public get-blocks-graph (match-lambda [(X86Program info blocks)
      ; (define g (make-multigraph '()))
      (define g (graph-make-empty))
      (for ([(tag block) (in-dict blocks)])
        (match-define (Block _ binstr*) block)
        (define tos (get-block-tail binstr*))
        ; (add-vertex! g tag)
        (set! g (add-vertex g tag))
        ; (for ([t (in-ral0 tos)]) (add-directed-edge! g tag t))
        (for ([t (in-ral0 tos)]) (set! g (add-vertex g t)) (set! g (add-directed-edge g tag t)))
      )
      g
    ]))
    (define (topology-order graph)
      (define visited (mutable-set))
      (define (find nodetag cont)
        (set-add! visited nodetag)
        (for ([to (in-neighbors graph nodetag)])
          (unless (set-member? visited to)
            (set! cont (find to cont)))
        )
        (cons nodetag cont)
      )
      (define ret '())
      (for ([n (in-vertices graph)])
        (cond
          [(set-member? visited n) (void)]
          [else 
            (set! ret (find n ret))
          ])
      )
      ret
    )
    (define/public (search-blocks program g)
      (match-define (X86Program info blocks) program)
      (parameterize ([graph g] [cnt 0] [block-visited (box (ordl-make-empty symbol-compare))] [id-map (box (ordl-make-empty symbol-compare))] [id-inv-map (box (ordl-make-empty integer-compare))] [visit-temporary-set (mutable-set)])
        (for ([tag (in-dict-keys blocks)]) 
          (search-block-impl tag))
        (for ([tag (in-dict-keys blocks)])
          (update-id tag))
        ; (parameterize ([simple-graph (make-multigraph '())])
        (parameterize ([simple-graph (graph-make-empty-raw integer-compare)])
          (build-simple-graph)
          (define inv-graph (transpose (simple-graph)))
          (define torder (topology-order inv-graph))
          (define collects (box (ordl-make-empty integer-compare)))
          (let ([bv (unbox (block-visited))])
            (for ([tag (in-dict-keys blocks)]) 
              (define id (dict-ref bv tag))
              (define get (dict-ref (unbox collects) id '()))
              (set-box! collects (dict-set (unbox collects) id (cons tag get)))
            )
          )
          (define torder^ (for/list ([ti torder]) (dict-ref (unbox collects) ti)))
          (set! info (dict-set info 'connect-component torder^))
          (set! info (dict-set info 'graph (graph)))
          (X86Program info blocks)
        )
      )
    )
    (define/public (update-id tag)
      (define id (dict-ref (unbox (block-visited)) tag))
      (cond
        [(equal? id (dict-ref (unbox (id-map)) tag)) id]
        [else
          (define nxt (update-id (dict-ref (unbox (id-inv-map)) id)))
          (set-box! (block-visited) (dict-set (unbox (block-visited)) tag nxt))
          nxt
        ])
    )
    (define/public (search-block-impl block-tag)
      (define g (graph))
      (define visited (block-visited))
      (match (dict-ref (unbox visited) block-tag #f)
        [#f 
          (define id (cnt))
          (cnt (+ id 1))
          (set-box! (id-map) (dict-set (unbox (id-map)) block-tag id))
          (set-box! (id-inv-map) (dict-set (unbox (id-inv-map)) id block-tag))
          (set-box! visited (dict-set (unbox visited) block-tag id))
          (when (set-member? (visit-temporary-set) block-tag) (error 'search-block-impl "Invalid state, revisit a node. "))
          (set-add! (visit-temporary-set) block-tag)
          (define (drop) (set-remove! (visit-temporary-set) block-tag))
          (define tos
            (for/list ([next-tag (in-neighbors g block-tag)])
              (search-block-impl next-tag)
            ))
          (drop)
          (set! tos (filter (compose not false?) tos))
          (match tos
            ['() id]
            [_ (=> fail)
              (define m (apply min tos))
              (when (> m id) (fail))
              (set-box! visited (dict-set (unbox visited) block-tag m))
              m
            ]
            [_ id]
          )
        ]
        [a
          (if (set-member? (visit-temporary-set) block-tag) a #f)]
      )
    )
    (define/public (build-simple-graph)
      (define g (graph))
      (define bv (block-visited))
      (define sg (simple-graph))
      (for ([f (in-vertices g)])
        (define f^ (dict-ref (unbox bv) f))
        ; (add-vertex! sg f^)
        (set! sg (add-vertex sg f^))
        (for ([t (in-neighbors g f)])
          (define t^ (dict-ref (unbox bv) t))
          (unless (equal? f^ t^)
            ; (add-directed-edge! sg f^ t^)
            (set! sg (add-directed-edge sg f^ t^))
          )
        )
      )
      (simple-graph sg)
    )
  )
)

(define connect-component (λ (p) (send (new (pass-uncover-live-mixin2 object%)) pass p)))

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
    (define/public (analyze-dataflow graph transfer bottom join)
      (define s (subqueue))
      (define mapping (box (ordl-make-empty symbol-compare)))
      (define change-able (mutable-set))
      (define worklist (make-queue))
      (define graph-t (transpose graph))
      (for ([sij (in-vertices graph-t)]) (set-box! mapping (dict-set (unbox mapping) sij bottom)))
      (for ([si s])
        (set-clear! change-able)
        (for ([sij si]) (set-add! change-able sij))
        (for ([sij si]) (enqueue! worklist sij))
        (while (not (queue-empty? worklist))
          (define node (dequeue! worklist))
          (define preds (get-neighbors graph-t node))
          #; (begin
            (printf "node: ~a\n" node)
            (for ([p (in-ral0 preds)]) (printf "\t~a\n" p))
          )
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

(define (pass-build-interference-mixin2 super-class)
  (class super-class
    (super-new) 
    (inherit get-read get-write)
    (define/override (pass p) (super pass p))
  ))

(define build-interference (λ (p) (send (new 
  (pass-build-interference-mixin2 
  (pass-build-interference-mixin 
  (pass-read-write-Cwhile-mixin pass-abstract)))) pass p)))

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

(define pass-allocate-registers-Lvec
  (class pass-allocate-registers
    (super-new)
    (define/override ((allocate-register table) value) (match value
      [(or (Global _) (Deref _ _)) value]
      [_ ((super allocate-register table) value)]
    ))
    (define/override (pass p) (match (super pass p) [(X86Program info blocks)
      ; (pretty-print (map (λ (b) (cons (car b) (Block-instr* (cdr b)))) blocks))
      (X86Program (dict-set info 'num-root-spills 0) blocks)
    ]))
  )
)

(define allocate-registers (λ (p) (send (new pass-allocate-registers-Lvec) pass p)))

(define cast-types-to-tag (match-lambda
  [`(Vector ,types ...)
    (define types-mask 
      (let get-bit-fields ([idx 0] [rest-types types] [mask 1] [rst 0])
        (match rest-types
          [`(Integer ,rest ...) 
            (get-bit-fields (+ idx 1) rest (* mask 2) rst)]
          [`((Vector ,_ ...) ,rest ...) 
            (get-bit-fields (+ idx 1) rest (arithmetic-shift mask 1) (bitwise-ior rst mask))]
          ['() rst]
      ))
    )
    (bitwise-ior 
      (arithmetic-shift types-mask 7)
      (arithmetic-shift (length types) 1)
      1
    )
  ]
))

(define collect-vector-variables 
  (class pass-abstract
    (super-new)
    (define/override pass (match-lambda 
      [(CProgram info blocks)
        (error 'pass "unimpl")
      ]))
    (define/public ((pass-block env) block)
      (error 'pass-block "unimpl")
    )
    (define/public (merge-type-info lhs rhs)
      (for/fold ([r lhs]) ([(k v) (in-dict-pairs rhs)])
        (define r-sub (dict-ref r k '()))
        (define r-sub^ (set-union r-sub v))
        (dict-set r k r-sub^)
      )
    )
    (define/public (get-all-uninit-types block)
      (void)
    )
  ))

(define collect-type-info
  (class pass-abstract
    (super-new)
    (define/override pass (match-lambda 
      [(Program info exp)
        (error 'pass) 
      ]
    ))
    (field [prim-table (ordl-make-empty symbol-compare)])
    (define/public (pass-exp table) (match-lambda
      [(Let var rhs body)
        (define rhs-t ((pass-exp table) rhs))
        (hash-set! table var rhs-t) 
        ((pass-exp table) body)
      ]
      [(Prim op operands) 
        prim-table
        (error 'prim)
      ]
      [(If cnd thn els)
        (define cnd-t ((pass-exp table) cnd))
        (match cnd-t
          ['Void (void)]
          [_ (collect (Void) cnd-t cnd)])
        (define thn-t ((pass-exp table) thn))
        (define els-t ((pass-exp table) els))
        (cond
          [(not (equal? thn-t els-t)) (collect thn-t els-t els)])
        thn-t
      ]
      [(WhileLoop cnd body)
        (define cnd-t ((pass-exp table) cnd))
        (match cnd-t 
          ['Boolean (void)]
          [_ (collect 'Boolean cnd-t cnd)])
        ((pass-exp table) body)
        'Void
      ]
      [(SetBang var rhs)
        (define rhs-t ((pass-exp table) rhs))
        (match (hash-ref table var (λ () #f))
          [#f (hash-set! table var rhs-t)]
          [var-t 
            (unless (equal? var-t rhs-t) (collect var-t rhs-t rhs))
          ])
        'Void
      ]
      [(Begin es body)
        (for ([e es]) ((pass-exp table) e))
        ((pass-exp table) body)
      ]
      [(GetBang var)
        (hash-ref table var (λ () #f))
      ]
      [(Int _) 'Integer]
      [(Bool _) 'Boolean]
      [(GlobalValue _) 'Integer]
      [(HasType _ t) t]
      [(Collect _) 'Void]
    ))
    (abstract collect)
  ))

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
        (define connect-component (reverse (dict-ref info 'connect-component)))
        (set-box! (dominance-map) (dict-set (unbox (dominance-map)) 'start (set 'start)))
        (define graph (dict-ref info 'graph))
        (define graph-rev (transpose graph))
        (for ([component connect-component]) 
          (define (loop hint)
            (for ([block component])
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
