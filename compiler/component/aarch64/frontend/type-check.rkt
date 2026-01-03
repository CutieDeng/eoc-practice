#lang racket/base

;; AArch64 Assembly Type Checker
;;
;; Strict type checking for register/type compatibility.
;; Ensures that register usage matches declared types.

(require racket/match
         "../../../../cutie-ftree/pvector.rkt"
         "../../../../cutie-ftree/ordered-map.rkt"
         "../../../../cutie-ftree/comparator.rkt"
         "../ir/types.rkt"
         "../ir/cfg.rkt"
         "../ir/config.rkt")

(provide
 ;; Main type checking entry points
 type-check-function
 type-check-insn

 ;; Type environment
 TypeEnv TypeEnv?
 make-type-env
 type-env-lookup
 type-env-extend

 ;; Type checking result
 TypeResult TypeResult?
 TypeResult-ok? TypeResult-type TypeResult-errors
 type-ok
 type-error
 type-errors

 ;; Type compatibility
 register-type-compatible?
 types-equal?
 type-width

 ;; Instruction type inference
 infer-arith-result-type
 infer-load-result-type)

;; ============================================================================
;; Type Checking Result
;; ============================================================================

(struct TypeResult (ok? type errors) #:prefab)

(define (type-ok ty)
  (TypeResult #t ty '()))

(define (type-error msg ctx)
  (TypeResult #f #f (list (cons msg ctx))))

(define (type-errors errs)
  (if (null? errs)
      (TypeResult #t #f '())
      (TypeResult #f #f errs)))

(define (combine-type-results . results)
  (define all-errors
    (apply append (map TypeResult-errors results)))
  (type-errors all-errors))

;; ============================================================================
;; Type Environment
;; ============================================================================

;; TypeEnv maps variable/register names to their types
(struct TypeEnv (bindings) #:prefab)

(define (make-type-env)
  (TypeEnv (ordered-map-empty symbol-compare)))

(define (type-env-lookup env name)
  (ordered-map-ref (TypeEnv-bindings env) name #f))

(define (type-env-extend env name type)
  (TypeEnv (ordered-map-set (TypeEnv-bindings env) name type)))

;; Build environment from function parameters
(define (build-param-env params)
  (for/fold ([env (make-type-env)])
            ([param (in-list params)])
    (type-env-extend env
                     (reg->name (AsmParam-reg param))
                     (AsmParam-type param))))

(define (reg->name reg)
  (match reg
    [(Reg:x id) (string->symbol (format "x~a" id))]
    [(Reg:w id) (string->symbol (format "w~a" id))]
    [(Reg:z id) (string->symbol (format "z~a" id))]
    [(Reg:p id) (string->symbol (format "p~a" id))]
    [(Reg:v id _) (string->symbol (format "v~a" id))]
    [(Reg:sp) 'sp]
    [(Reg:xzr) 'xzr]
    [(Reg:wzr) 'wzr]
    [_ #f]))

;; ============================================================================
;; Register-Type Compatibility
;; ============================================================================

;; Check if a register is compatible with a type
(define (register-type-compatible? reg type)
  (match* (reg type)
    ;; x-registers: 64-bit integers or pointers
    [((Reg:x _) (Type:scalar 64 _)) #t]
    [((Reg:x _) (Type:ptr _)) #t]

    ;; w-registers: 32-bit integers
    [((Reg:w _) (Type:scalar 32 _)) #t]

    ;; xzr/wzr: zero registers
    [((Reg:xzr) (Type:scalar 64 _)) #t]
    [((Reg:wzr) (Type:scalar 32 _)) #t]

    ;; sp: pointer
    [((Reg:sp) (Type:ptr _)) #t]
    [((Reg:sp) (Type:scalar 64 _)) #t]

    ;; v-registers: NEON vectors (based on width)
    [((Reg:v _ 'q) (Type:vec _ 4)) #t]   ; 128-bit = 4x32
    [((Reg:v _ 'q) (Type:vec _ 2)) #t]   ; 128-bit = 2x64
    [((Reg:v _ 'd) (Type:vec _ 2)) #t]   ; 64-bit = 2x32
    [((Reg:v _ 'd) (Type:float 64)) #t]  ; d = 64-bit float
    [((Reg:v _ 's) (Type:float 32)) #t]  ; s = 32-bit float
    [((Reg:v _ 'h) (Type:float 16)) #t]  ; h = 16-bit float

    ;; Specific width registers
    [((Reg:d _) (Type:float 64)) #t]
    [((Reg:s _) (Type:float 32)) #t]
    [((Reg:h _) (Type:float 16)) #t]

    ;; z-registers: SVE vectors
    [((Reg:z _) (Type:sve _)) #t]
    [((Reg:z _) (Type:sve2 _)) #t]

    ;; p-registers: predicates
    [((Reg:p _) (Type:pred)) #t]

    ;; Default: not compatible
    [(_ _) #f]))

;; Check type equality
(define (types-equal? t1 t2)
  (equal? t1 t2))

;; Get type width in bits
(define (type-width type)
  (match type
    [(Type:scalar width _) width]
    [(Type:float width) width]
    [(Type:vec elem lanes) (* (type-width elem) lanes)]
    [(Type:ptr _) 64]
    [(Type:pred) 1]  ; Per lane
    [_ #f]))

;; ============================================================================
;; Type Inference
;; ============================================================================

;; Infer result type for arithmetic operations
(define (infer-arith-result-type op t1 t2)
  (cond
    ;; Integer arithmetic
    [(memq op '(add sub mul sdiv udiv and orr eor))
     (if (types-equal? t1 t2) t1 #f)]

    ;; FP arithmetic
    [(memq op '(fadd fsub fmul fdiv))
     (if (types-equal? t1 t2) t1 #f)]

    ;; Comparison (produces pred or flags)
    [(memq op '(cmp cmn tst))
     (Type:pred)]

    [else #f]))

;; Infer result type for load operations
(define (infer-load-result-type op addr-type)
  (match op
    ['ldr (Type:scalar 64 #t)]
    ['ldrw (Type:scalar 32 #t)]
    ['ldrb (Type:scalar 8 #f)]
    ['ldrh (Type:scalar 16 #f)]
    ['ldrsb (Type:scalar 8 #t)]
    ['ldrsh (Type:scalar 16 #t)]
    ['ldrsw (Type:scalar 32 #t)]
    [_ #f]))

;; ============================================================================
;; Instruction Type Checking
;; ============================================================================

(define (type-check-insn insn env #:config [config #f])
  (match insn
    [(Insn:arith op dst src1 src2)
     (type-check-arith op dst src1 src2 env)]

    [(Insn:arith2 op dst src)
     (type-check-unary op dst src env)]

    [(Insn:mov _ dst src)
     (type-check-mov dst src env)]

    [(Insn:load op dst addr)
     (type-check-load op dst addr env)]

    [(Insn:store op src addr)
     (type-check-store op src addr env)]

    [(Insn:sve op pred dst srcs)
     (type-check-sve op pred dst srcs env config)]

    [(Insn:sve-load op pred dst addr)
     (type-check-sve-load op pred dst addr env config)]

    [(Insn:sve-store op pred src addr)
     (type-check-sve-store op pred src addr env config)]

    [(Insn:ret)
     (type-ok (Type:void))]

    [(Insn:branch _ _)
     (type-ok (Type:void))]

    [(Insn:cond-branch _ _ _)
     (type-ok (Type:void))]

    [(AsmInsn op args)
     (type-check-generic op args env config)]

    [_ (type-ok (Type:void))]))

;; Type check arithmetic instruction
(define (type-check-arith op dst src1 src2 env)
  (define t1 (operand-type src1 env))
  (define t2 (operand-type src2 env))

  (cond
    [(not t1)
     (type-error "cannot determine type of source1" src1)]

    [(not t2)
     (type-error "cannot determine type of source2" src2)]

    [(not (types-equal? t1 t2))
     (type-error "operand type mismatch" (list src1 t1 src2 t2))]

    [else
     (define result-type (infer-arith-result-type op t1 t2))
     (if (and result-type (register-type-compatible? dst result-type))
         (type-ok result-type)
         (type-error "result type incompatible with destination"
                     (list dst result-type)))]))

;; Type check unary instruction
(define (type-check-unary op dst src env)
  (define t (operand-type src env))
  (cond
    [(not t)
     (type-error "cannot determine type of source" src)]
    [(register-type-compatible? dst t)
     (type-ok t)]
    [else
     (type-error "result type incompatible with destination"
                 (list dst t))]))

;; Type check move instruction
(define (type-check-mov dst src env)
  (define t (operand-type src env))
  (cond
    [(not t)
     (type-error "cannot determine type of source" src)]
    [(register-type-compatible? dst t)
     (type-ok t)]
    [else
     (type-error "move: type incompatible with destination"
                 (list dst t))]))

;; Type check load instruction
(define (type-check-load op dst addr env)
  (define result-type (infer-load-result-type op #f))
  (cond
    [(not result-type)
     (type-error "unknown load operation" op)]
    [(register-type-compatible? dst result-type)
     (type-ok result-type)]
    [else
     (type-error "load result type incompatible with destination"
                 (list op dst result-type))]))

;; Type check store instruction
(define (type-check-store op src addr env)
  (define t (operand-type src env))
  (if t
      (type-ok (Type:void))
      (type-error "cannot determine type of store source" src)))

;; Type check SVE instruction
(define (type-check-sve op pred dst srcs env config)
  ;; Check predicate is pred type
  (define pred-type (operand-type pred env))
  (unless (and pred-type (Type:pred? pred-type))
    (type-error "SVE instruction requires predicate register" pred))

  ;; Check sources are SVE vectors
  (for ([src (in-list srcs)])
    (define src-type (operand-type src env))
    (unless (and src-type (or (Type:sve? src-type) (Type:sve2? src-type)))
      (type-error "SVE instruction requires SVE vector operand" src)))

  ;; Check destination is SVE vector
  (define dst-type (operand-type dst env))
  (if (and dst-type (or (Type:sve? dst-type) (Type:sve2? dst-type)))
      (type-ok dst-type)
      (type-error "SVE destination must be z-register" dst)))

;; Type check SVE load
(define (type-check-sve-load op pred dst addr env config)
  (define pred-type (operand-type pred env))
  (unless (and pred-type (Type:pred? pred-type))
    (type-error "SVE load requires predicate register" pred))

  ;; Infer element type from load op
  (define elem-type
    (match op
      [(or 'ld1b 'ld1sb) (Type:scalar 8 (eq? op 'ld1sb))]
      [(or 'ld1h 'ld1sh) (Type:scalar 16 (eq? op 'ld1sh))]
      [(or 'ld1w 'ld1sw) (Type:scalar 32 (eq? op 'ld1sw))]
      ['ld1d (Type:scalar 64 #t)]
      [_ (Type:scalar 32 #t)]))

  (type-ok (Type:sve elem-type)))

;; Type check SVE store
(define (type-check-sve-store op pred src addr env config)
  (define pred-type (operand-type pred env))
  (unless (and pred-type (Type:pred? pred-type))
    (type-error "SVE store requires predicate register" pred))

  (define src-type (operand-type src env))
  (if (and src-type (or (Type:sve? src-type) (Type:sve2? src-type)))
      (type-ok (Type:void))
      (type-error "SVE store requires z-register source" src)))

;; Type check generic instruction
(define (type-check-generic op args env config)
  ;; For generic instructions, just return void
  (type-ok (Type:void)))

;; ============================================================================
;; Operand Type Lookup
;; ============================================================================

(define (operand-type op env)
  (match op
    ;; Register: look up in environment or infer from register type
    [(? any-reg?)
     (or (type-env-lookup env (reg->name op))
         (infer-register-type op))]

    ;; Immediate: integer type (size depends on value)
    [(Imm value)
     (cond
       [(<= (abs value) 255) (Type:scalar 8 #t)]
       [(<= (abs value) 65535) (Type:scalar 16 #t)]
       [(<= (abs value) 4294967295) (Type:scalar 32 #t)]
       [else (Type:scalar 64 #t)])]

    [(Imm:shifted value shift)
     (Type:scalar 64 #t)]

    ;; Memory: pointer type
    [(? mem-addr?)
     (Type:ptr (Type:scalar 8 #f))]

    ;; Label: no type
    [(? label?) #f]

    [_ #f]))

;; Infer type from register alone (no environment)
(define (infer-register-type reg)
  (match reg
    [(Reg:x _) (Type:scalar 64 #t)]
    [(Reg:w _) (Type:scalar 32 #t)]
    [(Reg:xzr) (Type:scalar 64 #t)]
    [(Reg:wzr) (Type:scalar 32 #t)]
    [(Reg:sp) (Type:ptr (Type:scalar 8 #f))]
    [(Reg:z _) (Type:sve (Type:scalar 32 #t))]  ; Default to s32
    [(Reg:p _) (Type:pred)]
    [(Reg:v _ 'd) (Type:float 64)]
    [(Reg:v _ 's) (Type:float 32)]
    [(Reg:v _ 'q) (Type:vec (Type:float 32) 4)]
    [(Reg:d _) (Type:float 64)]
    [(Reg:s _) (Type:float 32)]
    [(Reg:h _) (Type:float 16)]
    [_ #f]))

;; ============================================================================
;; Function Type Checking
;; ============================================================================

(define (type-check-function fn #:config [config #f])
  (define env (build-param-env (AsmFunction-params fn)))

  (define body (AsmFunction-body fn))

  (cond
    [(AsmCfg? body)
     (type-check-cfg body env config)]

    [else
     (type-ok (Type:void))]))

(define (type-check-cfg cfg env config)
  (define results
    (for/list ([block (in-cfg-blocks cfg)])
      (type-check-block block env config)))
  (apply combine-type-results results))

(define (type-check-block block env config)
  (define results
    (for/list ([insn (in-pvector (AsmBlock-insns block))])
      (type-check-insn insn env #:config config)))
  (apply combine-type-results results))
