#lang racket/base

;; ============================================================
;; Interpreter: X86
;; ============================================================
;;
;; Reference interpreter for x86 pseudo-assembly.
;; Simulates execution on a virtual x86 machine.
;;
;; ============================================================

(require racket/match
         "../ir/x86var.rkt")

(provide interp-x86
         interp-x86program)

;; ============================================================
;; Machine State
;; ============================================================

;; Machine state consists of:
;; - registers: hash from register name to value
;; - memory: hash from address to value
;; - flags: comparison flags (for conditional jumps)

(struct MachineState (registers memory flags) #:transparent #:mutable)

(define (make-initial-state)
  (define regs (make-hash))
  ;; Initialize stack pointer
  (hash-set! regs 'rsp #x7fffffffe000)
  (hash-set! regs 'rbp #x7fffffffe000)
  (hash-set! regs 'rax 0)
  (MachineState regs (make-hash) (make-hash)))

(define (get-reg state reg-name)
  (hash-ref (MachineState-registers state) reg-name 0))

(define (set-reg! state reg-name val)
  (hash-set! (MachineState-registers state) reg-name val))

(define (get-mem state addr)
  (hash-ref (MachineState-memory state) addr 0))

(define (set-mem! state addr val)
  (hash-set! (MachineState-memory state) addr val))

(define (set-flag! state flag val)
  (hash-set! (MachineState-flags state) flag val))

(define (get-flag state flag)
  (hash-ref (MachineState-flags state) flag #f))

;; ============================================================
;; Main Interpreter
;; ============================================================

(define (interp-x86 prog)
  (interp-x86program prog))

(define (interp-x86program prog)
  (match prog
    [(X86Program info blocks)
     (define state (make-initial-state))
     ;; For x86var, variables are stored in a separate hash
     (define vars (make-hash))
     ;; Start from 'main if it exists, otherwise 'start
     (define start-label (if (hash-has-key? blocks 'main) 'main 'start))
     (interp-block blocks state vars start-label)
     ;; Return value is in rax
     (get-reg state 'rax)]))

;; ============================================================
;; Block Interpreter
;; ============================================================

(define (interp-block blocks state vars label)
  ;; 'conclusion without a block means program end
  (when (and (eq? label 'conclusion) (not (hash-has-key? blocks 'conclusion)))
    (void))  ; Just return
  (when (hash-has-key? blocks label)
    (define block (hash-ref blocks label))
    (match block
      [(X86Block info instrs)
       (interp-instrs blocks state vars instrs)])))

(define (interp-instrs blocks state vars instrs)
  (for/fold ([continue? #t])
            ([instr instrs]
             #:break (not continue?))
    (interp-instr blocks state vars instr)))

;; ============================================================
;; Instruction Interpreter
;; ============================================================

(define (interp-instr blocks state vars instr)
  (match instr
    ;; movq
    [(Instr 'movq (list src dst))
     (define val (read-arg state vars src))
     (write-arg state vars dst val)
     #t]

    ;; addq
    [(Instr 'addq (list src dst))
     (define src-val (read-arg state vars src))
     (define dst-val (read-arg state vars dst))
     (write-arg state vars dst (+ dst-val src-val))
     #t]

    ;; subq
    [(Instr 'subq (list src dst))
     (define src-val (read-arg state vars src))
     (define dst-val (read-arg state vars dst))
     (write-arg state vars dst (- dst-val src-val))
     #t]

    ;; negq
    [(Instr 'negq (list dst))
     (define val (read-arg state vars dst))
     (write-arg state vars dst (- val))
     #t]

    ;; xorq
    [(Instr 'xorq (list src dst))
     (define src-val (read-arg state vars src))
     (define dst-val (read-arg state vars dst))
     (write-arg state vars dst (bitwise-xor dst-val src-val))
     #t]

    ;; cmpq - set flags
    [(Instr 'cmpq (list src1 src2))
     (define val1 (read-arg state vars src1))
     (define val2 (read-arg state vars src2))
     (set-flag! state 'ZF (= val2 val1))
     (set-flag! state 'SF (< (- val2 val1) 0))
     (set-flag! state 'OF #f)  ; Simplified
     #t]

    ;; set (setcc)
    [(Instr 'set (list cc dst))
     (define result (check-condition state cc))
     (write-arg state vars dst (if result 1 0))
     #t]

    ;; movzbq
    [(Instr 'movzbq (list src dst))
     (define val (read-arg state vars src))
     (write-arg state vars dst (bitwise-and val #xff))
     #t]

    ;; pushq
    [(Instr 'pushq (list src))
     (define val (read-arg state vars src))
     (define rsp (get-reg state 'rsp))
     (set-reg! state 'rsp (- rsp 8))
     (set-mem! state (- rsp 8) val)
     #t]

    ;; popq
    [(Instr 'popq (list dst))
     (define rsp (get-reg state 'rsp))
     (define val (get-mem state rsp))
     (write-arg state vars dst val)
     (set-reg! state 'rsp (+ rsp 8))
     #t]

    ;; callq
    [(Callq label arity)
     (cond
       [(eq? label 'read_int)
        (display "read> ")
        (flush-output)
        (set-reg! state 'rax (read))]
       [else
        (error 'interp-instr "unknown function: ~a" label)])
     #t]

    ;; retq - stop execution
    [(Retq)
     #f]

    ;; jmp
    [(Jmp label)
     (interp-block blocks state vars label)
     #f]

    ;; jmpif
    [(JmpIf cc label)
     (if (check-condition state cc)
         (begin
           (interp-block blocks state vars label)
           #f)
         #t)]

    [_ (error 'interp-instr "unhandled instruction: ~a" instr)]))

;; ============================================================
;; Argument Read/Write
;; ============================================================

(define (read-arg state vars arg)
  (match arg
    [(Imm n) n]
    [(Reg name) (get-reg state name)]
    [(Deref reg offset)
     (define addr (+ (get-reg state reg) offset))
     (get-mem state addr)]
    [(ByteReg name)
     (bitwise-and (get-reg state (byte-to-quad name)) #xff)]
    [(Var name)
     (hash-ref vars name 0)]
    [_ (error 'read-arg "unknown arg: ~a" arg)]))

(define (write-arg state vars arg val)
  (match arg
    [(Reg name)
     (set-reg! state name val)]
    [(Deref reg offset)
     (define addr (+ (get-reg state reg) offset))
     (set-mem! state addr val)]
    [(ByteReg name)
     (define quad-name (byte-to-quad name))
     (define old (get-reg state quad-name))
     (set-reg! state quad-name
               (bitwise-ior (bitwise-and old (bitwise-not #xff)) (bitwise-and val #xff)))]
    [(Var name)
     (hash-set! vars name val)]
    [_ (error 'write-arg "cannot write to: ~a" arg)]))

(define (byte-to-quad name)
  (case name
    [(al) 'rax]
    [(bl) 'rbx]
    [(cl) 'rcx]
    [(dl) 'rdx]
    [else (error 'byte-to-quad "unknown byte reg: ~a" name)]))

;; ============================================================
;; Condition Checking
;; ============================================================

(define (check-condition state cc)
  (case cc
    [(e) (get-flag state 'ZF)]
    [(ne) (not (get-flag state 'ZF))]
    [(l) (get-flag state 'SF)]
    [(le) (or (get-flag state 'SF) (get-flag state 'ZF))]
    [(g) (and (not (get-flag state 'SF)) (not (get-flag state 'ZF)))]
    [(ge) (not (get-flag state 'SF))]
    [else (error 'check-condition "unknown cc: ~a" cc)]))
