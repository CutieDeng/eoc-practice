#lang racket/base

;; ============================================================================
;; AArch64 Assembly Text Emitter
;; ============================================================================
;;
;; Converts IR to AArch64 assembly text format.
;; Output is compatible with GNU as (gas) and LLVM's integrated assembler.
;;
;; ============================================================================

(require racket/match
         racket/string
         racket/format
         racket/port
         "../ir/types.rkt"
         "../ir/cfg.rkt"
         "../../../../cutie-ftree/pvector.rkt"
         (only-in "../../../../cutie-ftree/graph.rkt" vertex-id? vertex-id-val))

(provide
 ;; Main entry points
 emit-function
 emit-cfg
 emit-to-string
 emit-to-file

 ;; Individual emitters
 emit-insn
 emit-operand
 emit-register
 emit-directive)

;; ============================================================================
;; Emit Configuration
;; ============================================================================

(struct EmitConfig (
  indent           ; String: instruction indent (default "    ")
  comment-char     ; String: comment prefix (default "//")
  label-suffix     ; String: label suffix (default ":")
  use-gas-syntax?  ; Boolean: use GNU as syntax (default #t)
) #:prefab)

(define default-config
  (EmitConfig "    " "//" ":" #t))

;; ============================================================================
;; Register Emission
;; ============================================================================

(define (emit-register reg)
  (match reg
    [(Reg:x id) (format "x~a" id)]
    [(Reg:w id) (format "w~a" id)]
    [(Reg:z id) (format "z~a" id)]
    [(Reg:p id) (format "p~a" id)]
    [(Reg:v id width) (format "v~a.~a" id width)]
    [(Reg:b id) (format "b~a" id)]
    [(Reg:h id) (format "h~a" id)]
    [(Reg:s id) (format "s~a" id)]
    [(Reg:d id) (format "d~a" id)]
    [(Reg:q id) (format "q~a" id)]
    [(Reg:sp) "sp"]
    [(Reg:xzr) "xzr"]
    [(Reg:wzr) "wzr"]
    [(Reg:ffr) "ffr"]
    ;; Virtual registers (should be allocated before emit)
    [(VReg:gpr id _) (format "%%~a" id)]
    [(VReg:sve id) (format "%%z.~a" id)]
    [(VReg:pred id) (format "%%p.~a" id)]
    [(VReg:vec id) (format "%%v.~a" id)]
    [_ (format "~a" reg)]))

;; ============================================================================
;; Operand Emission
;; ============================================================================

(define (emit-operand op)
  (match op
    ;; Registers
    [(? Reg:x?) (emit-register op)]
    [(? Reg:w?) (emit-register op)]
    [(? Reg:z?) (emit-register op)]
    [(? Reg:p?) (emit-register op)]
    [(? Reg:sp?) (emit-register op)]
    [(? Reg:xzr?) (emit-register op)]
    [(? Reg:wzr?) (emit-register op)]
    [(? vreg?) (emit-register op)]

    ;; Immediates
    [(Imm val) (format "#~a" val)]
    [(Imm:shifted val shift) (format "#~a, lsl #~a" val shift)]

    ;; Memory addressing
    [(Mem:base reg)
     (format "[~a]" (emit-register reg))]
    [(Mem:offset reg off)
     (if (= off 0)
         (format "[~a]" (emit-register reg))
         (format "[~a, #~a]" (emit-register reg) off))]
    [(Mem:pre reg off)
     (format "[~a, #~a]!" (emit-register reg) off)]
    [(Mem:post reg off)
     (format "[~a], #~a" (emit-register reg) off)]
    [(Mem:reg base index)
     (format "[~a, ~a]" (emit-register base) (emit-register index))]
    [(Mem:scaled base index scale)
     (format "[~a, ~a, lsl #~a]" (emit-register base) (emit-register index) scale)]

    ;; Labels
    [(Label:named name) (symbol->string name)]
    [(Label:id id) (format ".L~a" id)]
    [(Label:local parent id) (format ".L~a_~a" parent id)]

    ;; Default
    [_ (format "~a" op)]))

;; ============================================================================
;; Instruction Emission
;; ============================================================================

(define (emit-insn insn [config default-config])
  (define indent (EmitConfig-indent config))

  (match insn
    ;; Arithmetic
    [(Insn:arith op dst src1 src2)
     (format "~a~a ~a, ~a, ~a"
             indent op
             (emit-operand dst)
             (emit-operand src1)
             (emit-operand src2))]

    [(Insn:arith2 op dst src)
     (format "~a~a ~a, ~a"
             indent op
             (emit-operand dst)
             (emit-operand src))]

    ;; Load/Store
    [(Insn:load op dst addr)
     (format "~a~a ~a, ~a"
             indent op
             (emit-operand dst)
             (emit-operand addr))]

    [(Insn:store op src addr)
     (format "~a~a ~a, ~a"
             indent op
             (emit-operand src)
             (emit-operand addr))]

    [(Insn:ldp op dst1 dst2 addr)
     (format "~a~a ~a, ~a, ~a"
             indent op
             (emit-operand dst1)
             (emit-operand dst2)
             (emit-operand addr))]

    [(Insn:stp op src1 src2 addr)
     (format "~a~a ~a, ~a, ~a"
             indent op
             (emit-operand src1)
             (emit-operand src2)
             (emit-operand addr))]

    ;; Move
    [(Insn:mov op dst src)
     (format "~a~a ~a, ~a"
             indent op
             (emit-operand dst)
             (emit-operand src))]

    ;; Compare
    [(Insn:cmp op src1 src2)
     (format "~a~a ~a, ~a"
             indent op
             (emit-operand src1)
             (emit-operand src2))]

    ;; Conditional select
    [(Insn:csel op dst src1 src2 cond)
     (format "~a~a ~a, ~a, ~a, ~a"
             indent op
             (emit-operand dst)
             (emit-operand src1)
             (emit-operand src2)
             cond)]

    ;; Branch
    [(Insn:branch op target)
     (format "~a~a ~a" indent op (emit-operand target))]

    [(Insn:cond-branch op cond target)
     (format "~ab.~a ~a" indent cond (emit-operand target))]

    [(Insn:cbz op reg target)
     (format "~a~a ~a, ~a"
             indent op
             (emit-operand reg)
             (emit-operand target))]

    ;; Return
    [(Insn:ret)
     (format "~aret" indent)]

    ;; SVE arithmetic
    [(Insn:sve op pred dst srcs)
     (define src-strs (map emit-operand srcs))
     (format "~a~a ~a, ~a/m, ~a"
             indent op
             (emit-operand dst)
             (emit-operand pred)
             (string-join src-strs ", "))]

    ;; SVE load
    [(Insn:sve-load op pred dst addr)
     (format "~a~a ~a, ~a/z, ~a"
             indent op
             (emit-operand dst)
             (emit-operand pred)
             (emit-operand addr))]

    ;; SVE store
    [(Insn:sve-store op pred src addr)
     (format "~a~a ~a, ~a, ~a"
             indent op
             (emit-operand src)
             (emit-operand pred)
             (emit-operand addr))]

    ;; SVE reduce
    [(Insn:sve-reduce op pred dst src)
     (format "~a~a ~a, ~a, ~a"
             indent op
             (emit-operand dst)
             (emit-operand pred)
             (emit-operand src))]

    ;; SVE compare
    [(Insn:sve-cmp op pd pg src1 src2)
     (format "~a~a ~a, ~a/z, ~a, ~a"
             indent op
             (emit-operand pd)
             (emit-operand pg)
             (emit-operand src1)
             (emit-operand src2))]

    ;; SVE predicate operations
    [(Insn:sve-pred-op op pd pg pn)
     (format "~a~a ~a, ~a, ~a"
             indent op
             (emit-operand pd)
             (emit-operand pg)
             (emit-operand pn))]

    ;; WHILELT
    [(Insn:whilelt pd rn rm)
     (format "~awhilelt ~a.s, ~a, ~a"
             indent
             (emit-operand pd)
             (emit-operand rn)
             (emit-operand rm))]

    ;; Default
    [_ (format "~a// unknown: ~a" indent insn)]))

;; ============================================================================
;; Terminator Emission
;; ============================================================================

(define (emit-terminator term [config default-config])
  (define indent (EmitConfig-indent config))

  ;; Helper to get block label ID
  (define (block-label-id target)
    (cond
      [(vertex-id? target) (vertex-id-val target)]
      [(BlockId? target) (BlockId-id target)]
      [else target]))

  (match term
    [(Term:ret)
     (format "~aret" indent)]

    [(Term:jump target)
     (format "~ab .L~a" indent (block-label-id target))]

    [(Term:cond cond then-target else-target)
     (string-append
      (format "~ab.~a .L~a\n" indent cond (block-label-id then-target))
      (format "~ab .L~a" indent (block-label-id else-target)))]

    [(Term:switch _ _ _)
     (format "~a// switch not implemented" indent)]

    [(Term:unreachable)
     (format "~abrk #0" indent)]

    [#f ""]
    [_ ""]))

;; ============================================================================
;; Block Emission
;; ============================================================================

(define (emit-block block [config default-config])
  (define lines '())

  ;; Helper to get block ID value
  (define (get-block-id-val bid)
    (cond
      [(vertex-id? bid) (vertex-id-val bid)]
      [(BlockId? bid) (BlockId-id bid)]
      [else bid]))

  ;; Label
  (define label (AsmBlock-label block))
  (define label-str
    (match label
      [(Label:named name) (format "~a:" name)]
      [(Label:id id) (format ".L~a:" id)]
      [_ (format ".L~a:" (get-block-id-val (AsmBlock-id block)))]))

  (set! lines (cons label-str lines))

  ;; Instructions
  (for ([insn (in-pvector (AsmBlock-insns block))])
    (set! lines (cons (emit-insn insn config) lines)))

  ;; Terminator
  (define term (AsmBlock-terminator block))
  (when term
    (set! lines (cons (emit-terminator term config) lines)))

  (string-join (reverse lines) "\n"))

;; ============================================================================
;; Function/CFG Emission
;; ============================================================================

(define (emit-cfg cfg fn-name [config default-config])
  (define lines '())

  ;; Function header
  (set! lines (cons (format ".global ~a" fn-name) lines))
  (set! lines (cons (format ".type ~a, %function" fn-name) lines))
  (set! lines (cons (format "~a:" fn-name) lines))

  ;; Emit blocks in order
  (for ([bid (in-cfg-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (when block
      (set! lines (cons (emit-block block config) lines))))

  ;; Function footer
  (set! lines (cons (format ".size ~a, .-~a" fn-name fn-name) lines))

  (string-join (reverse lines) "\n"))

(define (emit-function fn [config default-config])
  (define name (AsmFunction-name fn))
  (define cfg (AsmFunction-body fn))
  (emit-cfg cfg name config))

;; ============================================================================
;; Directive Emission
;; ============================================================================

(define (emit-directive name . args)
  (if (null? args)
      (format ".~a" name)
      (format ".~a ~a" name (string-join (map ~a args) ", "))))

;; ============================================================================
;; File Header/Footer
;; ============================================================================

(define (emit-file-header [config default-config])
  (string-join
   (list "// Generated by aarch64-sve-asm compiler"
         (emit-directive "arch" "armv8-a+sve")
         (emit-directive "text")
         "")
   "\n"))

(define (emit-file-footer)
  "")

;; ============================================================================
;; High-Level API
;; ============================================================================

;; Emit to string
(define (emit-to-string fn [config default-config])
  (string-append
   (emit-file-header config)
   "\n"
   (emit-function fn config)
   "\n"
   (emit-file-footer)))

;; Emit to file
(define (emit-to-file fn path [config default-config])
  (define content (emit-to-string fn config))
  (call-with-output-file path
    (lambda (out)
      (display content out))
    #:exists 'replace))
