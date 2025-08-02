#lang racket/base

(require racket/struct racket/dict racket/set racket/class racket/string racket/bool racket/function)
(require racket/port racket/system racket/list racket/contract/base)
(require racket/pretty racket/match)
(require (for-syntax racket))
(require rackunit rackunit/text-ui)
(require racket/vector)
(require racket/file)

(require cutie-ftree)

(struct Value (val) #:transparent)
(provide (struct-out Value))

(struct Decl (name type) #:transparent)
(provide (struct-out Decl))

(struct Poly (name type) #:transparent)
(provide (struct-out Poly))

(struct Inst (expr type types) #:transparent)
(provide (struct-out Inst))

(struct Var (id) #:transparent)
(struct Var:r (name) #:transparent)
(provide (contract-out (struct Var ([id integer?])))) 
(provide (contract-out (struct Var:r ([name (or/c symbol? string?)])))) 

(define Var?/weak (or/c Var? Var:r?))
(provide Var?/weak)

(struct Int (v) #:transparent)
(provide (contract-out (struct Int ([v integer?]))))

(struct Let (x e body) #:transparent)
(provide (contract-out (struct Let ([x (or/c Var? Var:r?)] [e exp?] [body exp?]))))

(struct WhileLoop (cnd body) #:transparent)
(provide (contract-out (struct WhileLoop ([cnd exp?] [body exp?]))))

(struct SetBang (var rhs) #:transparent)
(provide (contract-out (struct SetBang ([var Var?/weak] [rhs exp?]))))

(struct GetBang (var) #:transparent)
(provide (contract-out (struct GetBang ([var Var?/weak]))))

(struct Begin (es body) #:transparent)
(provide (contract-out (struct Begin ([es exps?] [body exp?]))))

(struct Bool (v) #:transparent)
(provide (contract-out (struct Bool ([v boolean?]))))

(struct If (cnd thn els) #:transparent)
(provide (contract-out (struct If ([cnd exp?] [thn exp?] [els exp?]))))

(struct Cast (expr source target) #:transparent)
(provide (struct-out Cast))

(struct Void () #:transparent)
(provide (contract-out (struct Void ())))

(struct Prim (op arg*) #:transparent)
(provide (contract-out (struct Prim ([op symbol?] [arg* ral?]))))

(define exp? 
  (or/c Int? Bool? Void? Var?/weak
    Let? Lambda? Prim? Apply? GlobalValue?
    Allocate? AllocateArray? AllocateProxy?
    AllocateClosure?
    If? HasType? UncheckedCast? Cast?
    Collect? FunRef? Call? Inject? Project?
    ValueOf? Closure? WhileLoop? SetBang?
    GetBang? Begin? Value? Inst?
  ))
(define exps? ral?)
(provide exp? exps?)

(struct Apply (fun arg*) #:transparent)
(provide (contract-out (struct Apply ([fun exp?] [arg* exp?]))))

(struct Def (name param* rty info body) #:transparent)
(provide (struct-out Def))

(struct StructDef (name field*) #:transparent)
(provide (struct-out StructDef))

(struct Lambda (param* rty body) #:transparent)
(provide (struct-out Lambda))

(struct Inject (value type) #:transparent)
(provide (struct-out Inject))

(struct ValueOf (value type) #:transparent)
(provide (struct-out ValueOf))

(struct Project (value type) #:transparent)
(provide (struct-out Project))

(struct AssignedFree (var) #:transparent)
(provide (struct-out AssignedFree))

(struct Closure (arity fvs) #:transparent)
(provide (struct-out Closure))

(struct FunRef (name arity) #:transparent)
(provide (struct-out FunRef))

(struct Assign (lhs rhs) #:transparent)
(provide (struct-out Assign))

(struct Return (arg) #:transparent)
(provide (struct-out Return))

(struct Goto (label) #:transparent)
(provide (struct-out Goto))

(struct HasType (expr type) #:transparent)
(provide (struct-out HasType))

(struct UncheckedCast (expr type) #:transparent)
(provide (struct-out UncheckedCast))

(struct GlobalValue (name) #:transparent)
(provide (struct-out GlobalValue))

(struct Global (name) #:transparent)
(provide (struct-out Global))

(struct Collect (size) #:transparent)
(provide (struct-out Collect))

(struct CollectionNeeded? (size) #:transparent)
(provide (struct-out CollectionNeeded?))

(struct Allocate (amount type) #:transparent)
(provide (struct-out Allocate))

(struct AllocateArray (amount type) #:transparent)
(provide (struct-out AllocateArray))

(struct AllocateClosure (amount type arity) #:transparent)
(provide (struct-out AllocateClosure))

(struct AllocateProxy (type) #:transparent)
(provide (struct-out AllocateProxy))

(struct Call (fun arg*) #:transparent)
(provide (struct-out Call))

(struct TailCall (fun arg*) #:transparent)
(provide (struct-out TailCall))

(struct Imm (value) #:transparent)
(provide (struct-out Imm))

(struct Reg (name) #:transparent)
(provide (struct-out Reg))

(struct Deref (reg offset) #:transparent)
(provide (struct-out Deref))

(struct Instr (name arg*) #:transparent)
(provide (struct-out Instr))

(struct Callq (target arity) #:transparent)
(provide (struct-out Callq))

(struct Retq () #:transparent)
(provide (struct-out Retq))

(struct IndirectCallq (target arity) #:transparent)
(provide (struct-out IndirectCallq))

(struct IndirectJmp (target) #:transparent)
(provide (struct-out IndirectJmp))

(struct Jmp (target) #:transparent)
(provide (struct-out Jmp))

(struct TailJmp (target arity) #:transparent)
(provide (struct-out TailJmp))

(struct Block (info instr*) #:transparent)
(provide (struct-out Block))

(struct ByteRef (name) #:transparent)
(provide (struct-out ByteRef))

(struct JmpIf (cnd target) #:transparent)
(provide (struct-out JmpIf))

; Value struct definitions
(struct Tagged (value tag) #:transparent)
(provide (struct-out Tagged))

(struct Function (params body env) #:transparent)
(provide (struct-out Function))

(define Type? (or/c ))

(struct IfStmt (cnd thn els) #:transparent)
(provide (contract-out (struct IfStmt ([cnd exp?] [thn integer?] [els integer?]))))

(struct Program (info body) #:transparent)
(struct ProgramDefsExp (info def* body) #:transparent)
(struct ProgramDefs (info def*) #:transparent)
(struct CProgram (info blocks) #:transparent)

(provide 
  (struct-out Program)
  (struct-out ProgramDefsExp)
  (struct-out ProgramDefs)
  (struct-out CProgram)
)

(struct X86Program (info blocks) #:transparent)
(provide (struct-out X86Program))

(struct RvsdgGraph (info graph) #:transparent)
(provide (struct-out RvsdgGraph))

(define src-primitives 
  '(
    read
    + - * eq? < <= > >= and or not
    vector vector-ref vector-set vector-length
    procedure-arity boolean? integer? vector? procedure? void?
    any-vector-ref any-vector-set! any-vector-length
    any-vectorof-ref any-vectorof-set! any-vectorof-length
    make-vector
  )
)

(define parse-exp (match-lambda
  [(and s (? symbol?)) (Var:r s)]
  [(and i (? integer?)) (Int i)]
  [(and b (? boolean?)) (Bool b)]
  [`(void) (Void)]
  [`(let ([,x ,rhs]) ,body) (Let (parse-exp x) (parse-exp rhs) (parse-exp body))]
  [`(if ,cnd ,thn ,els) (If (parse-exp cnd) (parse-exp thn) (parse-exp els))]
  [`(lambda: ,ps : ,rt ,body) (Lambda ps rt (parse-exp body))]
  [`(lambda: ,ps ,body) (Lambda ps 'Any (parse-exp body))]
  [`(lambda ,ps ,body) (Lambda ps 'Any (parse-exp body))]
  [`(project ,e ,t) (Project (parse-exp e) t)]
  [`(inject ,e ,t) (Inject (parse-exp e) t)]
  [`(while ,cnd ,body) (WhileLoop (parse-exp cnd) (parse-exp body))]
  [`(set! ,x ,rhs) (SetBang x (parse-exp rhs))]
  [`(begin ,es ... ,e)
    (Begin (for/fold ([es^ (ral-empty)]) ([e0 es])
      (ral-consr es^ (parse-exp e0))) (parse-exp e))
  ]
  [`(has-type ,e ,t) (HasType (parse-exp e) t)]
  [`(unchecked-cast ,e ,t) (UncheckedCast (parse-exp e) t)]
  [`(,op ,es ...)
    #:when (set-member? src-primitives op)
    (Prim op (for/fold ([es^ (ral-empty)]) ([e es]) (ral-consr es^ e)))
  ]
  [`(,e ,es ...)
    (Apply (parse-exp e) (for/fold ([es^ (ral-empty)]) ([e0 es]) (ral-consr es^ e0)))
  ]
))
(provide parse-exp)

(define list->ral (compose vector->ral list->vector))
(provide list->ral)

(define parse-def (match-lambda
  [`(define (,f ,ps ...) : ,rty ,body)
    (Def f (list->ral ps) rty '() (parse-exp body))]
  [`(define (,f ,xs ...) ,body)
    (Def f (list->ral xs) 'Any '() (parse-exp body))]
  [`(struct ,name ,fields #:mutable)
    (StructDef name fields)]
  [`(: ,name ,type)
    (Decl name type)]
))

(define (normalize-info info)
  (for/fold ([info^ (ordl-make-empty symbol-compare)]) ([(k v) (in-dict info)])
    (dict-set info^ k v)
  )
)

(define parse-program (match-lambda
  [`(program ,info ,body)
    (Program (normalize-info info) (parse-exp body))]
  [`(program ,info ,def* ... ,body)
    (ProgramDefsExp (normalize-info info)
      (for/fold ([def*^ (ral-empty)]) ([d def*])
        (ral-consr def*^ (parse-def d)))
      (parse-exp body))]
))
(provide parse-program)

(define (unparse-exp e)
  (match e
    [(Var x) x]
    [(Int n) n]
    [(Bool b) b]
    [(Void) '(void)]
    [(Let x rhs body)
     `(let ([,x ,(unparse-exp rhs)]) ,(unparse-exp body))]
    [(Lambda ps rt body)
     `(lambda: ,ps ,rt ,(unparse-exp body))]
    [(Prim op es)
      (define es^ (ral->vector es))
      (vector-map! unparse-exp es^)
     `(,op (vector->list es^))]
    [(Apply e es)
      (define es^ (ral->vector es))
      (vector-map! unparse-exp es^)
     `(,(unparse-exp e) (vector->list es^))]
    ))
(provide unparse-exp)

(define (read-program path)
  (debug "read-program" path)
  (define rst (file->lines path))
  `(program () ,rst)
)
(provide (contract-out (read-program (-> path-string? any/c))))

(define-syntax-rule (define-debug-level name level)
  (...
    (define-syntax (name stx)
      (syntax-case stx ()
        [(_ label value ...)
          #`(when (at-debug-level? level)
            #,(syntax/loc stx (print-label-and-values label value ...)))]))))
(define-debug-level traced 1)
(define-debug-level debug 2)
(define-debug-level verbose 3)
(define-debug-level copious 4)

(define-syntax (print-label-and-values stx)
  (syntax-case stx ()
    [(_ label value ...)
      (let* ([src (syntax-source stx)] [src (if (path? src)
        (find-relative-path (current-directory) src) src)]
          [lno (syntax-line stx)])
        #`(begin
            (printf "~a @ ~a:~a\n" label #,src #,lno)
            (begin
              (printf "~a:\n" 'value)
              (pretty-print value)
              (newline))
            ...
            (newline)))]))

(define (at-debug-level? n)
  (unless (exact-nonnegative-integer? n)
    (error 'at-debug-level? "expected non-negative integer ~a" n))
  (>= (debug-level) n))

;; debug state is a nonnegative integer.
;; The easiest way to increment it is passing the -d option
;; to run-tests.rkt
;; 0 none 
;; 1 traced passes in run-test
;; 2 debug macros
;; 3 verbose debugging
;; 4 (copious) absolutely everything
;; The higher the setting the more information is reported.
;; If you want the same functionality as previous incarnation
;; of utilities then uncomment the line after this definition
;; and change the number there.
(define debug-level
  (make-parameter
    0
    (lambda (d)
      (unless (exact-nonnegative-integer? d) 
        (error 'debug-state "expected nonnegative-integer in ~a" d))
      d)))

(define (test-typecheck typecheck sexp)
  sexp
)

(define (print-asm p)
  (send (new x86-asm-printer-class) print-asm p)
)

(define ((compile typecheck passes) path)
  (define output-asm-path (path-replace-extension path ".s"))
  (traced "(compile)" path output-asm-path)
  (call-with-atomic-output-file output-asm-path (lambda (out-port tmp-path)
    (define sexp (read-program path))
    (define tsexp (test-typecheck typecheck sexp))
    (traced "compile-file" tsexp)
    (define final-p
      (let loop ([passes passes] [p tsexp])
        (match passes
          [`((,name ,pass ,interp ,type-checker) ,passe ...)
            (traced "pass/begin" name)
            (define p^ (pass p))
            (traced "pass/check")
            (define p^^ (type-checker p^))
            (traced "pass/end" name)
            (loop passe p^^)
          ]
          ['() p]
        )
      ))
    (parameterize ([current-output-port out-port])
      (print-asm final-p)
      (newline)
      (flush-output)
    )
  ))
)

(define x86-asm-printer-class
  (class object% (super-new)
    (define/public print-asm-label (match-lambda
      [x (printf "~a" x)]
    ))
    (define/public print-asm-imm (match-lambda
      [(Deref reg i) (printf "~a(%~a)" i reg)]
      [(Imm n) (printf "$~a" n)]
      [(Reg r) (printf "%~a" r)]
      [(ByteRef r) (printf "%~a" r)]
      [(Global label) (print-asm-label label) (printf "(%rip)")]
    ))
    (define/public print-asm-instr (match-lambda
      [(Callq f n) (printf "callq\t") (print-asm-label f)]
      [(IndirectCallq f n) (printf "callq\t*") (print-asm-imm f)]
      [(Jmp l) (printf "jmp ") (print-asm-label l)]
      [(IndirectJmp ta) (printf "jmp *") (print-asm-imm ta)]
      [(Instr 'set `(,cc ,d)) (printf "set~a\t" cc) (print-asm-imm d)]
      [(Instr 'cmpq `(,s1 ,s2)) (printf "cmpq\t") (print-asm-imm s1) (printf ", ") (print-asm-imm s2)]
      [(Instr i `(,s ,d)) (printf "~a\t" i) (print-asm-imm s) (printf ", ") (print-asm-imm d)]
      [(Instr i `(,d)) (printf "~a\t" i) (print-asm-imm d)]
      [(Retq) (printf "retq")]
      [(JmpIf cc l) (printf "j~a " cc) (print-asm-label l)]
    ))
    (define/public print-asm-block (match-lambda
      [(Block info ss)
        (for ([s (in-ral0 ss)]) (printf "\t") (print-asm-instr s) (printf "~n"))
      ]
    ))
    (define/public print-asm (match-lambda
      [(X86Program info blocks)
        (for ([(label block) (in-dict blocks)])
          (match label
            ['main (printf "\t.globl ") (print-asm-label label) (printf "~n")]
            [_ (void)]
          )
          (printf "\t.align 8~n")
          (print-asm-label label) (printf ":~n")
          (print-asm-block block)
          (printf "~n")
        )
      ]
    ))
  ))
