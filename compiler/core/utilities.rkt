#lang racket

(require "core-types.rkt")

(require racket/struct racket/dict racket/set racket/class racket/string racket/bool racket/function)
(require racket/port racket/system racket/list racket/contract/base)
(require racket/pretty racket/match)
(require (for-syntax racket))
(require rackunit rackunit/text-ui)
(require racket/vector)
(require racket/file)

(require cutie-ftree)

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
  [`(let ([,x ,rhs]) ,body) (Let x (parse-exp rhs) (parse-exp body))]
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
    (Prim op (for/list ([e es]) (parse-exp e)))
  ]
  [`(,e ,es ...)
    (Apply (parse-exp e) (for/list ([e0 es]) (parse-exp e0)))
  ]
))
(provide parse-exp)

(define list->ral (compose vector->ral list->vector))
(provide list->ral)

(define parse-def (match-lambda
  [`(define (,f ,ps ...) : ,rty ,body)
    (Def f ps rty '() (parse-exp body))]
  [`(define (,f ,xs ...) ,body)
    (Def f xs 'Any '() (parse-exp body))]
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
     `(,op ,(map unparse-exp es))]
    [(Apply e es)
     `(,(unparse-exp e) ,(map unparse-exp es))]
    ))
(provide unparse-exp)

(define (read-program path)
  (debug "read-program" path)
  (define rst (file->value path))
  `(program () ,rst)
)
(provide (contract-out (read-program (-> path-string? any/c))))

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
(provide debug-level)

(define (at-debug-level? n)
  (unless (exact-nonnegative-integer? n)
    (error 'at-debug-level? "expected non-negative integer ~a" n))
  (>= (debug-level) n))

(define-syntax (print-label-and-values stx)
  (syntax-case stx ()
    [(_ label value ...)
      (let* ([src (syntax-source stx)] [src (if (path? src)
        (find-relative-path (current-directory) src) src)]
          [lno (syntax-line stx)])
        #`(begin
            (printf "~a @ ~a:~a~n" label #,src #,lno)
            (begin
              (printf "(~a . " 'value)
              (pretty-print value #:newline? #f)
              (printf ")~n"))
            ...
            ))]))

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
(provide traced debug verbose copious)

(define (print-asm p)
  (send (new x86-asm-printer-class) print-asm p)
)

(define ((compile passes) path output-asm-path)
  (traced "(compile)" path output-asm-path)
  (call-with-atomic-output-file output-asm-path (lambda (out-port tmp-path)
    (define sexp (read-program path))
    (define sexp^ (parse-program sexp))
    (traced "compile-file" sexp^)
    (define p^^^
      (let loop ([passes passes] [p sexp^])
        (match passes
          [`((,name ,pass ,interp ,type-checker) ,passe ...)
            (traced "pass/begin" name)
            (define p^ (pass p))
            (traced "pass/check")
            (define p^^ (type-checker p^))
            (traced "pass/end" name)
            (loop passe p^^)
          ]
          [`((,name ,pass ,interp) ,passe ...)
            (traced "pass/begin" name)
            (define p^ (pass p))
            (traced "pass/end" name)
            (loop passe p^)
          ]
          ['() p]
        )
      ))
    (parameterize ([current-output-port out-port])
      (print-asm p^^^)
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
      [(Deref r i) (printf "~a(%~a)" i r)]
      [(Imm n) (printf "$~a" n)]
      [(Reg r) (printf "%~a" r)]
      [(ByteReg r) (printf "%~a" r)]
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

(define (wait-or-timeout poll maxsecs)
  (let loop ([slept 0] [delta 1e-3])
    (define remain (- maxsecs slept))
    (cond
      [(>= slept maxsecs) 'timed-out]
      [(< remain delta)
        (sleep remain) (or (poll) 'timed-out)]
      [else (sleep delta) (or (poll)
        (loop (+ slept delta) (* delta 2)))]
    )
  ) 
)

(define env-cc (make-parameter "gcc"))

(define (get-value-or-timeout command timeout)
  (match-define `(,in1 ,out ,_ ,inErr ,handle) (process command))
  (define (poll) (match (handle 'status) ['running #f] ['done-ok 'done-ok] ['done-error 'done-error]))
  (define res (wait-or-timeout poll timeout))
  (begin0
    (match res
      ['timed-out `(error ,res ,timeout)]
      ['done-error `(error ,res ,(handle 'exit-code))]
      ['done-ok `(result ,res ,(read-line in1))]
    )
    (close-input-port in1)
    (close-input-port inErr)
    (close-output-port out)
  )
)

(define (run-compile-test-suites name passes test-family test-nums)
  (define compiler (compile passes))
  (for ([test-number (in-list test-nums)])
    (define test-name (format "~a_~a" test-family test-number))
    ; (define type-error-path (format "./tests/~a.typerr" test-name))
    ; (define type-error-expected (file-exists? type-error-path))
    (define test-src-path (format "./tests/~a.rkt" test-name))
    (define test-asm-path (path-replace-extension test-src-path ".s"))
    (define test-bin-path (path-replace-extension test-src-path ""))
    (define test-input-path (path-replace-extension test-src-path ".in"))
    (define test-expect-path (path-replace-extension test-src-path ".res"))
    (define test-error-code-path (path-replace-extension test-src-path ".err"))
    (define typechecks (compiler test-src-path test-asm-path))
    (define cmd0 (format "~a -g -std=c99 ~a ~a runtime.o -o ~a" (env-cc) "-arch x86_64" test-asm-path test-bin-path))
    (define cmd1 (format "~a < ~a" test-bin-path test-input-path))
    (cond [(system cmd0)
      (match (get-value-or-timeout cmd1 1.0)
        [`(error done-error ,error-code)
          (traced "run-compile-test-suites" error-code)
          (cond 
          [(file-exists? test-error-code-path) 
            (define e (file->value test-error-code-path))
            (unless (equal? e error-code)
              (eprintf "(test-fail (test-name ~s) (actual-error-code ~s) (expect-error-code ~s))~n" test-name error-code e)
            )
          ]
          [else
            (eprintf "(test-fail (test-name ~s) (actual-error-code ~s))~n" test-name error-code)
          ])
        ]
        [`(error timed-out ,time-cost)
          (traced "run-compile-test-suites" time-cost)
        ]
        [`(result done ,content)
          (traced "run-compile-test-suites" content)
        ]
      )
    ]
    [else
      (traced "run-compile-test-suites" "compile failed" test-asm-path)
    ])
  )
)
(provide run-compile-test-suites)

(define (align v s)
  (define r (remainder v s))
  (if (zero? r) v
    (+ v s (- r)))
)
(provide align)
