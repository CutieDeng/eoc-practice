#lang racket/base

(require racket/struct)
(require racket/pretty racket/match)
(require (for-syntax racket))
(require rackunit rackunit/text-ui)
(require cutie-ftree)

(define AST-output-syntax (make-parameter 'abstract-syntax))

(define (make-recur port mode)
  (case mode
    [(#t) write]
    [(#f) display]
    [else (lambda (p port) (print p port mode))]
  ))

(define-syntax-rule (define-debug-level name level)
  (...
    (define-syntax (name stx)
      (syntax-case stx ()
        [(_ label value ...)
          #`(when (at-debug-level? level)
            #,(syntax/loc stx
              (print-label-and-values label value ...)))]))))

(struct Value (val) #:transparent #:property prop:custom-print-quotable 'never)

(struct Decl (name type)
  #:transparent #:property prop:custom-print-quotable 'never)

#; (struct Poly (name type)
  #:transparent #:property prop:custom-print-quotable 'never)

(struct Var (id) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
    [(define write-proc
      (let ([csp (make-constructor-style-printer (lambda (obj) 'Var) (lambda (obj) (list (Var-id obj))))])
        (lambda (ast port mode)
          (cond 
            [(eq? (AST-output-syntax) 'concrete-syntax)
              (let ([recur (make-recur port mode)])
                (match ast
                  [(Var id) (write-var id port)]))]
            [(eq? (AST-output-syntax) 'abstract-syntax)
              (csp ast port mode)]
          ))))])

(struct Int (value) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer (lambda (obj) 'Int) (lambda (obj) (list (Int-value obj))))])
      (lambda  (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(Int n) (recur n port)]))]
          [(eq? (AST-output-syntax) 'abstract-syntax) (csp ast port mode)]
        ))))])

(struct Prim (op arg*) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer (lambda (obj) 'Prim) (lambda (obj) (list (Prim-op obj) (Prim-arg* obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(Prim op arg*)
                  (write-string "(" port)
                  (write-string (symbol->string op) port)
                  (for ([arg (in-ral0 arg*)])
                    (write-string " " port)
                    (recur arg port))
                  (write-string ")" port)
                ]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct Let (var rhs body) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer (lambda (obj) 'Let) (lambda (obj) (list (Let-var obj) (Let-rhs obj) (Let-body obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(Let x rhs body)
                  (let-values ([(line col pos) (port-next-location port)])
                    (write-string "(let ([" port)
                    (write-var x port)
                    (write-string " " port)
                    (recur rhs port)
                    (write-string "])" port)
                    (newline-and-indent port col)
                    (write-string "   " port) ;; indent body
                    (recur body port)
                    (write-string ")" port)
                    ;(newline-and-indent port col)
                    )]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct WhileLoop (cnd body) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc (let ([csp 
    (make-constructor-style-printer (lambda (obj) 'WhileLoop) (lambda (obj) (list (WhileLoop-cnd obj) (WhileLoop-body obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(WhileLoop cnd body)
                  (let-values ([(line col pos) (port-next-location port)])
                    (write-string "(while " port)
                    (recur cnd port)
                    (newline-and-indent port col)
                    (write-string "   " port) ;; indent body
                    (recur body port)
                    (write-string ")" port)
                  )]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(define (write-var id port)
  (write-string "{Var " port)
  (write id port)
  (write-string "}" port)
)

(define (write-bb id port)
  (write-string "{BasicBlock " port)
  (write id port)
  (write-string "}" port)
)

(struct SetBang (var rhs) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp 
      (make-constructor-style-printer (lambda (obj) 'SetBang) (lambda (obj) (list (SetBang-var obj) (SetBang-rhs obj))))])
        (lambda (ast port mode)
          (cond 
            [(eq? (AST-output-syntax) 'concrete-syntax)
              (let ([recur (make-recur port mode)])
                (match ast
                  [(SetBang var rhs)
                    (let-values ([(line col pos) (port-next-location port)])
                      (write-string "(set! " port)
                      (write-var var port)
                      (newline-and-indent port col)
                      (write-string "   " port) ;; indent body
                      (recur rhs port)
                      (write-string ")" port)
                    )]))]
            [(eq? (AST-output-syntax) 'abstract-syntax)
              (csp ast port mode)]
          ))))])

(struct GetBang (var)  #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer (lambda (obj) 'GetBang) (lambda (obj) (list (GetBang-var obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(GetBang var)
                  (let-values ([(line col pos) (port-next-location port)])
                    (write-string "(get! " port)
                    (write-var var port)
                    (write-string ")" port)
                  )]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct Begin (es body)  #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp 
      (make-constructor-style-printer (lambda (obj) 'Begin) (lambda (obj) (list (Begin-es obj) (Begin-body obj))))])
        (lambda (ast port mode)
          (cond 
            [(eq? (AST-output-syntax) 'concrete-syntax)
              (let ([recur (make-recur port mode)])
                (match ast
                  [(Begin es body)
                    (let-values ([(line col pos) (port-next-location port)])
                      (write-string "(begin " port)
                      (newline-and-indent port col)
                      (for ([e (in-ral0 es)])
                        (write-string "   " port) ;; indent
                        (recur e port)
                        (newline-and-indent port col))
                      (write-string "   " port) ;; indent
                      (recur body port)
                      (write-string ")" port)
                    )]))]
            [(eq? (AST-output-syntax) 'abstract-syntax)
              (csp ast port mode)]
          ))))])

(struct Program (info body) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp 
      (make-constructor-style-printer (lambda (obj) 'Program) (lambda (obj) (list (Program-info obj) (Program-body obj))))])
        (lambda (ast port mode)
          (cond 
            [(eq? (AST-output-syntax) 'concrete-syntax)
              (let ([recur (make-recur port mode)])
                (match ast
                  [(Program info body)
                    (write-string "program:" port)
                    (newline port)
                    (print-info info port mode)
                    (cond 
                      [(ral? body)
                        (for ([def (in-ral0 body)])
                          (recur def port)
                          (newline port))]
                      [else
                        (recur body port)])]))]
            [(eq? (AST-output-syntax) 'abstract-syntax)
              (csp ast port mode)]
          ))))])
  
(struct ProgramDefsExp (info def* body) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp 
      (make-constructor-style-printer (lambda (obj) 'ProgramDefsExp) 
        (lambda (obj) (list (ProgramDefsExp-info obj) (ProgramDefsExp-def* obj) (ProgramDefsExp-body obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(ProgramDefsExp info def* body)
                  (write-string "functions:" port)
                  (newline port)
                  (for ([def (in-ral0 def*)]) (recur def port) (newline port))
                  (write-string "program:" port)
                  (newline port)
                  (recur body port)
                ]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct ProgramDefs (info def*) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'ProgramDefs) (lambda (obj) (list (ProgramDefs-info obj) (ProgramDefs-def* obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(ProgramDefs info def*)
                  (write-string "functions:" port)
                  (newline port)
                  (for ([def (in-ral0 def*)])
                    (recur def port)
                    (newline port)(newline port))
                ]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct X86ProgramDefs (info def*) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer 
      (lambda (obj) 'X86ProgramDefs) (lambda (obj) (list (X86ProgramDefs-info obj) (X86ProgramDefs-def* obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(X86ProgramDefs info def*)
                  (write-string "functions:" port)
                  (newline port)
                  (for ([def (in-ral0 def*)])
                    (recur def port)
                    (newline port)(newline port))
                ]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct CProgram (info blocks)
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'CProgram) (lambda (obj) (list (CProgram-info obj) (CProgram-blocks obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(CProgram info blocks)
                  (write-string "program:" port)
                  (newline port)
                  (print-info info port mode)
                  (for ([(label tail) (in-dict blocks)])
                    (write-bb label port)
                    (write-string ":" port)
                    (newline port)
                    (write-string "    " port)
                    (recur tail port)
                    (newline port))]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct X86Program (info blocks)
  #:transparent
  #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let 
      ([csp (make-constructor-style-printer
        (lambda (obj) 'X86Program) (lambda (obj) (list (X86Program-info obj) (X86Program-blocks obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(X86Program info blocks)
                  (write-string "program:" port)
                  (newline port)
                  (print-info info port mode)
                  (for ([(label tail) (in-dict blocks)])
                    (write-bb label port)
                    (write-string ":" port)
                    (newline port)
                    (write-string "    " port)
                    (recur tail port)
                    (newline port))]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct Bool (value) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer (lambda (obj) 'Bool) (lambda (obj) (list (Bool-value obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(Bool b)
                  (recur b port)]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])
  
(struct If (cnd thn els) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer (lambda (obj) 'If) (lambda (obj) (list (If-cnd obj) (If-thn obj) (If-els obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(If cnd thn els)
                  (let-values ([(line col pos) (port-next-location port)])
                    (write-string "(if" port)
                    (write-string " " port)
                    (recur cnd port)
                    (newline-and-indent port col)
                    (write-string "   " port) ;; indent 
                    (recur thn port)
                    (newline-and-indent port col)
                    (write-string "   " port) ;; indent 
                    (recur els port)
                    (write-string ")" port)
                    (newline-and-indent port col)
                  )]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]))))])
  
(struct Cast (expr source target) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'Cast) (lambda (obj) (list (Cast-expr obj) (Cast-source obj) (Cast-target obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(Cast expr src tgt)
                  (let-values ([(line col pos) (port-next-location port)])
                    (write-string "(cast " port)
                    (recur expr port)
                    (write-string " " port)
                    (write-type src port)
                    (write-string " " port)
                    (write-type tgt port)
                    (write-string ")" port)
                  )]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]))))])

(struct IfStmt (cnd thn els) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let 
      ([csp (make-constructor-style-printer
        (lambda (obj) 'IfStmt) (lambda (obj) (list (IfStmt-cnd obj) (IfStmt-thn obj) (IfStmt-els obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(IfStmt cnd thn els)
                  (let-values ([(line col pos) (port-next-location port)])
                    (write-string "if " port)
                    (recur cnd port)
                    (newline-and-indent port col)
                    (write-string "   " port) ;; indent 
                    (recur thn port)
                    (newline-and-indent port col)
                    (write-string "else" port)
                    (newline-and-indent port col)            
                    (write-string "   " port) ;; indent 
                    (recur els port)
                  )]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct Void () #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer (lambda (obj) 'Void) (lambda (obj) (list)))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (match ast
              [(Void)
                (write-string "(void)" port)])]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])
  
(struct Apply (fun arg*) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let 
      ([csp (make-constructor-style-printer (lambda (obj) 'Apply) (lambda (obj) (list (Apply-fun obj) (Apply-arg* obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(Apply fun arg*)
                  (write-string "(" port)
                  (recur fun port)
                  (for ([arg (in-ral0 arg*)])
                    (write-string " " port)
                    (recur arg port))
                  (write-string ")" port)
                ]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(define (write-type ty port)
  (match ty
    [`(Vector ,tys ...)
      (write-string "(Vector" port)
      (for ([ty tys])
        (write-string " " port)
        (write-type ty port))
      (write-string ")" port)]
    [`(PVector ,tys ...)
      (write-string "(PVector" port)
      (for ([ty tys])
        (write-string " " port)
        (write-type ty port))
      (write-string ")" port)]
    [`(Vectorof ,ty)
      (write-string "(Vectorof " port)
      (write-type ty port)
      (write-string ")" port)]
    [`(,ts ... -> ,rt)
      (write-string "(" port)
      (for ([t ts])
        (write-type t port)
        (write-string " " port))
      (write-string "-> " port)
      (write-type rt port)
      (write-string ")" port)]
    [(? symbol?)
      (write-string (symbol->string ty) port)]
  ))
  
(define (write-params param* port)
  (define fst #t)
  (for ([param param*])
    (match param
      [(? symbol?)
        (write-string (symbol->string param) port)]
      [`(,x : ,t)
        (if fst (set! fst #f) (write-string " " port))
        (write-string "[" port)
        (write-string (symbol->string x) port)
        (write-string " : " port)
        (write-type t port)
        (write-string "]" port)])))

(struct Def (name param* rty info body) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
     (let 
      ([csp (make-constructor-style-printer
        (lambda (obj) 'Def) (lambda (obj) (list (Def-name obj) (Def-param* obj)
          (Def-rty obj) (Def-info obj) (Def-body obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(Def name ps rty info body)
                  (let-values ([(line col pos) (port-next-location port)])
                    (write-string "(define (" port)
                    (write-string (symbol->string name) port)
                    (if (< 0 (length ps)) (write-string " " port) (void))
                    (write-params ps port)
                    (write-string ") " port)
                    (write-string ":" port)
                    (write-string " " port)
                    (write-type rty port)
                    (newline-and-indent port col)
                    (print-info info port mode)
                    (newline-and-indent port col)
                    (write-string "   " port)
                    (cond 
                      [(ral? body)
                        (for ([block (in-ral0 body)])
                          (cond 
                            [(pair? block)
                              (write-string (symbol->string (car block)) port)
                              (write-string ":" port)
                              (newline-and-indent port col)
                              (write-string "      " port)
                              (recur (cdr block) port)
                              (newline-and-indent port col)
                              (write-string "   " port)]
                            [else
                              (recur block port)
                              (newline-and-indent port col)
                              (write-string "   " port)]))]
                      [else
                        (recur body port)])
                    (newline-and-indent port col)            
                    (write-string ")" port)
                  )]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct StructDef (name field*) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc (let ([csp (make-constructor-style-printer
    (lambda (obj) 'StructDef) (lambda (obj) (list (StructDef-name obj) (StructDef-field* obj))))])
    (lambda (ast port mode)
      (cond 
        [(eq? (AST-output-syntax) 'concrete-syntax)
          (let ([recur (make-recur port mode)])
            (match ast
              [(StructDef name ps)
                (let-values ([(line col pos) (port-next-location port)])
                  (write-string "(struct " port)
                  (write-string (symbol->string name) port)
                  (write-string "(" port)
                  (write-params ps port)
                  (write-string "))" port)
                )]))]
        [(eq? (AST-output-syntax) 'abstract-syntax)
          (csp ast port mode)]
      ))))])

(struct Lambda (param* rty body) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'Lambda) (lambda (obj) (list (Lambda-param* obj) (Lambda-rty obj) (Lambda-body obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(Lambda ps rty body)
                  (let-values ([(line col pos) (port-next-location port)])
                  (write-string "(lambda: (" port)
                  (write-params ps port)
                  (write-string ") " port)
                  (write-string ":" port)
                  (write-string " " port)
                  (write-type rty port)
                  (newline-and-indent port col)
                  (write-string "   " port)
                  (recur body port)
                  (write-string ")" port)
                  (newline-and-indent port col)
                )]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct Inject (value type)
  #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write  
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'Inject) (lambda (obj) (list (Inject-value obj) (Inject-type obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(Inject value type)
                  (write-string "(inject " port)
                  (recur value port)
                  (write-string " " port)
                  (recur type port)
                  (write-string ")" port)
                ]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct ValueOf (value type) #:transparent #:property prop:custom-print-quotable 'never)

(struct Project (value type)
  #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'Project) (lambda (obj) (list (Project-value obj) (Project-type obj))))])
        (lambda (ast port mode)
          (cond 
            [(eq? (AST-output-syntax) 'concrete-syntax)
              (let ([recur (make-recur port mode)])
                (match ast
                  [(Project value type)
                    (write-string "(project " port)
                    (recur value port)
                    (write-string " " port)
                    (recur type port)
                    (write-string ")" port)
                  ]))]
            [(eq? (AST-output-syntax) 'abstract-syntax)
              (csp ast port mode)]
          ))))])

(struct AssignedFree (var)
  #:transparent #:property prop:custom-print-quotable 'never)

(struct Closure (arity fvs)
  #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'Closure) (lambda (obj) (list (Closure-arity obj)(Closure-fvs obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(Closure arity fvs)
                  (write-string "(closure " port)
                  (recur arity port)
                  (write-string " " port)
                  (recur fvs port)
                  (write-string ")" port)
                ]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct FunRef (name arity) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'FunRef) (lambda (obj) (list (FunRef-name obj) (FunRef-arity obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(FunRef f n)
                  (write-string "(fun-ref" port)
                  (write-string " " port)
                  (write-string (symbol->string f) port)
                  (write-string " " port)
                  (recur n port)
                  (write-string ")" port)
                ]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct Assign (lhs rhs) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'Assign) (lambda (obj) (list (Assign-lhs obj) (Assign-rhs obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(Assign lhs rhs)
                  (let-values ([(line col pos) (port-next-location port)])
                    (recur lhs port)
                    (write-string " " port)
                    (write-string "=" port)
                    (write-string " " port)
                    (recur rhs port)
                    (write-string ";" port)
                  )]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]))))])

(struct Seq (seq* end) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'Seq) (lambda (obj) (list (Seq-seq* obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(Seq seq* end)
                  (let-values ([(line col pos) (port-next-location port)])
                    (for ([seq (in-ral0 seq*)]) (recur seq port) (newline-and-indent port col))
                    (recur end port)
                  )]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct Return (arg) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'Return) (lambda (obj) (list (Return-arg obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(Return e)
                  (let-values ([(line col pos) (port-next-location port)])
                    (write-string "return" port)
                    (write-string " " port)
                    (recur e port)
                    (write-string ";" port)
                    )
                ]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct Goto (label) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'Goto) (lambda (obj) (list (Goto-label obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(Goto label)
                  (let-values ([(line col pos) (port-next-location port)])
                    (write-string "goto" port)
                    (write-string " " port)
                    (write-bb label port)
                    (write-string ";" port)
                  )]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct HasType (expr type) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'HasType) (lambda (obj) (list (HasType-expr obj) (HasType-type obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(HasType expr type)
                  (recur expr port)
                ]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct UncheckedCast (expr type) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'UncheckedCast) (lambda (obj) (list (UncheckedCast-expr obj) (UncheckedCast-type obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(UncheckedCast expr type)
                  (recur expr port)
                ]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct GlobalValue (name) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'GlobalValue) (lambda (obj) (list (GlobalValue-name obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(GlobalValue name)
                  (write-string "(global-value " port)
                  (write-string (symbol->string name) port)
                  (write-string ")" port)
                ]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct Global (name) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'Global) (lambda (obj) (list (Global-name obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(Global name)
                  (write-string (symbol->string name) port)
                  (write-string "(%rip)" port)
                ]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct Collect (size) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
    (match ast
      [(Collect size)
        (let-values ([(line col pos) (port-next-location port)])
          (write-string "(collect " port)
          (write size port)
          (write-string ")" port)
        )
      ]))])

(struct CollectionNeeded? (size) #:transparent #:property prop:custom-print-quotable 'never)

(struct Allocate (amount type) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
    (match ast
      [(Allocate amount type)
        (let-values ([(line col pos) (port-next-location port)])
          (write-string "(allocate " port)
          (write amount port)
          (write-string " " port)
          (write-type type port)
          (write-string ")" port)
        )
      ]))])

(struct AllocateArray (amount type) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
    (match ast
      [(AllocateArray amount type)
        (let-values ([(line col pos) (port-next-location port)])
          (write-string "(allocate-array " port)
          (write amount port)
          (write-string " " port)
          (write-type type port)
          (write-string ")" port)
        )
      ]))])

(struct AllocateClosure (amount type arity) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
    (match ast
      [(AllocateClosure len type arity)
        (let-values ([(line col pos) (port-next-location port)])
          (write-string "(allocate-closure " port)
          (write len port)
          (write-string " " port)
          (write-type type port)
          (write-string " " port)
          (write arity port)
          (write-string ")" port)
        )
      ]))])

(struct AllocateProxy (type) #:transparent #:property prop:custom-print-quotable 'never)

(struct Call (fun arg*) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'Call) (lambda (obj) (list (Call-fun obj) (Call-arg* obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(Call fun arg*)
                  (write-string "(call " port)
                  (recur fun port)
                  (for ([arg (in-ral0 arg*)])
                    (write-string " " port)
                    (recur arg port))
                  (write-string ")" port)
                ]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct TailCall (fun arg*) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'TailCall) (lambda (obj) (list (TailCall-fun obj) (TailCall-arg* obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(TailCall fun arg*)
                  (write-string "(tail-call " port)
                  (recur fun port)
                  (for ([arg (in-ral0 arg*)])
                    (write-string " " port)
                    (recur arg port))
                  (write-string ")" port)
                ]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct Imm (value) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'Imm) (lambda (obj) (list (Imm-value obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (match ast
              [(Imm n)
                (write-string "$" port)
                (write n port)])]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct Reg (name) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'Reg) (lambda (obj) (list (Reg-name obj))))])
      (lambda (ast port mode)
      (cond 
        [(eq? (AST-output-syntax) 'concrete-syntax)
          (match ast
            [(Reg r)
              (write-string "%" port)
              (write r port)])]
        [(eq? (AST-output-syntax) 'abstract-syntax)
          (csp ast port mode)]
      ))))])

(struct Deref (reg offset) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'Deref) (lambda (obj) (list (Deref-reg obj) (Deref-offset obj))))])
        (lambda (ast port mode)
          (cond 
            [(eq? (AST-output-syntax) 'concrete-syntax)
              (match ast
                [(Deref reg offset)
                  (write offset port)
                  (write-string "(" port)
                  (write-string "%" port)
                  (write reg port)
                  (write-string ")" port)
                ])]
            [(eq? (AST-output-syntax) 'abstract-syntax)
              (csp ast port mode)]
          ))))])

(struct Instr (name arg*) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'Instr) (lambda (obj) (list (Instr-name obj) (Instr-arg* obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(Instr 'set (list cc arg))
                  (let-values ([(line col pos) (port-next-location port)])
                    (write-string "set" port)
                    (write-string (symbol->string cc) port)
                    (write-string " " port)
                    (recur arg port)
                    (newline-and-indent port col))]
                [(Instr name arg*)
                  (let-values ([(line col pos) (port-next-location port)])
                    (write-string (symbol->string name) port)
                    (for ([arg arg*] [i (in-naturals)])
                      (cond [(not (eq? i 0))
                        (write-string "," port)])
                      (write-string " " port)
                      (recur arg port)
                      (set! i (add1 i))
                    )
                    (newline-and-indent port col))]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct Callq (target arity) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'Callq) (lambda (obj) (list (Callq-target obj) (Callq-arity obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(Callq label arity)
                  (let-values ([(line col pos) (port-next-location port)])
                    (write-string "callq" port)
                    (write-string " " port)
                    (write-string (symbol->string label) port)
                    (newline-and-indent port col))]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct Retq () #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'Retq) (lambda (obj) (list)))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (match ast
              [(Retq)
                (let-values ([(line col pos) (port-next-location port)])
                  (write-string "retq" port)
                  (newline-and-indent port col))])]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct IndirectCallq (target arity) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'IndirectCallq) (lambda (obj) (list (IndirectCallq-target obj) (IndirectCallq-arity obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(IndirectCallq target arity)
                  (let-values ([(line col pos) (port-next-location port)])
                    (write-string "callq" port)
                    (write-string " " port)
                    (write-string "*" port)
                    (recur target port)
                    (newline-and-indent port col))]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct IndirectJmp (target) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'IndirectJmp) (lambda (obj) (list (IndirectJmp-target obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(IndirectJmp target)
                  (let-values ([(line col pos) (port-next-location port)])
                    (write-string "jmp" port)
                    (write-string " " port)
                    (write-string "*" port)
                    (recur target port)
                    (newline-and-indent port col))]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct Jmp (target) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'Jmp) (lambda (obj) (list (Jmp-target obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (match ast
              [(Jmp target)
                (let-values ([(line col pos) (port-next-location port)])
                  (write-string "jmp" port)
                  (write-string " " port)
                  (write target port)
                  (newline-and-indent port col))])]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct TailJmp (target arity) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'TailJmp) (lambda (obj) (list (TailJmp-target obj) (TailJmp-arity obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (match ast
              [(TailJmp target arity)
                (let-values ([(line col pos) (port-next-location port)])
                  (write-string "tail-jmp" port)
                  (write-string " " port)
                  (write target port)
                  (newline-and-indent port col))])]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct Block (info instr*) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'Block) (lambda (obj) (list (Block-info obj) (Block-instr* obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (let ([recur (make-recur port mode)])
              (match ast
                [(Block info instr*)
                  (print-info info port mode)
                  (for ([instr (in-ral0 instr*)])
                    (recur instr port))
                ]))]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct StackArg (number) #:transparent #:property prop:custom-print-quotable 'never) ;; no longer needed? -Jeremy

(struct ByteReg (name) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define (write-proc reg port mode)
    (match reg
      [(ByteReg r)
        (write-string "%" port)
        (write r port)]))])

(struct JmpIf (cnd target) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
    (let ([csp (make-constructor-style-printer
      (lambda (obj) 'JmpIf) (lambda (obj) (list (JmpIf-cnd obj) (JmpIf-target obj))))])
      (lambda (ast port mode)
        (cond 
          [(eq? (AST-output-syntax) 'concrete-syntax)
            (match ast
              [(JmpIf cnd target)
                (let-values ([(line col pos) (port-next-location port)])
                  (write-string "j" port)
                  (write cnd port)
                  (write-string " " port)
                  (write target port)
                  (newline-and-indent port col))])]
          [(eq? (AST-output-syntax) 'abstract-syntax)
            (csp ast port mode)]
        ))))])

(struct Tagged (value tag) #:transparent)

(struct Function (params body env) #:transparent)

(struct CFunction (params info blocks env) #:transparent)

(struct X86Function (info blocks env) #:transparent)

(define (any? e) #t)

(define (symbol-list? xs)
  (or (null? xs)
      (and (symbol? (car xs))
           (symbol-list? (cdr xs)))))

(define (param-list? ps)
  (or (null? ps)
      (and (param? (car ps))
           (param-list? (cdr ps)))))

(define (param? p)
  (match p
    [(? symbolic?) #t]
    [`(,x : ,t) (and (symbolic? x) (type? t))]
    [else #f]))

(define (type-list? es)
  (or (null? es)
      (and (type? (car es))
           (type-list? (cdr es)))))

(define (exp-list? es)
  (or (null? es)
      (and (pair? es)
           (exp? (car es))
           (exp-list? (cdr es)))))

(define (exp? e)
  (match e
    [(Var x) #t]
    [(Int n) #t]
    [(Bool b) #t]
    [(Void) #t]
    [(Let x rhs body) #t]
    [(Lambda ps rt body) #t]
    [(Prim op es) #t]
    [(Apply e es) #t]
    [(GlobalValue n) #t]
    [(Allocate n t) #t]
    [(AllocateArray n t) #t]
    [(AllocateProxy t) #t]
    [(AllocateClosure n t a) #t]
    [(If cnd thn els) #t]
    [(HasType e t) #t]
    [(UncheckedCast e t) #t]
    [(Cast e src tgt) #t]
    [(Collect s) #t] ;; update figure in book? see expose-alloc-exp in vectors.rkt
    [(FunRef f n) #t]
    [(Call f e*) #t]
    [(Inject e t) #t]
    [(Project e t) #t]
    [(ValueOf e t) #t]
    [(Closure arity fvs) #t]
    [(WhileLoop cnd body) #t]
    [(SetBang x rhs) #t]
    [(GetBang x) #t]
    [(Begin es body) #t]
    [(Value v) #t]
    [(Inst e ty ts) #t]
    [else #f]))

(define (atm? e)
  (match e
    [(Var x) #t]
    [(Int n) #t]
    [(Bool b) #t]
    [(Void) #t]
    [(HasType e t) (atm? e)]
    [(UncheckedCast e t) (atm? e)]
    [else #f]))

(define (atm-list? es)
  (or (null? es)
      (and (pair? es)
           (atm? (car es))
           (atm-list? (cdr es)))))

(define (type? t)
  (match t
    [`(Vector ,ts ...) #t]
    [`(Vectorof ,t) #t]
    [`(PVector ,ts ...) #t]
    [`(All ,xs ,t) #t]
    ['Integer #t]
    ['Boolean #t]
    ['Void #t]
    [`(,ts ... -> ,t) #t]
    ['() #t] ;; for when a type is not specified
    ['_ #t]  ;; also for when a type is not specified
    ['Any #t]
    [(? symbol?) #t] ;; for type parameters
    [else #f]))

(define (lhs? v)
  (match v
    [(Var x) #t]
    [(Reg r) #t]
    [else #f]))

(define (stmt? s)
  (match s
    [(Assign x e) #t]
    [(Collect n) #t]
    [(Prim 'vector-set! es) #t]
    [(Prim 'vectorof-set! es) #t]
    [(Prim 'any-vector-set! es) #t]
    [(Prim 'any-vectorof-set! es) #t]
    [(Prim 'read '()) #t]
    [(Prim 'exit '()) #t]
    [(Call f arg) #t]
    [else #f]))

(define (tail? t)
  (match t
    [(Return e) #t]
    [(Seq s t) #t]
    [(Goto l) #t]
    [(IfStmt cnd els thn) #t]
    [(TailCall f arg*) #t]
    [(Prim 'exit '()) #t]
    [else #f]))

(define (goto? s)
  (match s
    [(Goto l) #t]
    [else #f]))

(define (cmp? e)
  (match e
    [(Prim cmp (list arg1 arg2)) #t] ;; should also check cmp -Jeremy
    ; [(Bool _) #t] ;; raw bool also available ) by cutiedeng, fxxk, needs more cmp... 
    ; [(Var _) #t]
    [else #f]))

(define (arg? arg)
  (match arg
    [(Imm n) #t]
    [(Var x) #t]
    [(Reg r) #t]
    [(Deref r n) #t]
    [(ByteReg r) #t]
    [(? symbol?) #t] ;; for condition code in set instruction
    [(Global name) #t]
    [(FunRef f n) #t]
    [else #f]))

(define (arg-list? es)
  (or (null? es)
      (and (arg? (car es))
           (arg-list? (cdr es)))))

(define (instr? ins)
  (match ins
    [(Instr name arg*) #t]
    [(Callq label n) #t]
    [(Retq) #t]
    [(IndirectCallq a n) #t]
    [(Jmp label) #t]
    [(IndirectJmp target) #t]
    [(TailJmp a n) #t]
    [(JmpIf c t) #t]
    [else #f]
    ))

(define (instr-list? es)
  (ral? es))

(define src-primitives
  '(read + - * eq? < <= > >= and or not
         vector vector-ref vector-set! vector-length
         procedure-arity
         boolean? integer? vector? procedure? void?
         any-vector-ref any-vector-set! any-vector-length
         any-vectorof-ref any-vectorof-set! any-vectorof-length
         make-vector))

(define (parse-exp e)
  (match e
    [(? symbol?) (Var e)]
    [(? fixnum?) (Int e)]
    [(? boolean?) (Bool e)]
    [`(void) (Void)]
    [`(let ([,x ,rhs]) ,body) (Let x (parse-exp rhs) (parse-exp body))]
    [`(if ,cnd ,thn ,els) (If (parse-exp cnd) (parse-exp thn) (parse-exp els))]
    [`(lambda: ,ps : ,rt ,body)
      (Lambda ps rt (parse-exp body))]
    [`(lambda: ,ps ,body)
      (Lambda ps 'Any (parse-exp body))]
    [`(lambda ,ps ,body) ;; dynamically typed lambda
      (Lambda ps 'Any (parse-exp body))]
    [`(project ,e ,t)
      (Project (parse-exp e) t)]
    [`(inject ,e ,t)
      (Inject (parse-exp e) t)]
    [`(while ,cnd ,body)
      (WhileLoop (parse-exp cnd) (parse-exp body))]
    [`(set! ,x ,rhs)
      (SetBang x (parse-exp rhs))]
    [`(begin ,es ... ,e)
      (Begin (for/fold ([r (ral-empty)]) ([e es]) (ral-consr r (parse-exp e))) (parse-exp e))]
    [`(has-type ,e ,t)
      (HasType (parse-exp e) t)]
    [`(unchecked-cast ,e ,t)
      (UncheckedCast (parse-exp e) t)]
    [`(,op ,es ...)
      #:when (set-member? src-primitives op)
      (Prim op (for/fold ([r (ral-empty)]) ([e es]) (ral-consr r (parse-exp e))))]
    [`(,e ,es ...)
      (Apply (parse-exp e) (for/fold ([r (ral-empty)]) ([e es]) (ral-consr r (parse-exp e))))]
    ))

(define (parse-def d)
  (match d
    [`(define (,f ,ps ...) : ,rty ,body)
     (Def f ps rty '() (parse-exp body))]
    [`(define (,f ,xs ...) ,body) ;; dynamically typed definition
     (Def f xs 'Any '() (parse-exp body))]
    [`(struct ,name ,fields #:mutable)
     (StructDef name fields)]    
    [`(: ,name ,type)
     (Decl name type)]
    ))

(define (parse-program p)
  (match p
    [`(program ,info ,body)
     (Program info (parse-exp body))]
    [`(program ,info ,def* ... ,body)
     (ProgramDefsExp info
                  (for/fold ([x (ral-empty)]) ([d def*]) (ral-consr x (parse-def d)))
                  (parse-exp body))]
    ))

(define (ral-map m r)
  (for/fold ([x (ral-empty)]) ([r0 (in-ral0 r)]) (ral-consr x (m r0)))
)
  
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
     ; `(,op ,@(map unparse-exp es))]
     `(,op ,(ral-map unparse-exp es))]

    [(Apply e es)
     `(,(unparse-exp e) ,(ral-map unparse-exp es))]
    ))

(define print-x86-class
  (class object%
    (super-new)

    (define/public (print-x86-imm e)
      (match e
        [(Deref reg i)
         (format "~a(%~a)" i reg)]
        [(Imm n) (format "$~a" n)]
        [(Reg r) (format "%~a" r)]
        [(ByteReg r) (format "%~a" r)]
        #;[(FunRef label n) (format "~a(%rip)" (label-name label))]
        [(Global label) (format "~a(%rip)" (label-name label))]
        ))
    
    (define/public (print-x86-instr e)
      (verbose "print-x86-instr" e)
      (match e
        [(Callq f n)
         (format "\tcallq\t~a\n" (label-name (symbol->string f)))]
        [(IndirectCallq f n)
         (format "\tcallq\t*~a\n" (print-x86-imm f))]
        [(Jmp label) (format "\tjmp ~a\n" (label-name label))]
        [(IndirectJmp target) (format "\tjmp *~a\n" (print-x86-imm target))]
        [(Instr 'set (list cc d)) (format "\tset~a\t~a\n" cc (print-x86-imm d))]
        [(Instr 'cmpq (list s1 s2))
         (format "\tcmpq\t~a, ~a\n" (print-x86-imm s1) (print-x86-imm s2))]
        [(Instr instr-name (list s d))
         (format "\t~a\t~a, ~a\n" instr-name
                 (print-x86-imm s) 
                 (print-x86-imm d))]
        [(Instr instr-name (list d))
         (format "\t~a\t~a\n" instr-name (print-x86-imm d))]
        [(Retq)
         "\tretq\n"]
        [(JmpIf cc label) (format "\tj~a ~a\n" cc (label-name label))]
        [else (error "print-x86-instr, unmatched" e)]))
    
    (define/public (print-x86-block e)
      (match e
        [(Block info ss)
         (string-append* (for/list ([s (in-ral0 ss)]) (print-x86-instr s)))]
        [else (error "print-x86-block unhandled " e)]))

    (define/public (print-x86 e)
      (match e
        [(X86Program info blocks)
         (string-append
          (string-append*
           (for/list ([(label block) (in-dict blocks)])
             (string-append
              (if (eq? label 'main)
                  (format "\t.globl ~a\n" (label-name 'main))
                  "")
              (string-append
               "\t.align 8\n"
               (format "~a:\n" (label-name label))
               (print-x86-block block)
               "\n"))))
          "\n"
          )]
        [else (error "print-x86, unmatched" e)]
        ))
    
    ))

(define (print-x86 x86-ast)
  (define printer (new print-x86-class))
  (send printer print-x86 x86-ast))

(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

(define fix (lambda (f) (lambda (x) ((f (fix f)) x))))

(define (i2b i)
  (cond [(eq? i 0) #f]
        [else #t]))

(define (b2i b)
  (cond [b 1]
        [else 0]))

(define (map2 f ls)
  (cond [(null? ls)
         (values '() '())]
        [else
         (let-values ([(x1 x2) (f (car ls))]
                      [(ls1 ls2) (map2 f (cdr ls))])
           (values (cons x1 ls1) (cons x2 ls2)))]))

(define (ral-map2 f ls)
  (cond [(ral-empty? ls)
         (values '() '())]
        [else
         (let-values ([(x1 x2) (f (car ls))]
                      [(ls1 ls2) (map2 f (cdr ls))])
           (values (cons x1 ls1) (cons x2 ls2)))]))

(define (map3 f ls)
  (cond [(null? ls)
         (values '() '() '())]
        [else
         (let-values ([(x1 x2 x3) (f (car ls))]
                      [(ls1 ls2 ls3) (map3 f (cdr ls))])
           (values (cons x1 ls1) (cons x2 ls2) (cons x3 ls3)))]))


