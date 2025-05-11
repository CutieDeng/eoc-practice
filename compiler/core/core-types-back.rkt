#lang racket/base

(require racket/struct racket/dict racket/set racket/class racket/string racket/bool racket/function)
(require racket/port racket/system racket/list racket/contract/base)
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

(define (newline-and-indent port col)
  (let ([lead (if col (make-string col #\space) "")])
    (newline port)
    (write-string lead port)
    ))

(struct Value (val) #:transparent #:property prop:custom-print-quotable 'never)

(struct Decl (name type)
  #:transparent #:property prop:custom-print-quotable 'never)

(struct Poly (name type)
  #:transparent #:property prop:custom-print-quotable 'never)

(struct Inst (expr type types)
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
        
(define (print-info info port mode)
  (let ([recur (make-recur port mode)])
    (for ([(label data) (in-dict info)])
      (match label
        ['locals-types
          (write-string "locals-types:" port)
          (newline port)
          (cond 
            [(dict? data)
              (write-string "    " port)
              (for ([(var type) (in-dict data)])
                (write-string (symbol->string var) port)
                (write-string " : " port)
                (recur type port)
                (write-string ", " port)
              )
              (newline port)]
            [else
              (recur data port)
              (newline port)])]
        [else
          (write-string (symbol->string label) port)
          (write-string ":" port)
          (newline port)
          (recur data port)
          (newline port)
          ]
      ))))

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
    [(? symbol?) #t]
    [`(,x : ,t) (and (symbol? x) (type? t))]
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
  (cond 
    [(null? ls)
      (values '() '())]
    [else
      (let-values ([(x1 x2) (f (car ls))]
        [(ls1 ls2) (map2 f (cdr ls))])
        (values (cons x1 ls1) (cons x2 ls2)))]
  ))

(define (ral-map2 f ls)
  (for/fold ([x0 (ral-empty)] [x1 (ral-empty)])
    ([i (in-ral0 ls)])
    (define-values (v0 v1) (f i))
    (values (ral-consr x0 v0) (ral-consr x1 v1))
  )
)

(define (map3 f ls)
  (cond [(null? ls)
         (values '() '() '())]
        [else
          (let-values ([(x1 x2 x3) (f (car ls))]
            [(ls1 ls2 ls3) (map3 f (cdr ls))])
          (values (cons x1 ls1) (cons x2 ls2) (cons x3 ls3)))]))

(define (ral-map3 f ls)
  (for/fold ([x0 (ral-empty)] [x1 (ral-empty)] [x2 (ral-empty)])
    ([i (in-ral0 ls)])
    (define-values (v0 v1 v2) (f i))
    (values (ral-consr x0 v0) (ral-consr x1 v1) (ral-consr x2 v2))
  )
)

(define (bit-union* ls)
  (foldl bitwise-ior 0 ls)
)

(define (set-union* ls)
  (foldl set-union (set) ls))

(define (hash-union . hs)
  (for*/hash ([h (in-list hs)]
              [(k v) (in-hash h)])
    (values k v)))

;; label-name prepends an underscore to a label (symbol or string)
;; if the current system is Mac OS and leaves it alone otherwise.
;; label-name : (U String Symbol) -> String
(define (label-name name)
  (unless (or (symbol? name) (string? name))
    (error 'label-name "expected string or symbol got ~s" name))
  (cond
    [(symbol? name) (label-name (symbol->string name))]
    [(eqv? (system-type 'os) 'macosx) (string-append "_" name)]
    [else name]))

(define (fun-call? s)
  (or (eq? s 'app) (eq? s 'tailcall)))

(define (indirect-call? s)
  (or (symbol=? s 'indirect-callq)
      (symbol=? s 'tail-jmp)))

(define no-default (gensym))

(define fst
  (lambda (p)
    (cond [(pair? p)   (car p)]
          [(mpair? p)  (mcar p)]
          [else        (error 'fst "not a pair of any sort" p)])))

(define snd
  (lambda (p)
    (cond [(pair? p)   (cdr p)]
          [(mpair? p)  (mcdr p)]
          [else        (error 'snd "not a pair of any sort" p)])))

(define (pairish? p)
  (or (pair? p) (mpair? p)))

(define (lookup x ls [default no-default])
  (let recur ([xs ls])
    (cond
      [(Ordl xs) (dict-ref xs x)]
      [(ral? xs)
        (printf "meet ral\n")
      ]
      [(null? xs)
       (if (eq? default no-default)
           (error 'lookup "didn't find ~a in ~a" x (map fst ls))
           default)]
      [(pair? xs)
       (define curr (car xs))
       (cond
         [(and (pairish?  curr) (equal? x (fst  curr))) (snd  curr)]
         [(not (pairish? curr))
          (error 'lookup "expected pair in alist, not ~a" curr)]
         [else (recur (cdr xs))])]
      [else (error 'lookup "expected an alist, not ~a" ls)])))

(define (extend-env env x v)
  (cons (cons x v) env))

(define (read-fixnum)
  (define r (read))
  (cond [(fixnum? r) r]
	[else (error 'read "expected an integer")]))

;; Read an entire .rkt file wrapping the s-expressions in
;; a list whose head is 'program.
(define (read-program^ path)
  (unless (or (string? path) (path? path))
    (error 'read-program "expected a string in ~s" path))
  (unless (file-exists? path)
    (error 'read-program "file doesn't exist in ~s" path))
  (define input-prog
    (call-with-input-file path
      (lambda (f)
        `(program () ,(for/fold ([x (ral-empty)]) ([e (in-port read f)]) (ral-consr x e))))))
  (define parsed-prog (parse-program input-prog))
  parsed-prog)

;; Read an entire .rkt file wrapping the s-expressions in
;; a list whose head is 'program.
(define (read-program path)
  (unless (or (string? path) (path? path))
    (error 'read-program "expected a string in ~s" path))
  (unless (file-exists? path)
    (error 'read-program "file doesn't exist in ~s" path))
  (define input-prog
    (call-with-input-file path
      (lambda (f)
        `(program () ,@(for/list ([e (in-port read f)]) e)))))
  (define parsed-prog (parse-program input-prog))
  parsed-prog)

(define (make-dispatcher mt)
  (lambda (e . rest)
    (match e
       [`(,tag ,args ...)
	(apply (hash-ref mt tag) (append rest args))]
       [else
	(error "no match in dispatcher for " e)]
       )))

#;(define (strip-has-type e)
  e)

(define (strip-has-type e)
  (match e 
    [`(has-type ,e ,T)
     (strip-has-type e)]
    [`(,(app strip-has-type e*) ...)
      `(,@e*)]
    [else
     e]))

(define ((check-exception name test-name error-expected) fn)
  (with-handlers
    ([exn:fail? (lambda (exn)
      (cond 
        [error-expected 'expected-error]
        [else
          (displayln (format "encountered exception while testing '~a`, case ~a" name test-name))
          (raise exn)]
      ))])
    (let ([res (fn)])
      (when (and (not (string? res)) (not (pair? res)) (not (eq? res #f)))
        (check-false error-expected (format "in check-exception, expected exception, not ~a" res)))
      res)))

(define ((check-passes-suite name typechecker passes initial-interp) test-name)
  (test-suite test-name
    (let* ([input-file-name (format "./tests/~a.in" test-name)]
        [result-file-name (format "./tests/~a.res" test-name)]
        [program-name (format "./tests/~a.rkt" test-name)]
        [sexp (read-program program-name)]
        [type-error-expected (file-exists? (format "./tests/~a.tyerr" test-name))]
        [tsexp ((check-exception name test-name type-error-expected)
                (thunk (test-typecheck typechecker sexp)))]
        [error-expected (file-exists? (format "./tests/~a.err" test-name))]
        [checker (check-exception name test-name error-expected)])
      (test-case
        "typecheck"  
        (if type-error-expected 
          (check-false
            tsexp
            (format "expected type error in compiler '~a', case ~a, but no error raised by typechecker" name test-name))
          (check-not-false
            tsexp
            (format "expected no type error in compiler '~a', case ~a, but received error ~a" name test-name tsexp))))
      (unless type-error-expected
        (make-test-suite
          "passes"
          (let ([expected-result (cond 
            [initial-interp
              (if (file-exists? input-file-name)
                (with-input-from-file input-file-name
                  (lambda () (checker (thunk (initial-interp tsexp)))))
                (checker (thunk (initial-interp tsexp))))]
            [else 
              (cond 
                [(file-exists? result-file-name)
                  (call-with-input-file result-file-name
                  (lambda (f) (string->number (read-line f))))]
                [error-expected 'expected-error]
                [else 
                  42])])])
            (let loop ([passes passes]
              [p tsexp]
              [tests '()])
            (cond 
              [(null? passes) (reverse tests)]
              [else
                (define pass-info (car passes))
                (define pass-name (list-ref pass-info 0))
                (define pass      (list-ref pass-info 1))
                (define interp    (list-ref pass-info 2))
                (define type-checker
                  (cond 
                    [(>= (length pass-info) 4)
                      (list-ref pass-info 3)]
                    [else #f]))
                (define input p)
                (define new-p^ ((check-exception name test-name #f) (thunk (pass p))))
                (define new-p (cond 
                  [type-checker
                    (type-checker new-p^)]
                  [else new-p^]))
                (cond 
                  [interp
                    (define result
                      (if (file-exists? input-file-name) 
                        (with-input-from-file input-file-name
                          (lambda () (checker (thunk (interp new-p)))))
                        (checker (thunk (interp new-p)))))
                    (cond 
                      [expected-result
                        (loop (cdr passes) new-p 
                          (cons 
                            (test-suite
                              (string-append "pass " pass-name)
                              (check-equal?
                                result expected-result 
                                (format 
                                  "differing results in compiler '~a' on test '~a' pass '~a', expected ~a, not ~a"
                                  name test-name pass-name expected-result result)))
                            tests))]
                      [else
                        (loop (cdr passes) new-p tests)]
                    );; cond result
                  ]
                  [else
                    (loop (cdr passes) new-p tests)]
                ) ;; cond interp
              ]
            ))))))))

(define (compile passes)
  (let ([prog-file-name (vector-ref (current-command-line-arguments) 0)])
    ((compile-file passes) prog-file-name)))

(define (compile-file typechecker passes)
  (lambda (prog-file-name)
    (define file-base (string-trim prog-file-name ".rkt"))
    (define out-file-name (string-append file-base ".s"))
    (call-with-output-file
      out-file-name
      #:exists 'replace
      (lambda (out-file)
        (define sexp (read-program prog-file-name))
        (define tsexp (test-typecheck typechecker sexp))
        (if tsexp
          (let ([x86 (let loop ([passes passes] [p tsexp])
            (cond 
              [(null? passes) p]
              [else
                (define pass-info (car passes))
                (define name      (list-ref pass-info 0))
                (define pass      (list-ref pass-info 1))
                (define interp    (list-ref pass-info 2))
                (define type-checker
                  (cond 
                    [(>= (length pass-info) 4)
                      (list-ref pass-info 3)]
                    [else #f]))
                (define new-p^
                  ((check-exception name file-base #f)
                    (thunk (pass p))))
                (define new-p (cond 
                  [type-checker
                    (type-checker new-p^)]
                  [else new-p^]))
                (loop (cdr passes) new-p)
              ]))])
            (cond 
              [(string? x86)
                (error "error, compiler should produce x86 AST, not a string")
                (write-string x86 out-file)
                (newline out-file)
                (flush-output out-file)
                #t]
              [else
                #;(error "compiler did not produce x86 output")
                (define x86-str (print-x86 x86))
                (write-string x86-str out-file)
                (newline out-file)
                (flush-output out-file)
                #t])
          )
          #f)
      ))))

(define (interp-tests name typechecker passes initial-interp test-family test-nums)
  (run-tests (interp-tests-suite name typechecker passes initial-interp test-family test-nums) (void)))

(define (interp-tests-suite name typechecker passes initial-interp test-family test-nums)
  (define checker-suite (check-passes-suite name typechecker passes initial-interp))
  (make-test-suite
   "interpreter tests"
   (for/list ([test-number (in-list test-nums)])
     (let ([test-name (format "~a_~a" test-family test-number)])
       (checker-suite test-name)))))

(define (result-check res expected)
  (or
    (equal? res (string->number expected))
    (if (string? res)
      (string=? res expected)
      (string=? (number->string res) expected))
    (equal? (with-input-from-string (number->string res) read)
            (with-input-from-string expected read))
  ))

(define (wait-or-timeout control-fun maxsecs)
  (define (poll)    
    (match (control-fun 'status)
      ['running    #f]
      ['done-ok    'done-ok]
      ['done-error 'done-error]))
  (let loop ((slept 0) (delta 0.001)) ;; Start at 1ms    
    (define remain (- maxsecs slept))
    (cond
      [(>= slept maxsecs) 'timed-out]
      [(< remain delta)
       (sleep remain)
       (or (poll) 'timed-out)]
      [else
       (define slept2 (+ slept delta))
       (sleep delta)
       (or (poll) (loop slept2 (* 2 delta)))])))

(define (get-value-or-fail command output)
  (match (process command)
    [`(,in1 ,out ,_ ,inErr ,control-fun)
      (let* (
          [timeout 6.0]
          [res (wait-or-timeout control-fun timeout)]
          [result (cond 
            [(symbol=? res 'timed-out)
              `(error timed-out ,timeout)]
            [(symbol=? res 'done-error)
              `(error done-error ,(control-fun 'exit-code))]
            [else `(result done ,(read-line in1))])])
        (close-input-port in1)
        (close-input-port inErr)
        (close-output-port out)
        result)]))

(define (compiler-tests-suite name typechecker passes test-family test-nums)
  (let ([compiler (compile-file typechecker passes)])
    (make-test-suite
      "compiler tests"
      (for/list ([test-number (in-list test-nums)])
        (let* (
            [test-name (format "~a_~a" test-family test-number)]
            [type-error-expected (file-exists? (format "./tests/~a.tyerr" test-name))]
            [typechecks (compiler (format "./tests/~a.rkt" test-name))])
          (test-suite
            test-name
            (if type-error-expected
              (test-case "typecheck" (check-false typechecks "Expected expression to fail typechecking"))
              (if (not typechecks) 
                (fail "Expected expression to typecheck")
                (test-case "code generation"
                  (let ([gcc-cmd (if 
                      (and (equal? (system-type 'os) 'macosx)
                        (equal? (system-type 'arch) 'aarch64))
                      "gcc -g -std=c99 -arch x86_64"
                      "gcc -g -std=c99")])
                    (let ([gcc-output (system (format "~a runtime.o ./tests/~a.s -o ./tests/~a.out" gcc-cmd test-name test-name))])
                      (if (not gcc-output) 
                        (fail "Failed during assembly")
                        (let (
                            [input (if (file-exists? (format "./tests/~a.in" test-name))
                              (format " < ./tests/~a.in" test-name)
                              "")]
                            [output (if 
                              (file-exists? (format "./tests/~a.res" test-name))
                              (call-with-input-file
                                (format "./tests/~a.res" test-name)
                                (lambda (f) (read-line f)))
                              "42")]
                            [error-expected (file-exists? (format "./tests/~a.err" test-name))])
                          (let* (
                              [command (format "./tests/~a.out ~a" test-name input)]
                              [result (get-value-or-fail command output)])
                            (check-not-false gcc-output "Unable to run program, gcc reported assembly failure")
                            (check-not-equal? (cadr result) 'timed-out 
                              (format "x86 execution timed out after ~a seconds" (caddr result)))
                            (cond 
                              [error-expected
                                (check-equal? (cadr result) 'done-error (format "expected error, not: ~a" (caddr result)))
                                (check-equal? (caddr result) 255 (format "expected error, not: ~a" (caddr result)))]
                              [else
                                (check-not-eq? (cadr result) eof "x86 execution did not produce output")
                                (check result-check (caddr result) output "Mismatched output from x86 execution")])))))))))))))))

(define (compiler-tests name typechecker passes test-family test-nums)
  (run-tests (compiler-tests-suite name typechecker passes test-family test-nums) (void)))

(define (compiler-tests-gui name typechecker passes test-family test-nums)
  ;; Disable this for now to run on a linux server. -Jeremy
  #;(test/gui (compiler-tests-suite name typechecker passes test-family test-nums))
  (void)
  )

;; TODO: when a type checking error is not expected, we should
;;   always print the error message. The following instead prints
;;   the error message when debug level >= 1. -Jeremy
(define (test-typecheck tcer exp)
  (define (handler e)
    #f)
  (if (eq? tcer #f)
    exp
    (let (
        [res (with-handlers ([exn:fail? handler])
          (tcer exp))])
      (match res
        [#f #f]
        ;[(Program info body) res]
        [else res]
        ;[else exp]
        ))))

(define assert
  (lambda (msg b)
    (if (not b)
      (begin
        (display "ERROR: ")
        (display msg)
        (newline))
      (void))))

;; (case-> (symbol . -> . symbol) (string . -> . string))
(define (racket-id->c-id x)
  (define (->c-id-char c)
    (if (or (char<=? #\A c #\Z)
            (char<=? #\a c #\z)
            (char<=? #\0 c #\9))
        c
        #\_))
  (cond
    [(symbol? x) (string->symbol (racket-id->c-id (symbol->string x)))]
    [(string? x) (list->string (map ->c-id-char (string->list x)))]
    [else (error 'racket-id->c-id "expected string or symbol: ~v" x)]))

(define rootstack-reg 'r15)
(define reserved-registers (set 'rax 'r11 'r15 'rsp 'rbp))

(define general-registers (vector 'rcx 'rdx 'rsi 'rdi 'r8 'r9 'r10
                                  'rbx  'r12 'r13 'r14))

(define caller-save (set 'rax 'rcx 'rdx 'rsi 'rdi 'r8 'r9 'r10 'r11))
(define callee-save (set 'rsp 'rbp 'rbx 'r12 'r13 'r14 'r15))

(define arg-registers (void))
(define registers-for-alloc (void))

(define (use-minimal-set-of-registers! flag)
  (if flag
    (begin
      ;; need at least 2 arg-registers, see limit-functions -Jeremy
      (set! arg-registers (vector 'rdi 'rsi))
      (set! registers-for-alloc (vector  'rsi 'rdi 'rbx))
      ; for the example in the register allocation chapter 
      ;(set! registers-for-alloc (vector 'rcx 'rbx))
    )
    (begin
      ;; The following ordering is specified in the x86-64 conventions.
      (set! arg-registers (vector 'rdi 'rsi 'rdx 'rcx 'r8 'r9))
      (set! registers-for-alloc general-registers))))

(use-minimal-set-of-registers! #f)

(define (vector->set vec)
  (for/set ([v vec]) v))

(define (caller-save-for-alloc)
  (set-intersect (set-subtract caller-save reserved-registers)
                 (vector->set registers-for-alloc)))
(define (callee-save-for-alloc)
  (set-intersect (set-subtract callee-save reserved-registers)
                 (vector->set registers-for-alloc)))

(define byte-register-table
  (make-immutable-hash
   `((ah . rax) (al . rax)
     (bh . rbx) (bl . rbx)
     (ch . rcx) (cl . rcx)
     (dh . rdx) (dl . rdx))))

(define (byte-reg->full-reg x)
  (let ([r? (hash-ref byte-register-table x #f)])
    (unless r?
      (error 'byte-reg->full-reg "invalid byte register ~a" x))
    r?))

(define (reg-colors)
  (define register-colors '((rax . -1) (rsp . -2) (rbp . -3) (r11 . -4)
      (r15 . -5) (__flag . -6)))
  (for (
      [r registers-for-alloc]
      [i (in-naturals)])
    (set! register-colors (cons (cons r i) register-colors)))
  register-colors)
  
(define (register->color r)
  (cond 
    [(assq r (reg-colors)) => (lambda (p) (cdr p))]
    [else -1]))

(define (num-registers-for-alloc)
  (vector-length registers-for-alloc))

(define (color->register c)
  (vector-ref registers-for-alloc c))

(define registers (set-union (list->set (vector->list general-registers))
  (set 'rax 'r11 'r15 'rsp 'rbp '__flag)))

(define (align n alignment)
  (cond 
    [(eq? 0 (modulo n alignment)) n]
	[else
	  (+ n (- alignment (modulo n alignment)))]))

(define (print-by-type ty [depth 12])
  (define (mov-and-print depth) 
    (lambda (ty index)
      (format "\tmovq\t~a(%r~a), %rax\n~a" (* 8 (+ 1 index)) depth (print-by-type ty (+ 1 depth)))))
  (match ty
    ['Any 
      (format "\tmovq\t%rax, %rdi\n\tcallq\t~a\n" (label-name "print_any"))]
    ['Void (format "\tcallq\t~a\n" (label-name "print_void"))]
    ['Integer 
      (format "\tmovq\t%rax, %rdi\n\tcallq\t~a\n" (label-name "print_int"))]
    ['Boolean 
      (format "\tmovq\t%rax, %rdi\n\tcallq\t~a\n" (label-name "print_bool"))]
    [`(Vector ,tys ...)
      (if (> depth 15)
        (format "\tmovq\t%rax, %rdi\n\tcallq\t~a\n" (label-name "print_ellipsis"))
        (string-join (map (mov-and-print depth) tys (range (length tys)))
          (format "\tcallq\t~a\n" (label-name "print_space"))
          #:before-first (format "\tmovq\t%rax, %r~a\n\tcallq\t~a\n" depth (label-name "print_vecbegin"))
          #:after-last (format "\tcallq\t~a\n" (label-name "print_vecend"))))]))

(define (make-lets bs e)
  (match bs
    [`() e]
    #;[`((_ . ,e^) . ,bs^)
       (Seq e^ (make-lets bs^ e))]
    [`((_ . ,e^) . ,bs^)
     (Begin (list e^) (make-lets bs^ e))]
    [`((,x . ,e^) . ,bs^)
     (Let x e^ (make-lets bs^ e))]))

(define (dict-remove-all dict keys)
  (for/fold ([d dict]) ([k keys])
    (dict-remove d k)))

(define (dict-set-all dict key-vals)
  (for/fold ([d dict]) ([(k v) (in-dict key-vals)])
    (dict-set d k v)))

(define get-basic-blocks (make-parameter '()))

(define (goto-label label)
  (dict-ref (get-basic-blocks) label))

(define (symbol-append s1 s2)
  (string->symbol (string-append (symbol->string s1) (symbol->string s2))))

(define (any-tag ty)
  (match ty
    ['Integer 1]		;; 001
    [`(Vector ,ts ...) 2]	;; 010
    [`(PVector ,ts ...) 2]
    [`(,ts ... -> ,rt) 3]	;; 011
    ['Boolean 4]		;; 100
    ['Void 5]                   ;; 101
    [`(Vectorof ,t) 6]          ;; 110
    [else (error "in any-tag, unrecognized type" ty)]
    ))

(provide AST-output-syntax 
         map2 map3 b2i i2b
         racket-id->c-id 
         hash-union set-union*
         fix while 
         label-name lookup extend-env make-dispatcher assert
         fun-call? indirect-call?
         read-fixnum read-program 
	 compile compile-file check-passes-suite interp-tests compiler-tests
         compiler-tests-gui compiler-tests-suite
         use-minimal-set-of-registers!
	 general-registers num-registers-for-alloc registers-for-alloc
         caller-save callee-save caller-save-for-alloc callee-save-for-alloc
	 arg-registers rootstack-reg register->color color->register
         registers align byte-reg->full-reg print-by-type strip-has-type
         make-lets dict-set-all dict-remove-all goto-label get-basic-blocks 
         symbol-append any-tag parse-program vector->set atm? fst
         print-x86 print-x86-class
         
         (contract-out [struct Prim ((op symbol?) (arg* exp-list?))])
         (contract-out [struct Var ((id integer?))])
         (contract-out [struct Int ((value fixnum?))])
         (contract-out [struct Let ((var integer?) (rhs exp?) (body exp?))])
         (struct-out Program)
         (struct-out ProgramDefsExp)
         (struct-out ProgramDefs)
         (struct-out CProgram)
         (struct-out X86Program)
         (struct-out X86ProgramDefs)
         (contract-out [struct WhileLoop ((cnd exp?) (body exp?))])
         (contract-out [struct SetBang ((var symbol?) (rhs exp?))])
         (contract-out [struct GetBang ((var symbol?))])
         (contract-out [struct Begin ((es exp-list?) (body exp?))])
         (contract-out [struct Bool ((value boolean?))])
         (contract-out [struct If ((cnd exp?) (thn exp?) (els exp?))])
         (contract-out [struct HasType ((expr exp?) (type type?))])
         (contract-out [struct UncheckedCast ((expr exp?) (type type?))])
         (struct-out Void)
         (contract-out [struct StructDef ((name symbol?) (field* param-list?))])
         (contract-out [struct Apply ((fun exp?) (arg* exp-list?))])
         (contract-out [struct Def ((name symbol?) (param* param-list?) (rty type?) (info any?)
                                                   (body any?))])
         (contract-out [struct Lambda ((param* param-list?) (rty type?) (body exp?))])
         (contract-out [struct FunRef ((name symbol?) (arity fixnum?))])
         (struct-out Inject)
         (struct-out Project)
         (struct-out ValueOf)
         (contract-out [struct Cast ((expr exp?) (source type?) (target type?))])
         (struct-out Value)
         (contract-out [struct Decl ((name symbol?) (type type?))])
         (struct-out Poly)
         (contract-out [struct Inst ((expr exp?)
                                     (type type?)
                                     (types type-list?))])

         
         #;(struct-out TagOf)
         (struct-out Closure)
         (struct-out AssignedFree)
           
         (contract-out [struct Assign ((lhs lhs?) (rhs exp?))])
         (contract-out [struct Seq ((seq* ral?) (end stmt?))])
         (contract-out [struct Return ((arg exp?))])
         (contract-out [struct IfStmt ((cnd cmp?) (thn goto?) (els goto?))])
         (contract-out [struct Goto ((label symbol?))])
         (struct-out Collect)
         (struct-out CollectionNeeded?)
         (struct-out GlobalValue)
         (struct-out Allocate)
         (struct-out AllocateArray)
         (struct-out AllocateClosure)
         (struct-out AllocateProxy)
         (contract-out [struct Call ((fun atm?) (arg* atm-list?))])
         (contract-out [struct TailCall ((fun atm?) (arg* atm-list?))])
         
         (contract-out [struct Imm ((value integer?))]) ;; allow 64-bit
         (contract-out [struct Reg ((name symbol?))])
         (contract-out [struct Deref ((reg symbol?) (offset fixnum?))])
         (contract-out [struct Instr ((name symbol?) (arg* arg-list?))])
         (contract-out [struct Callq ((target symbol?) (arity fixnum?))])
         (contract-out [struct IndirectCallq ((target arg?) (arity fixnum?))])
         (contract-out [struct IndirectJmp ((target arg?))])
         (struct-out Retq)
         (contract-out [struct Jmp ((target symbol?))])
         (contract-out [struct TailJmp ((target arg?) (arity fixnum?))])
         (contract-out [struct Block ((info any?) (instr* instr-list?))])
         ; (contract-out [struct SSABlock ((info any?) (ssa-defs dict?) (instr* instr-list?))])
         (struct-out StackArg)
         (struct-out Global)

         (contract-out [struct JmpIf ((cnd symbol?) (target symbol?))])
         (contract-out [struct ByteReg ((name symbol?))])
         
         (struct-out Tagged)
         (struct-out Function)
         (struct-out CFunction)
         (struct-out X86Function)
         
         )
