#lang racket/base

(require racket/struct racket/dict racket/set racket/class racket/string racket/bool racket/function)
(require racket/port racket/system racket/list racket/contract/base)
(require racket/pretty racket/match)
(require (for-syntax racket))
(require rackunit rackunit/text-ui)

(require cutie-ftree)

(struct Var (id) #:transparent)
(struct Int (v) #:transparent)
(struct Var:r (name) #:transparent)
(struct Let (x e body) #:transparent)
(struct WhileLoop (cnd body) #:transparent)
(struct SetBang (var rhs) #:transparent)
(struct GetBang (var) #:transparent)
(struct Begin (es body) #:transparent)
(struct Bool (v) #:transparent)
(struct If (cnd thn els) #:transparent)
(struct Void () #:transparent)

(struct Program (info body) #:transparent)
(struct ProgramDefsExp (info def* body) #:transparent)
(struct ProgramDefs (info def*) #:transparent)
(struct CProgram (info blocks) #:transparent)

(struct RvsdgGraph (info graph) #:transparent)

(define (exp? e) #t)
(define (exps? e) (ral? e))

(provide 
  (contract-out [struct Var ((id integer?))])
  (contract-out [struct Var:r ((name string?))])
  (contract-out [struct Int ((v integer?))])
  (contract-out [struct Let ((x integer?) (e exp?) (body exp?))])
  (contract-out [struct WhileLoop ((cnd exp?) (body exp?))])
  (contract-out [struct SetBang ((var integer?) (rhs exp?))])
  (contract-out [struct GetBang ((var integer?))])
  (contract-out [struct Begin ((es exps?) (body exp?))])
  (contract-out [struct Bool ((v boolean?))])
  (contract-out [struct If ((cnd exp?) (thn exp?) (els exp?))])
  (struct-out Void)
  (struct-out Program)
  (struct-out ProgramDefsExp)
  (struct-out ProgramDefs)
  (struct-out CProgram)

)