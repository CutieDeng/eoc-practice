#lang racket/base

(require racket/contract/base)
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
(provide (contract-out (struct Var:r ([name symbol?])))) 

(struct Int (v) #:transparent)
(provide (contract-out (struct Int ([v integer?]))))

(struct Let (x e body) #:transparent)
(provide (contract-out (struct Let ([x (or/c integer? symbol?)] [e exp?] [body exp?]))))

(struct WhileLoop (cnd body) #:transparent)
(provide (contract-out (struct WhileLoop ([cnd exp?] [body exp?]))))

(struct SetBang (var rhs) #:transparent)
(provide (contract-out (struct SetBang ([var (or/c integer? symbol?)] [rhs exp?]))))

(struct GetBang (var) #:transparent)
(provide (contract-out (struct GetBang ([var (or/c integer? symbol?)]))))

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
(provide (contract-out (struct Prim ([op symbol?] [arg* list?]))))

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

(struct GlobalValue (name) #:transparent)
(provide (struct-out GlobalValue))

(struct Global (name) #:transparent)
(provide (struct-out Global))

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

(struct HasType (expr type) #:transparent)
(provide (struct-out HasType))

(struct UncheckedCast (expr type) #:transparent)
(provide (struct-out UncheckedCast))

(struct Collect (size) #:transparent)
(provide (struct-out Collect))

(struct CollectionNeeded? (size) #:transparent)
(provide (struct-out CollectionNeeded?))

(struct TailCall (fun arg*) #:transparent)
(provide (struct-out TailCall))

(struct Program (info body) #:transparent)
(struct ProgramDefsExp (info def* body) #:transparent)
(struct ProgramDefs (info def*) #:transparent)

(provide 
  (struct-out Program)
  (struct-out ProgramDefsExp)
  (struct-out ProgramDefs)
)

(define exp? 
  (or/c Int? Bool? Void? Var? Var:r?
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
