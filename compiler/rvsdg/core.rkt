#lang racket/base

(require racket/struct racket/dict racket/match racket/class)

(require cutie-ftree)

(define (string-compare lhs rhs)
  (cond
    [(string<? lhs rhs) '<] 
    [(string=? lhs rhs) '=]
    [else '>]
  )
)

(struct NodeId (id) #:transparent)
; input, output is viewed by wire
(struct InputId (id) #:transparent)
(struct OutputId (id) #:transparent)

(struct WireId (id) #:transparent)

(define (((abstract-compare-generator compare) map) lhs rhs)
  (compare (map lhs) (map rhs))
)

(define integer-compare-generator (abstract-compare-generator integer-compare))

(define wire-compare (integer-compare-generator WireId-id))
(define input-compare (integer-compare-generator InputId-id))
(define output-compare (integer-compare-generator OutputId-id))
(define node-compare (integer-compare-generator NodeId-id))

(define INPUT-NODE-ID 0)
(define OUTPUT-NODE-ID 1)
(define START-NODE-CNT 2)

(struct Region (
  wire->input
  wire->output
  input->wire
  input->node
  output->wire
  output->node
  node->input
  node->output
  node->value
  wire-cnt
  input-cnt
  output-cnt
  node-cnt
))

(define Region-empty-instance (Region
  (ordl-make-empty wire-compare)
  (ordl-make-empty wire-compare)
  (ordl-make-empty input-compare)
  (ordl-make-empty input-compare)
  (ordl-make-empty output-compare)
  (ordl-make-empty output-compare)
  (ordl-make-empty node-compare)
  (ordl-make-empty node-compare)
  (ordl-make-empty node-compare)
  0
  0
  0
  START-NODE-CNT
))

(define (Region-empty) Region-empty-instance)

(struct Simple (op) #:transparent)

(struct Gamma (region*) #:transparent)
(struct Theta (region) #:transparent)

(struct Lambda (region) #:transparent)
(struct Delta (region) #:transparent)
(struct Phi (region) #:transparent)

(struct Omega (region) #:transparent)

(module+ test
  (define x (Gamma (Region-empty)))
  x
)

(define (connect-io region input-id output-id)
  (define region^ (cancel-output/maybe (cancel-input/maybe region input-id) output-id))
  (connect-io-raw region^ input-id output-id)
)

(define (cancel-input-raw region update-region wire input-id)
  (define input->wire^ (dict-remove (Region-input->wire region) input-id))
  (define wire->input^ (dict-remove (Region-wire->input region) wire))
  (struct-copy Region update-region [input->wire input->wire^] [wire->input wire->input^])
)

(define (cancel-output-raw region update-region wire output-id)
  (define output->wire^ (dict-remove (Region-output->wire region) output-id))
  (define wire->output^ (dict-remove (Region-wire->output region) wire))
  (struct-copy Region update-region [output->wire output->wire^] [wire->output wire->output^])
)

(define (cancel-wire region wire)
  (define input-id (dict-ref (Region-wire->input region wire)))
  (define output-id (dict-ref (Region-wire->output region wire)))
  (define region^ (cancel-input-raw region region wire input-id))
  (define region^^ (cancel-output-raw region region^ wire output-id))
  region^^
)

(define (cancel-input/maybe region input-id)
  (define wire (dict-ref (Region-input->wire region) input-id #f)) 
  (define region^ (if wire (cancel-wire region wire) region)) 
  region^
)

(define (cancel-output/maybe region output-id)
  (define wire (dict-ref (Region-output->wire region) output-id #f))
  (define region^ (if wire (cancel-wire region wire) region))
  region^
)

(define (connect-io-raw region input-id output-id)
  (define wire-id-v (Region-wire-cnt region))
  (define wire (WireId wire-id-v))
  (define input->wire^ (dict-set (Region-input->wire region) input-id wire))
  (define wire->input^ (dict-set (Region-wire->input region) wire input-id))
  (define output->wire^ (dict-set (Region-output->wire region) output-id wire))
  (define wire->output^ (dict-set (Region-wire->output region) wire output-id))
  (struct-copy Region region
    [wire-cnt (add1 wire-id-v)]
    [input->wire input->wire^]
    [wire->input wire->input^]
    [output->wire output->wire^]
    [wire->output wire->output^]
  )
)

; ignore the node-id origin input
(define create-node-input-cnt-init create-node-input-cnt-raw)

; ignore the node-id origin input
(define (create-node-input-cnt-raw region node-id input-cnt)
  (define input-v (Region-input-cnt region)) 
  (define input-id (InputId input-v))
  (define input-cnt^ (+ input-v input-cnt))
  (define node->input^ (dict-set (Region-node->input region) node-id (cons input-id input-cnt)))
  (define input->node^ (dict-set (Region-input->node region) input-id node-id))
  (define region^ (struct-copy Region region
    [input-cnt input-cnt^]
    [node->input node->input^]
    [input->node input->node^]
  ))
  (values region^ input-id)
)

(define (create-node-input-renew region node-id input-cnt)
  (match-define (cons old-input-id len) (dict-ref (Region-node->input region) node-id))
  (define node->input^ (dict-remove (Region-node->input region) node-id))
  (define input->node^ (if (zero? len) 
    (Region-input->node region)
    (dict-remove (Region-input->node region) old-input-id)))
  (define region^ (struct-copy Region region
    [node->input node->input^]
    [input->node input->node^]
  ))
  (create-node-input-cnt-raw region^ node-id input-cnt)
)

; ignore the node-id origin output
(define create-node-output-cnt-init create-node-output-cnt-raw)

; ignore the node-id origin output
(define (create-node-output-cnt-raw region node-id output-cnt)
  (define output-v (Region-output-cnt region)) 
  (define output-id (OutputId output-v))
  (define output-cnt^ (+ output-v output-cnt))
  (define node->output^ (dict-set (Region-node->output region) node-id (cons output-id output-cnt)))
  (define output->node^ (dict-set (Region-output->node region) output-id node-id))
  (define region^ (struct-copy Region region
    [output-cnt output-cnt^]
    [node->output node->output^]
    [output->node output->node^]
  ))
  (values region^ output-id)
)

(define (create-node-output-renew region node-id output-cnt)
  (match-define (cons old-output-id len) (dict-ref (Region-node->output region) node-id))
  (define node->output^ (dict-remove (Region-node->output region) node-id))
  (define output->node^ (if (zero? len) 
    (Region-output->node region)
    (dict-remove (Region-output->node region) old-output-id)))
  (define region^ (struct-copy Region region
    [node->output node->output^]
    [output->node output->node^]
  ))
  (create-node-output-cnt-raw region^ node-id output-cnt)
)

(define (create-node region input-cnt output-cnt)
  (define node-id-v (Region-node-cnt region))
  (define node-id (NodeId node-id-v))
  (define node-cnt^ (add1 node-id-v))
  (define region^ (struct-copy Region region
    [node-cnt node-cnt^]))
  (match-define-values (region^^ input-id) (create-node-input-cnt-init region^ input-cnt))
  (match-define-values (region^^^ output-id) (create-node-output-cnt-init region^^ output-cnt))
  (values region^^^ node-id input-id output-id)
)

(define (set-node-value region node-id value)
  (define node->value^ (dict-set (Region-node->value region) node-id value))
  (struct-copy Region region
    [node->value node->value^])
)

(define (create-region input-cnt output-cnt)
  (define region (Region-empty))
  (match-define-values (region^ (InputId 0)) (create-node-input-cnt-init region (NodeId 0) input-cnt))
  (match-define-values (region^^ (OutputId _)) (create-node-output-cnt-init region^ (NodeId 0) 0))
  (match-define-values (region^^^ (InputId _)) (create-node-input-cnt-init region^^ (NodeId 1) 0))
  (match-define-values (region^^^^ (OutputId 0)) (create-node-output-cnt-init region^^^ (NodeId 1) output-cnt))
  region^^^^
)

(define (region-change-node-sz region nodeid new-input-cnt new-output-cnt)
  (void)
)

(define (region-mv-wire-input-raw region old-input old-wire new-input)
  (define input->wire^ (dict-remove (Region-input->wire region) old-input))
  (define input->wire^^ (dict-set input->wire^ new-input old-wire))
  (define wire->input^ (dict-set (Region-wire->input region) old-wire new-input)) 
  (struct-copy Region region
    [input->wire input->wire^^]
    [wire->input wire->input^]
  )
)

(define (region-mv-wire-output-raw region old-output old-wire new-output)
  (define output->wire^ (dict-remove (Region-output->wire region) old-output))
  (define output->wire^^ (dict-set output->wire^ new-output old-wire))
  (define wire->output^ (dict-set (Region-wire->output region) old-wire new-output)) 
  (struct-copy Region region
    [output->wire output->wire^^]
    [wire->output wire->output^]
  )
)

(define (region-alloc-input-raw region input-cnt)
  (define old-input-cnt (Region-input-cnt region))
  (define input-cnt^ (+ old-input-cnt input-cnt))
  (define region^ (struct-copy Region region [input-cnt input-cnt^]))
  (values region^ (InputId old-input-cnt))
)

(define (region-alloc-output-raw region output-cnt)
  (define old-output-cnt (Region-output-cnt region))
  (define output-cnt^ (+ old-output-cnt output-cnt))
  (define region^ (struct-copy Region region [output-cnt output-cnt^]))
  (values region^ (OutputId old-output-cnt))
)

; (> new-input-cnt (input-cnt nodeid))
(define (region-change-node-input region nodeid new-input-cnt)
  (match-define (cons startup len) (dict-ref (Region-node->input region) nodeid))
  (match-define-values (region^ input^) (region-alloc-input-raw region new-input-cnt))
  (define input->node^ (dict-remove (Region-input->node region^) startup))
  (define node->input^ (dict-remove (Region-node->input region^) nodeid))
  (define node->input^^ (dict-set node->input^ nodeid (cons input^ new-input-cnt)))
  (define-values (wire->input^ input->wire^)
    (for/fold ([wire->input^ (Region-wire->input region^)] [input->wire^ (Region-input->wire region^)])
      ([i (in-range len)])
        (define wire-id (dict-ref (Region-input->wire region^) (InputId (+ (InputId-id startup) i)) #f))
        (cond [wire-id
          (define input-id^ (InputId (+ (InputId-id input^) i)))
          (define wire->input^^ (dict-remove wire->input^ wire-id))
          (define wire->input^^^ (dict-set wire->input^^ wire-id input-id^))
          (define input->wire^^ (dict-remove input->wire^ (InputId (+ (InputId-id startup) i))))
          (define input->wire^^^ (dict-set input->wire^^ input-id^ wire-id))
          (values wire->input^^^ input->wire^^^)
        ][else (values wire->input^ input->wire^)])
    ))
  (struct-copy Region region
    [input->node input->node^]
    [node->input node->input^^]
    [wire->input wire->input^]
    [input->wire input->wire^]
  )
)

(provide (struct-out Region))

(provide (struct-out NodeId))
(provide (struct-out InputId))
(provide (struct-out OutputId))
(provide (struct-out WireId))

(provide (struct-out Simple))

(provide (struct-out Gamma))
(provide (struct-out Theta))

(provide (struct-out Lambda))
(provide (struct-out Delta))
(provide (struct-out Phi))

(provide (struct-out Omega))

(provide INPUT-NODE-ID OUTPUT-NODE-ID START-NODE-CNT)

(provide connect-io cancel-wire cancel-input/maybe cancel-output/maybe create-node set-node-value)
(provide create-node-input-renew create-node-output-renew create-region)
