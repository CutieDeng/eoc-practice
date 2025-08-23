#lang racket/base

(struct NodeId (id) #:transparent)
; input, output is viewed by wire
(struct InputId (id) #:transparent)
(struct OutputId (id) #:transparent)

(struct WireId (id) #:transparent)

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

(struct NodeInfo (info) #:transparent)

(struct Simple NodeInfo (op) #:transparent)

(struct Gamma NodeInfo (region*) #:transparent)
(struct Theta NodeInfo (region) #:transparent)

(struct Lambda NodeInfo (region) #:transparent)
(struct Delta NodeInfo (region) #:transparent)
(struct Phi NodeInfo (region) #:transparent)

(struct Omega NodeInfo (region) #:transparent)

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

(provide (struct-out NodeInfo))
