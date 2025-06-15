#lang racket/base

(require racket/struct racket/dict racket/match)

(require cutie-ftree)

(struct Region (node-cnt input-cnt output-cnt nodes input-src output-dsts input-loc output-loc 
  region-parent-id) #:transparent)


