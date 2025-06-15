#lang racket

(require racket/draw)

(define target (make-bitmap 30 30))
(define dc (new bitmap-dc% [bitmap target]))

(send dc draw-rectangle 0 10 30 10)
(send dc draw-line 0 0 30 30)
(send dc draw-line 0 30 30 0)

(send target save-file "box.png" 'png)

