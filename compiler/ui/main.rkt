#lang racket/gui

(define-struct node (x y width height label connections) #:mutable)
(define-struct connection (from-node from-port to-node to-port))

(define nodes '())
(define connections '())
(define selected-node #f)
(define mouse-down? #f)

(define (draw-node dc node)
  (send dc set-pen "black" 2 'solid)
  (send dc set-brush "white" 'transparent)
  (send dc draw-rectangle (node-x node) (node-y node) (node-width node) (node-height node))
  (send dc draw-text (node-label node) (+ (node-x node) 10) (+ (node-y node) 10)))

(define (draw-connection dc conn)
  (send dc set-pen "blue" 2 'solid)
  (let* ((from-node (connection-from-node conn))
         (to-node (connection-to-node))
         (from-x (+ (node-x from-node) (node-width from-node)))
         (from-y (+ (node-y from-node) (/ (node-height from-node) 2)))
         (to-x (node-x to-node))
         (to-y (+ (node-y to-node) (/ (node-height to-node) 2))))
    (send dc draw-line from-x from-y to-x to-y)))

(define (draw-canvas canvas dc)
  (for-each (lambda (node) (draw-node dc node)) nodes)
  (for-each (lambda (conn) (draw-connection dc conn)) connections))

(define (create-node x y label)
  (let ((node (make-node x y 100 50 label '())))
    (set! nodes (cons node nodes))
    node))

(define (get-node-at x y)
  (for/or ([node nodes])
    (and (<= (node-x node) x (+ (node-x node) (node-width node)))
         (<= (node-y node) y (+ (node-y node) (node-height node)))
         node)))

(define frame (new frame% [label "cutie:rvsdg:compiler"] [width 800] [height 600]))

(define Canvas0 (class canvas%
  (super-new)
  (define/override (on-event mouse-event)
    (super on-event mouse-event)
    (printf "ev\n") 
    (printf "m? ~a\n" ((is-a?/c mouse-event%) mouse-event))
    (when (not (send mouse-event get-control-down))
      ((handle-mouse-event canvas) mouse-event))
  )
))

(define canvas (new Canvas0 [parent frame] [paint-callback draw-canvas]))

(define ((handle-mouse-event canvas) event)
  (let* [(x (send event get-x))
         (y (send event get-y))
         (type (send event get-event-type))]
    (case type
      [(motion) (when selected-node
                  (set-node-x! selected-node (- x (/ (node-width selected-node) 2)))
                  (set-node-y! selected-node (- y (/ (node-height selected-node) 2)))
                  (send canvas refresh))]
      [(left-down) (set! selected-node (get-node-at x y))
                   (set! mouse-down? #t) (printf "left down\n")]
      [(left-up) (when (and selected-node mouse-down?)
                  (let ((target-node (get-node-at x y)))
                    (when (and target-node (not (eq? selected-node target-node)))
                      (set! connections (cons (make-connection selected-node 'output target-node 'input) connections))))
                  (set! selected-node #f)
                  (set! mouse-down? #f)
                  (send canvas refresh)) (printf "left up\n")])))

(define (add-node-button)
  (new button% [parent frame] [label "Add Node"] [callback (lambda (b e) (create-node 100 100 "Node"))]))

(add-node-button)

(send frame show #t)

