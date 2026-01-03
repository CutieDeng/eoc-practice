#lang racket/base

;; ============================================================
;; Driver: Worklist Algorithms
;; ============================================================
;;
;; Generic worklist data structures and algorithms.
;; These provide efficient iteration strategies for
;; dataflow analysis and other fixed-point computations.
;;
;; ============================================================

(require racket/list)
(require racket/set)
(require "../../kernel/data/main.rkt")

(provide
  ;; Worklist types
  make-fifo-worklist
  make-lifo-worklist
  make-priority-worklist

  ;; Worklist operations
  worklist-empty?
  worklist-add!
  worklist-add-all!
  worklist-remove!
  worklist-contains?

  ;; Iteration
  worklist-iterate
  worklist-iterate-with-state)

;; ============================================================
;; Worklist Structures
;; ============================================================

;; Abstract worklist interface:
;;   - empty?   : -> boolean
;;   - add!     : item -> void
;;   - remove!  : -> item
;;   - member?  : item -> boolean

(struct FifoWorklist (queue members) #:mutable)
(struct LifoWorklist (stack members) #:mutable)
(struct PriorityWorklist (heap members priority-fn) #:mutable)

;; ============================================================
;; FIFO Worklist (Queue-based)
;; ============================================================

(define (make-fifo-worklist [initial '()])
  (define members (mutable-set))
  (for ([item initial])
    (set-add! members item))
  (FifoWorklist initial members))

(define (fifo-empty? wl)
  (null? (FifoWorklist-queue wl)))

(define (fifo-add! wl item)
  (unless (set-member? (FifoWorklist-members wl) item)
    (set-add! (FifoWorklist-members wl) item)
    (set-FifoWorklist-queue! wl
      (append (FifoWorklist-queue wl) (list item)))))

(define (fifo-remove! wl)
  (define queue (FifoWorklist-queue wl))
  (define item (car queue))
  (set-FifoWorklist-queue! wl (cdr queue))
  (set-remove! (FifoWorklist-members wl) item)
  item)

(define (fifo-member? wl item)
  (set-member? (FifoWorklist-members wl) item))

;; ============================================================
;; LIFO Worklist (Stack-based)
;; ============================================================

(define (make-lifo-worklist [initial '()])
  (define members (mutable-set))
  (for ([item initial])
    (set-add! members item))
  (LifoWorklist initial members))

(define (lifo-empty? wl)
  (null? (LifoWorklist-stack wl)))

(define (lifo-add! wl item)
  (unless (set-member? (LifoWorklist-members wl) item)
    (set-add! (LifoWorklist-members wl) item)
    (set-LifoWorklist-stack! wl
      (cons item (LifoWorklist-stack wl)))))

(define (lifo-remove! wl)
  (define stack (LifoWorklist-stack wl))
  (define item (car stack))
  (set-LifoWorklist-stack! wl (cdr stack))
  (set-remove! (LifoWorklist-members wl) item)
  item)

(define (lifo-member? wl item)
  (set-member? (LifoWorklist-members wl) item))

;; ============================================================
;; Priority Worklist
;; ============================================================

(define (make-priority-worklist priority-fn [initial '()])
  (define members (mutable-set))
  (for ([item initial])
    (set-add! members item))
  ;; Sort initial list by priority (lower = higher priority)
  (define sorted (sort initial < #:key priority-fn))
  (PriorityWorklist sorted members priority-fn))

(define (priority-empty? wl)
  (null? (PriorityWorklist-heap wl)))

(define (priority-add! wl item)
  (unless (set-member? (PriorityWorklist-members wl) item)
    (set-add! (PriorityWorklist-members wl) item)
    (define pf (PriorityWorklist-priority-fn wl))
    (define heap (PriorityWorklist-heap wl))
    ;; Insert in sorted order
    (set-PriorityWorklist-heap! wl
      (let insert ([items heap])
        (cond
          [(null? items) (list item)]
          [(< (pf item) (pf (car items)))
           (cons item items)]
          [else
           (cons (car items) (insert (cdr items)))])))))

(define (priority-remove! wl)
  (define heap (PriorityWorklist-heap wl))
  (define item (car heap))
  (set-PriorityWorklist-heap! wl (cdr heap))
  (set-remove! (PriorityWorklist-members wl) item)
  item)

(define (priority-member? wl item)
  (set-member? (PriorityWorklist-members wl) item))

;; ============================================================
;; Generic Operations (Dispatch based on type)
;; ============================================================

(define (worklist-empty? wl)
  (cond
    [(FifoWorklist? wl) (fifo-empty? wl)]
    [(LifoWorklist? wl) (lifo-empty? wl)]
    [(PriorityWorklist? wl) (priority-empty? wl)]
    [else (error 'worklist-empty? "unknown worklist type")]))

(define (worklist-add! wl item)
  (cond
    [(FifoWorklist? wl) (fifo-add! wl item)]
    [(LifoWorklist? wl) (lifo-add! wl item)]
    [(PriorityWorklist? wl) (priority-add! wl item)]
    [else (error 'worklist-add! "unknown worklist type")]))

(define (worklist-add-all! wl items)
  (for ([item items])
    (worklist-add! wl item)))

(define (worklist-remove! wl)
  (cond
    [(FifoWorklist? wl) (fifo-remove! wl)]
    [(LifoWorklist? wl) (lifo-remove! wl)]
    [(PriorityWorklist? wl) (priority-remove! wl)]
    [else (error 'worklist-remove! "unknown worklist type")]))

(define (worklist-contains? wl item)
  (cond
    [(FifoWorklist? wl) (fifo-member? wl item)]
    [(LifoWorklist? wl) (lifo-member? wl item)]
    [(PriorityWorklist? wl) (priority-member? wl item)]
    [else (error 'worklist-contains? "unknown worklist type")]))

;; ============================================================
;; Iteration
;; ============================================================

;; Iterate over worklist, calling process for each item
;; process : item -> (listof item) - returns items to add
;;
(define (worklist-iterate wl process)
  (let loop ()
    (unless (worklist-empty? wl)
      (define item (worklist-remove! wl))
      (define new-items (process item))
      (worklist-add-all! wl new-items)
      (loop))))

;; Iterate with state
;; process : item state -> (values state (listof item))
;;
(define (worklist-iterate-with-state wl init-state process)
  (let loop ([state init-state])
    (if (worklist-empty? wl)
        state
        (let ()
          (define item (worklist-remove! wl))
          (define-values (new-state new-items) (process item state))
          (worklist-add-all! wl new-items)
          (loop new-state)))))
