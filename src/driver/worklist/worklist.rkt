#lang racket/base

;; ============================================================
;; Driver: Worklist Algorithms
;; ============================================================
;;
;; Generic worklist data structures and algorithms.
;; These provide efficient iteration strategies for
;; dataflow analysis and other fixed-point computations.
;; Uses pvector and ordered-map for internal data structures.
;;
;; ============================================================

(require "../../kernel/data/data.rkt")

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

;; Using boxes to hold mutable state with immutable data structures
(struct FifoWorklist (item-compare queue-box members-box) #:mutable)
(struct LifoWorklist (item-compare stack-box members-box) #:mutable)
(struct PriorityWorklist (item-compare heap-box members-box priority-fn) #:mutable)

;; ============================================================
;; FIFO Worklist (Queue-based)
;; ============================================================

(define (make-fifo-worklist item-compare [initial (pvector-empty)])
  (unless (pvector? initial)
    (raise-argument-error 'make-fifo-worklist "pvector?" initial))
  (define members
    (for/fold ([m (ordered-map-empty item-compare)])
              ([item (in-pvector initial)])
      (ordered-map-set m item #t)))
  (FifoWorklist item-compare (box initial) (box members)))

(define (fifo-empty? wl)
  (pvector-empty? (unbox (FifoWorklist-queue-box wl))))

(define (fifo-add! wl item)
  (define members (unbox (FifoWorklist-members-box wl)))
  (unless (ordered-map-has-key? members item)
    (set-box! (FifoWorklist-members-box wl)
              (ordered-map-set members item #t))
    (set-box! (FifoWorklist-queue-box wl)
              (pvector-cons-right (unbox (FifoWorklist-queue-box wl)) item))))

(define (fifo-remove! wl)
  (define queue (unbox (FifoWorklist-queue-box wl)))
  (define-values (item rest) (pvector-pop-left queue))
  (set-box! (FifoWorklist-queue-box wl) rest)
  (define members (unbox (FifoWorklist-members-box wl)))
  (define-values (new-members _) (ordered-map-delete members item))
  (set-box! (FifoWorklist-members-box wl) new-members)
  item)

(define (fifo-member? wl item)
  (ordered-map-has-key? (unbox (FifoWorklist-members-box wl)) item))

;; ============================================================
;; LIFO Worklist (Stack-based)
;; ============================================================

(define (make-lifo-worklist item-compare [initial (pvector-empty)])
  (unless (pvector? initial)
    (raise-argument-error 'make-lifo-worklist "pvector?" initial))
  (define members
    (for/fold ([m (ordered-map-empty item-compare)])
              ([item (in-pvector initial)])
      (ordered-map-set m item #t)))
  (LifoWorklist item-compare (box initial) (box members)))

(define (lifo-empty? wl)
  (pvector-empty? (unbox (LifoWorklist-stack-box wl))))

(define (lifo-add! wl item)
  (define members (unbox (LifoWorklist-members-box wl)))
  (unless (ordered-map-has-key? members item)
    (set-box! (LifoWorklist-members-box wl)
              (ordered-map-set members item #t))
    (set-box! (LifoWorklist-stack-box wl)
              (pvector-cons-left (unbox (LifoWorklist-stack-box wl)) item))))

(define (lifo-remove! wl)
  (define stack (unbox (LifoWorklist-stack-box wl)))
  (define-values (item rest) (pvector-pop-left stack))
  (set-box! (LifoWorklist-stack-box wl) rest)
  (define members (unbox (LifoWorklist-members-box wl)))
  (define-values (new-members _) (ordered-map-delete members item))
  (set-box! (LifoWorklist-members-box wl) new-members)
  item)

(define (lifo-member? wl item)
  (ordered-map-has-key? (unbox (LifoWorklist-members-box wl)) item))

;; ============================================================
;; Priority Worklist
;; ============================================================

(define (make-priority-worklist item-compare priority-fn [initial (pvector-empty)])
  (unless (pvector? initial)
    (raise-argument-error 'make-priority-worklist "pvector?" initial))
  ;; Build the heap by incremental sorted insertion, which is the same
  ;; operation priority-add! uses — no list round-trip needed.
  (define wl (PriorityWorklist item-compare
                               (box (pvector-empty))
                               (box (ordered-map-empty item-compare))
                               priority-fn))
  (for ([item (in-pvector initial)])
    (priority-add! wl item))
  wl)

(define (priority-empty? wl)
  (pvector-empty? (unbox (PriorityWorklist-heap-box wl))))

(define (priority-add! wl item)
  (define members (unbox (PriorityWorklist-members-box wl)))
  (unless (ordered-map-has-key? members item)
    (set-box! (PriorityWorklist-members-box wl)
              (ordered-map-set members item #t))
    (define pf (PriorityWorklist-priority-fn wl))
    (define heap (unbox (PriorityWorklist-heap-box wl)))
    ;; Insert in sorted order
    (define item-priority (pf item))
    (define-values (before after)
      (pvector-split-at heap
        (let loop ([i 0])
          (if (>= i (pvector-length heap))
              i
              (if (< item-priority (pf (pvector-ref heap i)))
                  i
                  (loop (+ i 1)))))))
    (set-box! (PriorityWorklist-heap-box wl)
              (pvector-append before
                (pvector-cons-left after item)))))

(define (priority-remove! wl)
  (define heap (unbox (PriorityWorklist-heap-box wl)))
  (define-values (item rest) (pvector-pop-left heap))
  (set-box! (PriorityWorklist-heap-box wl) rest)
  (define members (unbox (PriorityWorklist-members-box wl)))
  (define-values (new-members _) (ordered-map-delete members item))
  (set-box! (PriorityWorklist-members-box wl) new-members)
  item)

(define (priority-member? wl item)
  (ordered-map-has-key? (unbox (PriorityWorklist-members-box wl)) item))

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
  (unless (pvector? items)
    (raise-argument-error 'worklist-add-all! "pvector?" items))
  (for ([item (in-pvector items)])
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
;; process : item -> pvector of items to add
;;
(define (worklist-iterate wl process)
  (let loop ()
    (unless (worklist-empty? wl)
      (define item (worklist-remove! wl))
      (define new-items (process item))
      (worklist-add-all! wl new-items)
      (loop))))

;; Iterate with state
;; process : item state -> (values state pvector-of-items)
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
