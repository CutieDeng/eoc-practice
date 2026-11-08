#lang racket/base

;; ============================================================
;; opt: the iteration bounds GCC keeps on a loop (cfgloop.h
;; any_upper_bound / nb_iterations_upper_bound, the likely upper bound,
;; nb_iterations_estimate; cfgloop.cc record_niter_bound,
;; get_*_loop_iterations; tree-ssa-loop-ivcanon.cc
;; adjust_loop_info_after_peeling)
;; ============================================================
;;
;; A function's bounds are its 'loop-bounds declaration flag: a hash
;; from the loop number to (lbnd upper likely estimate), #f = unknown.
;; They outlive the passes as GCC's loop structures do and only go down;
;; free_numbers_of_iterations_estimates does not touch them.
;;
;;   (loop-bound st decl num) -> lbnd
;;   (record-niter-bound st decl num bound realistic? upper?) -> st
;;   (set-loop-upper-bound st decl num bound) -> st     (the exact count)
;;   (adjust-loop-info-after-peeling st decl num npeel precise?) -> st
;; ============================================================

(require "../state.rkt")

(provide (struct-out lbnd) loop-bound record-niter-bound set-loop-upper-bound adjust-loop-info-after-peeling)

(struct lbnd (upper likely estimate) #:prefab)

(define (table st decl) (decl-flag st decl 'loop-bounds (hasheqv)))
(define (loop-bound st decl num) (hash-ref (table st decl) num (lbnd #f #f #f)))
(define (put st decl num b) (set-decl-flag st decl 'loop-bounds (hash-set (table st decl) num b)))

;; record_niter_bound
(define (record-niter-bound st decl num bound realistic? upper?)
  (define b (loop-bound st decl num))
  (define-values (u0 lk0 e0) (values (lbnd-upper b) (lbnd-likely b) (lbnd-estimate b)))
  (define-values (u1 lk1)
    (if (and upper? (or (not u0) (< bound u0)))
        (values bound (or lk0 bound))
        (values u0 lk0)))
  (define e1 (if (and realistic? (or (not e0) (< bound e0))) bound e0))
  (define lk2 (if (and (not realistic?) (or (not lk1) (< bound lk1))) bound lk1))
  (define e2 (if (and u1 e1 (< u1 e1)) u1 e1))
  (define lk3 (if (and u1 lk2 (< u1 lk2)) u1 lk2))
  (put st decl num (lbnd u1 lk3 e2)))

(define (set-loop-upper-bound st decl num bound)
  (define b (loop-bound st decl num))
  (put st decl num (lbnd bound (lbnd-likely b) (lbnd-estimate b))))

;; adjust_loop_info_after_peeling
(define (adjust-loop-info-after-peeling st decl num npeel precise?)
  (define b (loop-bound st decl num))
  (define e0 (lbnd-estimate b))
  (define e1 (and e0 (<= npeel e0) (- e0 npeel)))
  (define u1 (if (and (lbnd-upper b) precise?) (- (lbnd-upper b) npeel) (lbnd-upper b)))
  (define-values (lk2 e2)
    (cond
      [(not (lbnd-likely b)) (values #f e1)]
      [(<= npeel (lbnd-likely b)) (values (- (lbnd-likely b) npeel) e1)]
      [else (values 0 0)]))
  (put st decl num (lbnd u1 lk2 e2)))
