#lang racket/base

;; ============================================================
;; opt: gcc_qsort and gcc_stablesort (gcc/sort.cc)
;; ============================================================
;;
;;   (gcc-qsort lst cmp)       -> list
;;   (gcc-stablesort lst cmp)  -> list
;;     CMP: (a b) -> integer, negative when A goes first (a qsort
;;     comparator).  vec::qsort is gcc_qsort: a merge sort whose leaves
;;     of up to 5 elements go through a sorting network, so equal
;;     elements are not kept in order when a leaf has 4 or 5 of them.
;;     gcc_stablesort limits the leaves to 3 elements (stable).
;;     The networks swap E0 and E1 when E1 < E0 (cmp1 (e1, e0) < 0);
;;     the merge takes the right element only when it is strictly
;;     smaller, and copies the left half unchanged when the first right
;;     element is not smaller than the last left one.
;; ============================================================

(require racket/vector)

(provide gcc-qsort gcc-stablesort)

(define (gcc-qsort lst cmp) (sort* lst cmp 5))
(define (gcc-stablesort lst cmp) (sort* lst cmp 3))

(define (sort* lst cmp nlim)
  (define v (list->vector lst))
  (if (< (vector-length v) 2) lst (vector->list (msort v cmp nlim))))

;; netsort: the network for 2 to 5 elements
(define (netsort v cmp)
  (define e (vector-copy v))
  (define n (vector-length e))
  (define (c! i j)
    (when (< (cmp (vector-ref e j) (vector-ref e i)) 0)
      (define t (vector-ref e i))
      (vector-set! e i (vector-ref e j))
      (vector-set! e j t)))
  (c! 0 1)
  (when (= n 3) (c! 1 2) (c! 0 1))
  (when (> n 3)
    (when (= n 5) (c! 3 4) (c! 2 4))
    (c! 2 3)
    (when (= n 5) (c! 0 3) (c! 1 4))
    (c! 0 2) (c! 1 3) (c! 1 2))
  e)

(define (msort v cmp nlim)
  (define n (vector-length v))
  (cond
    [(<= n nlim) (if (< n 2) v (netsort v cmp))]
    [else
     (define nl (quotient n 2))
     (define r (msort (vector-copy v nl n) cmp nlim))
     (define l (msort (vector-copy v 0 nl) cmp nlim))
     (cond
       [(not (< (cmp (vector-ref r 0) (vector-ref l (sub1 nl))) 0)) (vector-append l r)]
       [else
        (define out (make-vector n #f))
        (let loop ([i 0] [j 0] [k 0])
          (cond
            [(= i nl) (for ([x (in-vector r j)] [k (in-naturals k)]) (vector-set! out k x))]
            [(= j (vector-length r)) (for ([x (in-vector l i)] [k (in-naturals k)]) (vector-set! out k x))]
            [(< (cmp (vector-ref r j) (vector-ref l i)) 0) (vector-set! out k (vector-ref r j)) (loop i (add1 j) (add1 k))]
            [else (vector-set! out k (vector-ref l i)) (loop (add1 i) j (add1 k))]))
        out])]))

(module+ test
  (require rackunit)
  ;; (loop, version) pairs ordered by loop only (loop_name_cmp)
  (define (by-car a b) (- (car a) (car b)))
  (check-equal? (gcc-qsort '((2 . 1) (2 . 2) (1 . 3) (2 . 4)) by-car) '((1 . 3) (2 . 2) (2 . 1) (2 . 4)))
  (check-equal? (gcc-stablesort '((2 . 1) (2 . 2) (1 . 3) (2 . 4)) by-car) '((1 . 3) (2 . 1) (2 . 2) (2 . 4)))
  (check-equal? (gcc-qsort '(5 3 9 1 7 2 8 6 4 0) -) '(0 1 2 3 4 5 6 7 8 9))
  (check-equal? (gcc-qsort '() -) '())
  (check-equal? (gcc-qsort '(1) -) '(1)))
