#lang racket/base

;; Alignment test: genericize + gimplify + print must reproduce GCC's
;; own -fdump-tree-gimple for the committed fixtures, modulo the
;; numbering of artificial names (D.NNNN, _N, name.K_N), which is
;; renumbered by order of first appearance on both sides.

(require rackunit racket/runtime-path racket/string racket/list racket/file racket/port
         "../generic/tree.rkt" "../generic/import.rkt"
         "genericize.rkt" "gimplify.rkt" "gimple-print.rkt"
         "../../../frontend/gccast/reader.rkt"
         "../../../kernel/ir/gccast/types.rkt")

(provide lower-tu->gimple-text normalize-gimple-text)

(define-runtime-path fixtures "../../../../test/integration/gccast")

;; Whole-TU lowering, printed like the dump (functions in order).
(define (lower-tu->gimple-text tu)
  (install-tu-context! tu)
  (parameterize ([tmp-var-id (box 0)] [uid-counter (box 100000)])
    (define o (open-output-string))
    (for ([fn (TU-fns tu)])
      (define-values (decl body) (import-fn tu fn))
      (define b (box body))
      (c-genericize! decl (box-ref b))
      (define bind (gimplify-function decl (unbox b)))
      (print-function decl bind o))
    (get-output-string o)))

(define (normalize-gimple-text s)
  (define d-map (make-hash)) (define ssa-map (make-hash)) (define k-map (make-hash))
  (define (num m k) (hash-ref! m k (lambda () (add1 (hash-count m)))))
  (define renamed
    (regexp-replace* #px"D\\.[0-9]+|([A-Za-z_][A-Za-z0-9_]*)\\.([0-9]+)_([0-9]+)|(?<![A-Za-z0-9_.])_[0-9]+\\b"
                     s
                     (lambda (all name k n)
                       (cond
                         [(string-prefix? all "D.") (format "D.~a" (num d-map all))]
                         [name (format "~a.~a_~a" name (num k-map k) (num ssa-map n))]
                         [else (format "_~a" (num ssa-map (substring all 1)))]))))
  ;; drop dump headers / trailing spaces / runs of blank lines
  (define lines
    (for/list ([l (string-split renamed "\n" #:trim? #f)]
               #:unless (string-prefix? l ";;"))
      (string-trim l #:left? #f)))
  (define collapsed
    (let loop ([ls lines] [acc '()] [blank? #t])
      (cond
        [(null? ls) (reverse acc)]
        [(string=? (car ls) "") (loop (cdr ls) (if blank? acc (cons "" acc)) #t)]
        [else (loop (cdr ls) (cons (car ls) acc) #f)])))
  (string-join (let loop ([l (reverse collapsed)]) (if (and (pair? l) (string=? (car l) "")) (loop (cdr l)) (reverse l))) "\n"))

(define (first-diff a b)
  (define la (string-split a "\n" #:trim? #f)) (define lb (string-split b "\n" #:trim? #f))
  (for/first ([i (in-naturals)] [x (in-list (append la (make-list (max 0 (- (length lb) (length la))) "<eof>")))]
              [y (in-list (append lb (make-list (max 0 (- (length la) (length lb))) "<eof>")))]
              #:unless (string=? x y))
    (format "line ~a:\n  ours:   ~a\n  gcc:    ~a" (add1 i) x y)))

(module+ test
  (for* ([ver '("15.2.0" "12.5.0")]
         [name '("basic.c" "calls.c" "struct.c" "locals.c")])
    (test-case (format "~a ~a: gimple dump aligns with GCC" name ver)
      (define tu (read-gccast-file (build-path fixtures (format "~a.~a.dat" name ver))))
      (define ours (normalize-gimple-text (lower-tu->gimple-text tu)))
      (define gcc (normalize-gimple-text (file->string (build-path fixtures (format "~a.~a.gimple" name ver)))))
      (unless (string=? ours gcc)
        (fail (format "mismatch (~a ~a)\n~a\n--- ours (normalized) ---\n~a\n--- gcc (normalized) ---\n~a"
                      name ver (first-diff ours gcc) ours gcc))))))
