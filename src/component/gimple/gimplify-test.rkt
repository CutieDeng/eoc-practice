#lang racket/base

;; Alignment test (pure pipeline): import + genericize + gimplify + print
;; must reproduce GCC's -fdump-tree-gimple for the fixtures, modulo the
;; numbering of artificial names (renumbered by first appearance).

(require rackunit racket/runtime-path racket/string racket/list racket/file
         "../../kernel/data/data.rkt"
         "../../kernel/ir/gimple/types.rkt"
         "../../kernel/ir/gccast/types.rkt"
         "../../frontend/gccast/reader.rkt"
         "state.rkt" "import.rkt" "genericize.rkt" "gimplify.rkt" "print.rkt")

(provide gimplify-tu normalize-gimple-text first-diff)

(define-runtime-path fixtures "../../../test/integration/gccast")

;; Whole-TU gimplification: (values text st)
(define (gimplify-tu tu)
  (define st0 (import-tu tu))
  (define o (open-output-string))
  (define st*
    (for/fold ([st st0]) ([fn (in-list (TU-fns tu))])
      (define-values (decl body st1) (import-fn st fn))
      (define-values (body* st2) (c-genericize st1 decl body))
      (define-values (bind st3) (gimplify-function st2 decl body*))
      (print-function st3 decl bind o)
      st3))
  (values (get-output-string o) st*))

(define (normalize-gimple-text s)
  (define d-map (make-hash)) (define ssa-map (make-hash)) (define k-map (make-hash))
  (define (num m k) (hash-ref! m k (lambda () (add1 (hash-count m)))))
  ;; D.N, name.K_N (named temporaries), name_N (SSA names of variables),
  ;; _N (anonymous SSA names); the version counter is one sequence
  (define renamed
    (regexp-replace* #px"D\\.[0-9]+|([A-Za-z_][A-Za-z0-9_]*)\\.([0-9]+)_([0-9]+)|([A-Za-z_][A-Za-z0-9_]*)_([0-9]+)\\b|(?<![A-Za-z0-9_.])_[0-9]+\\b"
                     s
                     (lambda (all name k n vname vn)
                       (cond
                         [(string-prefix? all "D.") (format "D.~a" (num d-map all))]
                         [name (format "~a.~a_~a" name (num k-map k) (num ssa-map n))]
                         [vname (format "~a_~a" vname (num ssa-map vn))]
                         [else (format "_~a" (num ssa-map (substring all 1)))]))))
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
         [name '("basic.c" "calls.c" "struct.c" "locals.c" "ccp.c")])
    (test-case (format "~a ~a: gimple dump aligns with GCC (pure pipeline)" name ver)
      (define tu (read-gccast-file (build-path fixtures (format "~a.~a.dat" name ver))))
      (define-values (text st) (gimplify-tu tu))
      (define ours (normalize-gimple-text text))
      (define gcc (normalize-gimple-text (file->string (build-path fixtures (format "~a.~a.gimple" name ver)))))
      (unless (string=? ours gcc)
        (fail (format "mismatch (~a ~a)\n~a\n--- ours (normalized) ---\n~a\n--- gcc (normalized) ---\n~a"
                      name ver (first-diff ours gcc) ours gcc))))))
