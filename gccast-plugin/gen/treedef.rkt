#lang racket/base

;; ============================================================
;; gccast generator: harvest tree-code tables from a GCC plugin
;; include directory.
;; ============================================================
;;
;; Inputs (all under `$(gcc -print-file-name=plugin)/include`):
;;   tree.def, c-family/c-common.def, cp/cp-tree.def
;;     DEFTREECODE (CODE, "name", tcc_class, nargs)
;;   tree.h, c-family/c-common.h, cp/cp-tree.h
;;     #define ACCESSOR(NODE) TREE_OPERAND (CODE_CHECK (NODE), n)
;;     #define ACCESSOR(NODE) TREE_OPERAND (TREE_CHECKk (NODE, C1, ..., Ck), n)
;;
;; Output: a `tree-table` struct; see main.rkt for the .rktd/.inc
;; emitters.  Everything here is pure parsing; no GCC is executed.
;; ============================================================

(require racket/string racket/list racket/match racket/file racket/set)

(provide (struct-out tree-code)
         (struct-out tree-table)
         harvest
         diff-tables)

;; code     : symbol, e.g. 'PLUS_EXPR
;; name     : string, GCC's printable name, e.g. "plus_expr"
;; class    : symbol, e.g. 'tcc_binary
;; nargs    : integer (operand count; -1 for vl_exp)
;; source   : string, which .def it came from
;; operands : list of symbol-or-#f, index = operand position (from accessor macros)
(struct tree-code (code name class nargs source operands) #:transparent)
(struct tree-table (include-dir codes) #:transparent)

(define def-files '("tree.def" "c-family/c-common.def" "cp/cp-tree.def"))
(define hdr-files '("tree.h" "c-family/c-common.h" "cp/cp-tree.h"))

(define deftreecode-rx
  #px"DEFTREECODE\\s*\\(\\s*([A-Z0-9_]+)\\s*,\\s*\"([^\"]+)\"\\s*,\\s*(tcc_[a-z_]+)\\s*,\\s*(-?[0-9]+)\\s*\\)")

;; #define NAME(NODE) TREE_OPERAND (X_CHECK (NODE), n)
(define accessor-rx
  #px"(?m:^#define\\s+([A-Z0-9_]+)\\s*\\(\\s*NODE\\s*\\)\\s+TREE_OPERAND\\s*\\(\\s*([A-Z0-9_]+)_CHECK\\s*\\(\\s*NODE\\s*\\)\\s*,\\s*([0-9]+)\\s*\\))")
;; #define NAME(NODE) TREE_OPERAND (TREE_CHECKk (NODE, C1, C2, ...), n)
(define accessor-multi-rx
  #px"(?m:^#define\\s+([A-Z0-9_]+)\\s*\\(\\s*NODE\\s*\\)\\s+TREE_OPERAND\\s*\\(\\s*TREE_CHECK[0-9]\\s*\\(\\s*NODE\\s*,([^)]*)\\)\\s*,\\s*([0-9]+)\\s*\\))")

(define (read-if-exists path)
  (and (file-exists? path) (file->string path)))

(define (parse-defs include-dir)
  (for*/list ([rel (in-list def-files)]
              [txt (in-value (read-if-exists (build-path include-dir rel)))]
              #:when txt
              [m (in-list (regexp-match* deftreecode-rx txt #:match-select values))])
    (match-define (list _ code name cls nargs) m)
    (tree-code (string->symbol code) name (string->symbol cls) (string->number nargs) rel '())))

;; -> hash code-symbol -> hash position -> accessor-symbol
(define (parse-accessors include-dir)
  (define acc (make-hash))
  (define (add! code pos name)
    (define h (hash-ref! acc code (lambda () (make-hash))))
    ;; keep the first accessor seen for a position (headers define the
    ;; canonical one first; aliases come later)
    (unless (hash-has-key? h pos) (hash-set! h pos name)))
  (for* ([rel (in-list hdr-files)]
         [txt (in-value (read-if-exists (build-path include-dir rel)))]
         #:when txt)
    (for ([m (in-list (regexp-match* accessor-rx txt #:match-select values))])
      (match-define (list _ name code pos) m)
      (add! (string->symbol code) (string->number pos) (string->symbol name)))
    (for ([m (in-list (regexp-match* accessor-multi-rx txt #:match-select values))])
      (match-define (list _ name codes pos) m)
      (for ([c (in-list (map string-trim (string-split codes ",")))] #:when (regexp-match? #px"^[A-Z0-9_]+$" c))
        (add! (string->symbol c) (string->number pos) (string->symbol name)))))
  acc)

(define (harvest include-dir)
  (define defs (parse-defs include-dir))
  (define accs (parse-accessors include-dir))
  (tree-table
   (path->string (simplify-path (path->complete-path include-dir)))
   (for/list ([tc (in-list defs)])
     (define h (hash-ref accs (tree-code-code tc) #f))
     (define n (max 0 (tree-code-nargs tc)
                    (if h (add1 (apply max -1 (hash-keys h))) 0)))
     (struct-copy tree-code tc
                  [operands (for/list ([i (in-range n)]) (and h (hash-ref h i #f)))]))))

;; Human-readable diff between two harvested tables.
(define (diff-tables a b)
  (define ha (for/hash ([c (tree-table-codes a)]) (values (tree-code-code c) c)))
  (define hb (for/hash ([c (tree-table-codes b)]) (values (tree-code-code c) c)))
  (define ka (list->set (hash-keys ha)))
  (define kb (list->set (hash-keys hb)))
  (define changed
    (for/list ([k (in-list (sort (set->list (set-intersect ka kb)) symbol<?))]
               #:unless (let ([x (hash-ref ha k)] [y (hash-ref hb k)])
                          (and (eq? (tree-code-class x) (tree-code-class y))
                               (= (tree-code-nargs x) (tree-code-nargs y))
                               (equal? (tree-code-operands x) (tree-code-operands y)))))
      (list k
            (list (tree-code-class (hash-ref ha k)) (tree-code-nargs (hash-ref ha k)) (tree-code-operands (hash-ref ha k)))
            (list (tree-code-class (hash-ref hb k)) (tree-code-nargs (hash-ref hb k)) (tree-code-operands (hash-ref hb k))))))
  (hash 'only-in-a (sort (set->list (set-subtract ka kb)) symbol<?)
        'only-in-b (sort (set->list (set-subtract kb ka)) symbol<?)
        'changed changed))

(module+ test
  (require rackunit)
  ;; parsing is exercised on a synthetic snippet so the test needs no GCC
  (define dir (make-temporary-file "gccast-~a" 'directory))
  (make-directory* (build-path dir "c-family"))
  (display-to-file "DEFTREECODE (PLUS_EXPR, \"plus_expr\", tcc_binary, 2)\nDEFTREECODE (FOR_STMT, \"for_stmt\", tcc_statement,\n  5)\n"
                   (build-path dir "tree.def"))
  (display-to-file "#define FOR_INIT_STMT(NODE)\tTREE_OPERAND (FOR_STMT_CHECK (NODE), 0)\n#define FOR_BODY(NODE) TREE_OPERAND (FOR_STMT_CHECK (NODE), 3)\n#define OBJ_X(NODE) TREE_OPERAND (TREE_CHECK2 (NODE, PLUS_EXPR, FOR_STMT), 1)\n"
                   (build-path dir "tree.h"))
  (define t (harvest dir))
  (define by (for/hash ([c (tree-table-codes t)]) (values (tree-code-code c) c)))
  (check-equal? (tree-code-nargs (hash-ref by 'FOR_STMT)) 5 "multi-line DEFTREECODE")
  (check-equal? (tree-code-operands (hash-ref by 'FOR_STMT)) '(FOR_INIT_STMT OBJ_X #f FOR_BODY #f))
  (check-equal? (tree-code-operands (hash-ref by 'PLUS_EXPR)) '(#f OBJ_X))
  (delete-directory/files dir))
