#lang racket/base

;; Tests for the gccast reader against committed fixtures produced by
;; gccast-plugin (no GCC needed here).  One fixture set per GCC version
;; exercises the per-version decode tables: FOR_STMT has 5 operands in
;; GCC 12 and 8 in GCC 15, yet FOR_COND / FOR_BODY resolve in both.

(require rackunit racket/runtime-path racket/list racket/string
         "reader.rkt"
         "../../kernel/ir/gccast/types.rkt")

(define-runtime-path fixtures "../../../test/integration/gccast")

(define versions '("12.5.0" "15.2.0"))

(define (fixture name ver) (build-path fixtures (format "~a.~a.dat" name ver)))
(define (fn-named tu name)
  (or (for/first ([f (TU-fns tu)] #:when (equal? (Fn-name f) name)) f)
      (error 'fn-named "no function ~a" name)))

(module+ test
  (for ([ver (in-list versions)])
    (define table (load-decode-table ver))

    (test-case (format "~a: basic.c reads, is closed, has f and g" ver)
      (define tu (read-gccast-file (fixture "basic.c" ver)))
      (check-equal? (TU-gcc-version tu) ver)
      (check-equal? (TU-main-file tu) "basic.c")
      (check-equal? (check-tu-closed tu) '())
      (check-equal? (map Fn-name (TU-fns tu)) '("f" "g"))
      ;; every node's code is known to the decode table
      (for* ([fn (TU-fns tu)] [n (in-vector (Fn-nodes fn))])
        (check-not-false (decode-entry table n) (format "unknown code ~a" (Node-code n)))))

    (test-case (format "~a: g's for loop resolves by accessor name" ver)
      (define tu (read-gccast-file (fixture "basic.c" ver)))
      (define g (fn-named tu "g"))
      (define fors (fn-nodes-of-code g 'for_stmt))
      (check-equal? (length fors) 1)
      (define for-stmt (car fors))
      (define d (decode-entry table for-stmt))
      (check-equal? (decode-nargs d) (if (equal? ver "12.5.0") 5 8))
      (check-equal? (length (node-ops for-stmt)) (decode-nargs d))
      (check-equal? (take (decode-operands d) 5) '(FOR_INIT_STMT FOR_COND FOR_EXPR FOR_BODY FOR_SCOPE))
      (define cond (fn-op g table for-stmt 'FOR_COND))
      ;; the C front end canonicalizes `i < 10` to `i <= 9`
      (check-not-false (memq (Node-code cond) '(lt_expr le_expr)) (format "cond is ~a" (Node-code cond)))
      (define body (fn-op g table for-stmt 'FOR_BODY))
      (check-not-false body)
      ;; the body contains the `s += i` modify
      (check-true (pair? (fn-nodes-of-code g 'modify_expr))))

    (test-case (format "~a: declarations, constants and the decl graph" ver)
      (define tu (read-gccast-file (fixture "basic.c" ver)))
      (define f (fn-named tu "f"))
      (define decl (fn-ref f (Fn-decl f)))
      (check-equal? (Node-code decl) 'function_decl)
      (check-equal? (node-field decl 'name) "f")
      (check-equal? (length (node-field decl 'arguments)) 1)
      (define parm (fn-ref f (car (node-field decl 'arguments))))
      (check-equal? (Node-code parm) 'parm_decl)
      (check-equal? (node-field parm 'name) "a")
      (define int-type (fn-ref f (Node-type parm)))
      (check-equal? (Node-code int-type) 'integer_type)
      (check-equal? (node-field int-type 'precision) 32)
      (check-equal? (node-field (fn-ref f (node-field int-type 'min)) 'value) -2147483648)
      ;; 128-bit all-ones max of bitsizetype arrives as a Wide
      (define wides (for/list ([n (in-vector (Fn-nodes f))]
                               #:when (and (eq? (Node-code n) 'integer_cst) (Wide? (node-field n 'value))))
                      (node-field n 'value)))
      (check-true (pair? wides))
      (check-equal? (wide->integer (car wides)) -1)
      ;; body is a bind_expr whose vars list is explicit
      (define body (fn-ref f (Fn-body f)))
      (check-equal? (Node-code body) 'bind_expr)
      (check-true (list? (node-field body 'vars))))

    (test-case (format "~a: struct.c string/real constants and record type" ver)
      (define tu (read-gccast-file (fixture "struct.c" ver)))
      (check-equal? (check-tu-closed tu) '())
      (define sum (fn-named tu "sum"))
      (define strs (fn-nodes-of-code sum 'string_cst))
      (check-equal? (map (lambda (n) (node-field n 'bytes)) strs) (list #"hi\n\0"))
      (define reals (fn-nodes-of-code sum 'real_cst))
      (check-true (pair? reals))
      (check-true (string-prefix? (node-field (car reals) 'hex) "0x"))
      (define recs (fn-nodes-of-code sum 'record_type))
      (check-true (pair? recs))
      (define fields (node-field (car recs) 'fields))
      (check-equal? (map (lambda (r) (node-field (fn-ref sum r) 'name)) fields) '("x" "y" "tag"))
      (check-true (pair? (fn-nodes-of-code sum 'while_stmt)))
      (check-true (pair? (fn-nodes-of-code sum 'switch_stmt))))

    (test-case (format "~a: method.cc (C++) stays small without decl chains" ver)
      (define tu (read-gccast-file (fixture "method.cc" ver)))
      (check-equal? (check-tu-closed tu) '())
      (check-equal? (sort (map Fn-name (TU-fns tu)) string<?) '("get" "h"))
      (for ([fn (TU-fns tu)])
        (check-true (< (vector-length (Fn-nodes fn)) 400)
                    (format "~a has ~a nodes" (Fn-name fn) (vector-length (Fn-nodes fn))))))))
