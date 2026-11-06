#lang racket/base

;; ============================================================
;; gccast-plugin build driver
;; ============================================================
;;
;;   racket build.rkt --gcc gcc-15 [--gcc /opt/.../gcc-12] [--no-tests]
;;                    [--fixtures DIR]
;;
;; For every --gcc:
;;   1. locate its plugin include dir and version
;;   2. run gen/main.rkt -> build/<ver>/gccast-config.inc
;;                        + ../src/frontend/gccast/tables/<ver>.rktd
;;   3. compile src/gccast.cc with the matching g++ -> build/<ver>/gccast.{dylib,so}
;;   4. compile tests/*.c|*.cc with -fplugin, write build/<ver>/<name>.dat,
;;      check each .dat `read`s back as a TU
;;   5. with --fixtures DIR, copy the .dat files there as <name>.<ver>.dat
;; ============================================================

(require racket/cmdline racket/system racket/port racket/string racket/file
         racket/path racket/list)

(define here (path-only (path->complete-path (syntax-source #'here))))
(define (rel . xs) (apply build-path here xs))

(define (run . args)
  (define-values (sp out in err)
    (apply subprocess #f #f #f (find-executable-path (car args)) (cdr args)))
  (close-output-port in)
  (define o (port->string out)) (define e (port->string err))
  (subprocess-wait sp)
  (values (subprocess-status sp) o e))

(define (run/ok . args)
  (define-values (rc o e) (apply run args))
  (unless (zero? rc)
    (error 'gccast-build "command failed (~a): ~a\n~a~a" rc (string-join args " ") o e))
  (string-trim o))

(define (gxx-for gcc)
  ;; gcc-15 -> g++-15 ; /x/bin/gcc-12 -> /x/bin/g++-12 ; gcc -> g++
  (define-values (dir name _) (split-path (string->path gcc)))
  (define n (regexp-replace #rx"gcc" (path->string name) "g++"))
  (if (path? dir) (path->string (build-path dir n)) n))

;; GCC derives the plugin name by stripping ".so", so keep that suffix on every OS.
(define (shared-ext) "so")

(define (extra-flags)
  (append
   (if (and (eq? (system-type 'os) 'macosx) (directory-exists? "/opt/homebrew/include"))
       '("-I/opt/homebrew/include") '())
   (if (eq? (system-type 'os) 'macosx) '("-Wl,-undefined,dynamic_lookup") '())))

(define (build-one gcc #:tests? tests? #:fixtures fixtures)
  (define gxx (gxx-for gcc))
  (define plugin-dir (run/ok gcc "-print-file-name=plugin"))
  (define inc (build-path plugin-dir "include"))
  (unless (file-exists? (build-path inc "gcc-plugin.h"))
    (error 'gccast-build "~a has no plugin headers under ~a" gcc inc))
  (define ver (run/ok gcc "-dumpfullversion"))
  (define out (rel "build" ver))
  (make-directory* out)
  (define tables-dir (rel 'up "src" "frontend" "gccast" "tables"))
  (make-directory* tables-dir)
  (printf "== ~a (~a) plugin headers: ~a\n" gcc ver inc)
  ;; 2. generate
  (printf "   gen: ~a\n"
          (run/ok "racket" (path->string (rel "gen" "main.rkt"))
                  "--include-dir" (path->string inc) "--version" ver
                  "--rktd" (path->string (build-path tables-dir (string-append ver ".rktd")))
                  "--inc" (path->string (build-path out "gccast-config.inc"))))
  ;; 3. compile
  (define lib (build-path out (string-append "gccast." (shared-ext))))
  (apply run/ok gxx
         (append (list "-std=c++17" "-fPIC" "-shared" "-fno-rtti" "-O1"
                       (string-append "-I" (path->string inc))
                       (string-append "-I" (path->string out)))
                 (extra-flags)
                 (list "-o" (path->string lib) (path->string (rel "src" "gccast.cc")))))
  (printf "   built: ~a (~a bytes)\n" lib (file-size lib))
  ;; 4. tests
  (when tests?
    (for ([src (in-list (sort (directory-list (rel "tests") #:build? #t) path<?))]
          #:when (regexp-match? #rx"[.](c|cc)$" (path->string src)))
      (define cxx? (regexp-match? #rx"[.]cc$" (path->string src)))
      (define name (path->string (file-name-from-path src)))
      (define dat (build-path out (string-append name ".dat")))
      ;; run from tests/ with a relative file name so the TU's main-file
      ;; (and every location) is path-stable across machines
      (parameterize ([current-directory (rel "tests")])
        (run/ok (if cxx? gxx gcc)
                (string-append "-fplugin=" (path->string lib))
                (string-append "-fplugin-arg-gccast-out=" (path->string dat))
                ;; GCC's own lowering dumps are the alignment oracle for
                ;; src/component/gimple/*
                "-O0"
                (string-append "-fdump-tree-original=" (path->string (build-path out (string-append name ".original"))))
                (string-append "-fdump-tree-gimple=" (path->string (build-path out (string-append name ".gimple"))))
                (string-append "-fdump-tree-lower=" (path->string (build-path out (string-append name ".lower"))))
                (string-append "-fdump-tree-cfg=" (path->string (build-path out (string-append name ".cfg"))))
                (string-append "-fdump-tree-ssa=" (path->string (build-path out (string-append name ".ssa"))))
                (string-append "-fdump-tree-ssa-vops=" (path->string (build-path out (string-append name ".ssavops"))))
                "-c" name "-o" (if (eq? (system-type 'os) 'windows) "NUL" "/dev/null")))
      (define tu (call-with-input-file dat read))
      (unless (and (vector? (struct->vector tu)) (eq? (prefab-struct-key tu) 'TU))
        (error 'gccast-build "~a: not a TU: ~e" dat tu))
      (define fns (vector-ref (struct->vector tu) 3))
      (printf "   ~a -> ~a: ~a function(s), ~a nodes\n" name dat (length fns)
              (for/sum ([fn fns]) (vector-length (vector-ref (struct->vector fn) 4))))
      (when fixtures
        (make-directory* fixtures)
        (copy-file dat (build-path fixtures (string-append name "." ver ".dat")) #t)
        (for ([ext '("original" "gimple" "lower" "cfg" "ssa" "ssavops")])
          (define src (build-path out (string-append name "." ext)))
          (when (file-exists? src)
            (copy-file src (build-path fixtures (string-append name "." ver "." ext)) #t)))))))

(module+ main
  (define gccs '()) (define tests? #t) (define fixtures #f)
  (command-line
   #:once-each
   [("--no-tests") "skip compiling tests/ with the plugin" (set! tests? #f)]
   [("--fixtures") dir "copy produced .dat files to DIR as <name>.<ver>.dat" (set! fixtures dir)]
   #:multi
   [("--gcc") g "gcc driver to build for (repeatable)" (set! gccs (append gccs (list g)))])
  (when (null? gccs) (set! gccs '("gcc")))
  (for ([g gccs]) (build-one g #:tests? tests? #:fixtures fixtures)))
