#lang racket/base

;; ============================================================
;; Java Frontend: 构建脚本
;; ============================================================
;;
;; 编译和运行 Java ASM 工具，生成 Racket 可读的类文件数据
;; ============================================================

(require racket/system)
(require racket/path)
(require racket/file)
(require racket/dict)
(require racket/string)

;; === 配置 ===

(define libs '(
  ((name . "asm")(url . "https://repo1.maven.org/maven2/org/ow2/asm/asm/9.7/asm-9.7.jar"))
  ((name . "asm-commons")(url . "https://repo1.maven.org/maven2/org/ow2/asm/asm-commons/9.7/asm-commons-9.7.jar"))
))

(define out-dir "out")
(define libs-dir "libs")
(define src-dir "src")

(define compile-files '(
  "com/cutiedeng/util/StringUtil.java"
  "com/cutiedeng/util/AccessUtil.java"
  "com/cutiedeng/util/AsmOpcodeUtil.java"
  "com/cutiedeng/info/DatumInsn.java"
  "com/cutiedeng/info/DatumField.java"
  "com/cutiedeng/info/DatumMethod.java"
  "com/cutiedeng/info/DatumClass.java"
  "com/cutiedeng/info/DatumDebugLineInfo.java"
  "com/cutiedeng/info/DatumAnnotation.java"
  "com/cutiedeng/info/DatumInnerClass.java"
  "com/cutiedeng/ClassTransform.java"
))

(define entry "com/cutiedeng/ClassTransform")

;; === 工具函数 ===

(define (get-java-tools-path)
  (let ([this-file (syntax-source #'here)])
    (if this-file
        (path->string (path-only this-file))
        ".")))

(define (run-in-dir dir thunk)
  (parameterize ([current-directory dir])
    (thunk)))

;; === 下载依赖 ===

(define (download-libs)
  (define base-path (get-java-tools-path))
  (define libs-path (build-path base-path libs-dir))
  (make-directory* libs-path)
  (parameterize ([current-directory libs-path])
    (for ([lib libs])
      (define name (dict-ref lib 'name))
      (define url (dict-ref lib 'url))
      (define filename (path->string (file-name-from-path url)))
      (unless (file-exists? filename)
        (eprintf "Downloading ~a...~n" name)
        (system* (find-executable-path "curl") "-O" "-L" url)))))

(provide download-libs)

;; === 编译 Java 源码 ===

(define (compile-java)
  (define base-path (get-java-tools-path))
  (define out-path (build-path base-path out-dir))
  (define src-path (build-path base-path src-dir))
  (define libs-path (build-path base-path libs-dir))

  (make-directory* out-path)

  ;; Classpath: libs/* (for jars) and src (for other sources)
  (define classpath
    (string-append
      (path->string libs-path) "/*:"
      (path->string src-path)))

  (for/and ([src-file compile-files])
    (define full-path (build-path src-path src-file))
    (eprintf "Compiling ~a...~n" src-file)
    (system* (find-executable-path "javac")
             "-d" (path->string out-path)
             "-cp" classpath
             (path->string full-path))))

(provide compile-java)

;; === 运行分析器 ===

(define (analyze-class class-name [output-file #f])
  (define base-path (get-java-tools-path))
  (define out-path (build-path base-path out-dir))
  (define libs-path (build-path base-path libs-dir))

  (define classpath
    (string-append
      (path->string out-path) ":"
      (path->string libs-path) "/*"))

  (define output-path (or output-file
                          (build-path base-path
                            (string-append (regexp-replace* #rx"/" class-name "_") ".dat"))))

  (eprintf "Analyzing ~a -> ~a~n" class-name output-path)
  (parameterize ([current-directory out-path])
    (with-output-to-file output-path #:exists 'replace
      (lambda ()
        (system* (find-executable-path "java")
                 "-cp" classpath
                 entry
                 class-name)))))

(provide analyze-class)

;; === 主程序 ===

(module+ main
  (require racket/cmdline)

  (define mode 'help)
  (define class-name #f)
  (define output-file #f)

  (command-line
    #:program "java-tools"
    #:once-any
    [("-d" "--download") "Download dependencies" (set! mode 'download)]
    [("-c" "--compile") "Compile Java sources" (set! mode 'compile)]
    [("-a" "--analyze") name "Analyze a class"
      (set! mode 'analyze) (set! class-name name)]
    #:once-each
    [("-o" "--output") file "Output file for analysis" (set! output-file file)]
    #:args ()
    (case mode
      [(download) (download-libs)]
      [(compile) (compile-java)]
      [(analyze) (analyze-class class-name output-file)]
      [else (displayln "Usage: racket build.rkt [-d|-c|-a <class>]")])))
