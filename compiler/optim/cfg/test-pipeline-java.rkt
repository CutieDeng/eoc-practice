#lang racket/base

;; ============================================================
;; 在真实 Java 代码上测试优化流水线
;; ============================================================

(require racket/list racket/format racket/string racket/match)
(require "../../ftree.rkt")
(require "../../core/cfg.rkt")
(require "../../core/jvm.rkt")
(require "../../cfg/raw.rkt")
(require "../../frontend/java/reader.rkt")
(require "../../transform/jvm-to-cfg.rkt")
(require "pipeline.rkt")

;; ============================================================
;; 辅助函数
;; ============================================================

(define (count-insns cfg)
  (for/sum ([bid (cfg-all-block-ids cfg)])
    (define block (cfg-get-block cfg bid))
    (if block (length (CfgBlock-insns block)) 0)))

(define (count-blocks cfg)
  (length (cfg-all-block-ids cfg)))

(define (print-separator [char #\=] [width 60])
  (displayln (make-string width char)))

(define (print-header title)
  (newline)
  (print-separator)
  (displayln title)
  (print-separator))

;; 打印 CFG 统计
(define (print-cfg-stats cfg label)
  (printf "~a:\n" label)
  (printf "  Blocks: ~a\n" (count-blocks cfg))
  (printf "  Instructions: ~a\n" (count-insns cfg)))

;; 打印块内容
(define (print-block cfg bid [max-insns 10])
  (define block (cfg-get-block cfg bid))
  (when block
    (printf "  Block ~a:\n" (BlockId-id bid))
    (define insns (CfgBlock-insns block))
    (define shown (take insns (min max-insns (length insns))))
    (for ([insn shown])
      (when (VfInsn? insn)
        (printf "    ~a ~a -> ~a\n"
                (VfInsn-op insn)
                (VfInsn-inputs insn)
                (VfInsn-outputs insn))))
    (when (> (length insns) max-insns)
      (printf "    ... (~a more)\n" (- (length insns) max-insns)))
    (printf "    terminator: ~a\n"
            (match (CfgBlock-terminator block)
              [(TermJump t) (format "jump ~a" (BlockId-id t))]
              [(TermBranch c t e) (format "branch ~a ? ~a : ~a"
                                          c (BlockId-id t) (BlockId-id e))]
              [(TermReturn vs) (format "return ~a" vs)]
              [(TermThrow e) (format "throw ~a" e)]
              [(TermUnreachable) "unreachable"]
              [other (format "~a" other)]))))

;; ============================================================
;; 测试单个方法
;; ============================================================

(define (test-method method)
  (define name (JvmMethod-name method))
  (define desc (JvmMethod-descriptor method))

  (print-header (format "Method: ~a~a" name desc))

  ;; 转换为 CFG
  (define cfg
    (with-handlers ([exn:fail? (λ (e) #f)])
      (jvm-method->cfg method)))

  (cond
    [(not cfg)
     (displayln "  [Skipped - conversion failed or abstract method]")]
    [(= (count-insns cfg) 0)
     (displayln "  [Skipped - empty method]")]
    [else
     ;; 优化前统计
     (print-cfg-stats cfg "Before optimization")

     ;; 打印优化前的前几个块
     (displayln "\n  Sample blocks before:")
     (for ([bid (take (cfg-all-block-ids cfg)
                      (min 2 (length (cfg-all-block-ids cfg))))])
       (print-block cfg bid 5))

     ;; 应用优化流水线
     (define-values (cfg-opt stats) (cfg-optimize-with-stats cfg))

     ;; 优化后统计
     (newline)
     (print-cfg-stats cfg-opt "After optimization")

     ;; 打印优化统计
     (displayln "\n  Optimization stats:")
     (for ([stat stats])
       (printf "    ~a: ~a\n" (car stat) (cdr stat)))

     ;; 计算改进
     (define before (count-insns cfg))
     (define after (count-insns cfg-opt))
     (define reduction (- before after))
     (define percent (if (> before 0)
                         (* 100.0 (/ reduction before))
                         0))
     (printf "\n  Improvement: ~a instructions removed (~a%)\n"
             reduction
             (~r percent #:precision 1))

     ;; 打印优化后的前几个块
     (displayln "\n  Sample blocks after:")
     (for ([bid (take (cfg-all-block-ids cfg-opt)
                      (min 2 (length (cfg-all-block-ids cfg-opt))))])
       (print-block cfg-opt bid 5))]))

;; ============================================================
;; 主测试
;; ============================================================

(define (run-tests dat-file)
  (print-header "Loading Java bytecode...")
  (printf "File: ~a\n" dat-file)

  ;; 读取类文件
  (define jvm-class (read-jvm-class-file dat-file))
  (printf "Class: ~a\n" (JvmClass-name jvm-class))
  (printf "Methods: ~a\n" (length (JvmClass-methods jvm-class)))

  ;; 测试每个方法
  (define methods (JvmClass-methods jvm-class))

  ;; 统计
  (define total-before 0)
  (define total-after 0)
  (define methods-optimized 0)

  (for ([method methods])
    (define name (JvmMethod-name method))

    ;; 跳过一些特殊方法
    (unless (or (string-prefix? name "lambda$")
                (string=? name "<clinit>"))
      (define cfg
        (with-handlers ([exn:fail? (λ (e) #f)])
          (jvm-method->cfg method)))

      (when (and cfg (> (count-insns cfg) 0))
        (set! total-before (+ total-before (count-insns cfg)))

        (define cfg-opt (cfg-optimize cfg))
        (set! total-after (+ total-after (count-insns cfg-opt)))
        (set! methods-optimized (+ methods-optimized 1))

        (test-method method))))

  ;; 总结
  (print-header "Summary")
  (printf "Methods optimized: ~a\n" methods-optimized)
  (printf "Total instructions before: ~a\n" total-before)
  (printf "Total instructions after: ~a\n" total-after)
  (define total-reduction (- total-before total-after))
  (define total-percent (if (> total-before 0)
                            (* 100.0 (/ total-reduction total-before))
                            0))
  (printf "Total reduction: ~a instructions (~a%)\n"
          total-reduction
          (~r total-percent #:precision 1)))

;; ============================================================
;; 运行
;; ============================================================

(module+ main
  (define dat-file
    (build-path (current-directory) "compiler/test/ClassTransform.dat"))
  (run-tests (path->string dat-file)))
