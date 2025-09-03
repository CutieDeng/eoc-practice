#lang racket

(require cutie-ftree)

(define ESCAPE_CHAR "\x1b")
(define (clear-screen) (printf "~a[2J~a[H" ESCAPE_CHAR ESCAPE_CHAR))
(define (hide-cursor) (printf "~a[?25l" ESCAPE_CHAR))
(define (show-cursor) (printf "~a[?25h" ESCAPE_CHAR))
(define (move-to row col) (printf "~a[~a;~aH" ESCAPE_CHAR row col))

(define (format-color-text fg bg text) (format "~a[~a;~am~a~a[0m" ESCAPE_CHAR fg bg text ESCAPE_CHAR))

(define menu-items
  '(("文件操作" ("新建" "打开" "保存" "删除" "重命名"))
    ("编辑功能" ("复制" "粘贴" "查找" "替换" "撤销"))
    ("视图设置" ("主题" "字体" "行号" "高亮" "分屏"))
    ("工具箱" ("终端" "文件管理" "计算器" "日历" "监控"))
    ("帮助" ("快捷键" "手册" "在线帮助" "关于" "更新"))))

(define current-menu 0)
(define menu-selects '(0 0 0 0 0))

(define (get-current-details)
  (second (list-ref menu-items current-menu)))

(hide-cursor)
(define (render-menu)
  (clear-screen)
  (move-to 1 1)
  (printf "~a" (format-color-text 37 44 " Racket TUI 菜单演示 - 使用 hjkl 或方向键导航 "))
  (for (
    [item menu-items]
    [i (in-naturals)])
    (move-to (+ i 2) 2)
    (if (= i current-menu)
      (printf "~a" (format-color-text 30 47 (format "> ~a " (first item))))
      (printf "  ~a" (first item))))
  (move-to 1 40)
  (printf "│ details:")
  (for ([i (in-range (length (second (list-ref menu-items current-menu))))])
    (move-to (+ i 2) 40)
    (printf "│")
  )
  (define second-menu (second (list-ref menu-items current-menu)))
  (for ([detail second-menu] [i (in-naturals)])
    (move-to (+ i 2) 44)
    (if (= i (list-ref menu-selects current-menu))
      (printf "~a" (format-color-text 37 40 (format "▶ ~a " detail)))
      (printf "  ~a" detail))
  )
  (flush-output)
)

(define (read-key)
  (define ch (read-byte))
  (cond
    [(= ch 27)  ; ESC 序列
      (define ch2 (read-byte))
      (if (= ch2 91)  ; [
        (case (read-byte)
          [(65) 'up]    ; A
          [(66) 'down]  ; B
          [(67) 'right] ; C
          [(68) 'left]  ; D
          [else 'unknown])
        'escape)]
    [(= ch 113) 'quit]   ; q
    [(= ch 104) 'left]   ; h
    [(= ch 106) 'down]   ; j
    [(= ch 107) 'up]     ; k
    [(= ch 108) 'right]  ; l
    [(= ch 13) 'enter]   ; Enter
    [(= ch 3) 'quit]     ; Ctrl+C
    [else 'unknown])
)

(define (set-raw-mode enable?)
  (if enable?
    (system "stty -echo -icanon min 1 time 0")
    (system "stty echo icanon")))

(define (handle-key key)
  (case key
    [(up k)
      (set! current-menu (max 0 (- current-menu 1)))
      #t]
    [(down j)
      (set! current-menu (min (- (length menu-items) 1) (+ current-menu 1)))
      #t]
    [(left h)
      (define old-select (list-ref menu-selects current-menu))
      (define select (max 0 (- old-select 1)))
      (set! menu-selects (list-set menu-selects current-menu select))
      #t]
    [(right h)
      (define old-select (list-ref menu-selects current-menu))
      (define select (min (- (length menu-selects) 1) (+ old-select 1)))
      (set! menu-selects (list-set menu-selects current-menu select))
      #t]
    [(enter)
      #f]
    [(quit)
      #f]  ; 退出
    [else
      #t]
  ))  ; 继续运行

;; 主循环
(define (run-menu)
  (set-raw-mode #t)
  (let loop ()
    (render-menu)
    (define key (read-key))
    (when (handle-key key)
      (loop)))
  (show-cursor)
  (clear-screen)
  (set-raw-mode #f)
)

(module+ main (run-menu))
