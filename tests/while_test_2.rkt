(let ([x -3])
    (begin
        (let ([i 0])
            (while (< i 10)
                (begin
                    (set! x (+ x i))
                    (set! i (+ i 1))))
        )
        x
    )
)