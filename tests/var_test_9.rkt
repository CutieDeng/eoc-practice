(let ([x 42]) 
    (let ([y x]) 
        (+ 
            (let ([x 41]) 0) 
            x
        )
    )
)