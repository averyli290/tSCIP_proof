(define (range n)
    (define (iter counter accum)
        (if (= counter -1)
            accum
            (iter (- counter 1) (cons counter accum))
            )
        )
    (iter (- n 1) '())
    )

; (range 5)
