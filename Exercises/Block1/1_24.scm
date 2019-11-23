(define (my-length l)
    (define (iter lst len)
        (if (null? lst)
            len
            (iter (cdr lst) (+ len 1))
            )
        )
    (iter l 0)
    )

; (define l (list 1 2 3 4 5))
; (my-length l)
