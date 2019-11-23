(define (my-reverse l)
    (define (iter new old)
        (if (null? old)
            new
            (iter (cons (car old) new) (cdr old))
            )
        )
    (iter '() l)
    )

; (define test (list 1 2 3 4))
; (my-reverse test)
