(define (my-append l1 l2)
    (define (iter l1 l2 accum)
        (if (null? l1)
            (if (null? l2)
                accum
                (iter l1 (cdr l2) (cons (car l2) accum))
                )
            (iter (cdr l1) l2 (cons (car l1) accum))
            )
        )

    (define (my-reverse l)
        (define (iter new old)
            (if (null? old)
                new
                (iter (cons (car old) new) (cdr old))))
        (iter '() l))

    (my-reverse (iter l1 l2 '()))
    )

; (define l1 (list 1 2 3 4))
; (define l2 (list 5 6 7 8))

; (my-append l1 l2)
