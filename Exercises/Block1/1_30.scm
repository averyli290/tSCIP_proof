(define (last-elt l)
    (if (null? l)
        (error "empty list"))
    (if (null? (cdr l))
        (car l)
        (last-elt (cdr l))
        )
    )

; (define test (list 1 2 3 4))
; (last-elt test)
