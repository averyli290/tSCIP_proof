(define (scale-2 numbers)
    (if (null? numbers)
        '()
        (cons (* (car numbers) 2) (scale-2 (cdr numbers)))
        )
    )

; (define test (list 1 2 3 4))
; (scale-2 test)
