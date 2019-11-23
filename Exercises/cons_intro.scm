(define (my-list-ref l n)
    (if (= n 0)
        (car l) 
        (my-list-ref (cdr l) (- n 1))
        )
    )

(define (my-length l)
    (if (null? l)
        0
        (+ 1 (my-length (cdr l)))
        ) 
    )

(define l (list 6 7 8))

(my-length l)
