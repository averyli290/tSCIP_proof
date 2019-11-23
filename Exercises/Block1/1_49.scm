(define (fold-left f start-value l)
    (define (iter l)
        (if (null? (cdr l))
            car (l)
            (f (car l) (iter (cdr l)))
            )
        )
    (f start-value (iter l))
    )

(define (my-reverse-left l)
    (fold-left (lambda (x y) (cons y x))
               '()
               l)
    )

;(define test1 (list 1 2 3 4))
;(my-reverse test1)

(define (my-reverse-right l)
    (fold-right (lambda (x y) (cons x))
                '()
                l)
    )

;(define test2 (list 1 2 3 4))
;(my-reverse-right test2)
