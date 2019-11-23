(define (deep-reverse t)
    (define (iter tree accum)
        (cond ((null? tree) accum)
              ((pair? (car tree)) (iter (cdr tree) (cons (deep-reverse (car tree)) accum)))
              (else (iter (cdr tree) (cons (car tree) accum)))
              )
        )
    (iter t '())
    )

;(define test (list (list 1 2) (list 3 4)))
;(deep-reverse test)



