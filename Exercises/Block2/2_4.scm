
(define (my-filter pred l)
    (define (iter accum l)
        (cond ((null? l) accum)
              ((pred (car l)) (iter (cons (car l) accum) (cdr l)))
              (else (iter accum (cdr l)))
            )
        )
    (reverse (iter '() l))
    )

;(my-filter (lambda (x) (> x 0)) (list -1 2 3 -4))
