(define (my-map numbers f)
    (if (null? numbers)
        '()
        (cons (f (car numbers)) (my-map (cdr numbers) f))
        )
    )
;; Test
; (define test (list 1 2 3 4))
; (my-map test (lambda (x) (* x x)))
