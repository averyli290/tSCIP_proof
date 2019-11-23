(define (map-tree t f)
    (cond ((null? t) '())
        ((pair? t) (cons (map-tree (car t) f) (map-tree (cdr t) f)))
        (else (f t))
        )
    )


;(define test (list 1 2 (list 3 4) (list 5 6)))
;(map-tree test (lambda (x) (+ 1 x)))
