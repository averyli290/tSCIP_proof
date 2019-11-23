(define (triangular n)
    (if (> n 0)
        (+ n (triangular (- n 1)))
        0))
