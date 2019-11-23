(define (enumerate tree)
    (cond ((null? tree) '())
        ((pair? tree) (append (enumerate (car tree)) (enumerate (cdr tree))))
        (else (list tree))))

(define (square-leaves tree)
    (reduce + 0 (map (lambda (x) (* x x))
                     (enumerate tree))
            )
    )

;(square-leaves (list 1 2 (list 3)))
