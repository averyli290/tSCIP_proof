(define (square x) (* x x))
(define (cube x) (* x x x))
(define (incr x) (+ x 1))
(define (identity x) x)

(define (accumulate-it a b start-value combine-fn term-fn next-fn)
    (define (iter accum counter)
        (if ( (if (< a b) > <) counter b)
            accum
            (iter (combine-fn accum (term-fn counter)) (next-fn counter))
        )
    )
    (iter start-value a) 
)


(define (integrate f a b n)
    (accumulate-it a b 0 +
            (lambda (x) (* (/ (f x) n) (- b a)))
            (lambda (x) (+ x (/ (- b a) n)))
    )
)

(integrate square 0 2 1000.0)
