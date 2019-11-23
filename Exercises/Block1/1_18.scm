(define (square x) (* x x))
(define (cube x) (* x x x))
(define (incr x) (+ x 1))
(define (identity x) x)

(define (accumulate-rec a b start-value combine-fn term-fn next-fn)
    (if (> a b)
        start-value
        (combine-fn (term-fn a)
            (accumulate-rec (next-fn a) b start-value combine-fn term-fn next-fn))
        )
    )

(define (accumulate-it a b start-value combine-fn term-fn next-fn)
    (define (iter accum counter)
        (if ( (if (< a b) > <) counter b)
            accum
            (iter (combine-fn accum (term-fn counter)) (next-fn counter)))
        )
    (iter start-value a) 
    )

;(accumulate-it 1 5 1 * (lambda (x) x) (lambda (x) (+ 1 x)))
