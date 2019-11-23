(define (accumulate-it a b start-value combine-fn term-fn next-fn)
    (define (iter accum counter)
        (if (> counter b)
            accum
            (iter (combine-fn accum (term-fn counter)) (next-fn counter))))
    (iter start-value a) 
    )

(accumulate-it 5 100 (/ (* 2 4) (* 3 3.0)) * 
               (lambda (n) (/ (* (- n 1) (+ n 1)) (* n n)))
               (lambda (n) (+ n 2)))

