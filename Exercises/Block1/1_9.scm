(define (fib-it n)
    (define (iter counter value-1 value-2)
        (if (= counter n)
            value-1
            (iter (+ 1 counter) value-2 (+ value-1 value-2))))

    (iter 0 0 1))
