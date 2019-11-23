(define (square n)
    (* n n))

(define (fast-exp-it b n)
    (define (iter total current-base counter)
        (if (= counter 0)
            total
            (if (even? counter)
                (iter total (square current-base) (/ counter 2))
                (iter (* total current-base) current-base (- counter 1)))))

    (iter 1 b n))
