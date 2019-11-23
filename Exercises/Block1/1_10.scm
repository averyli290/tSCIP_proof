(define (exp b n)
    (if (= 0 n)
        1
        (* b (exp b (- n 1)))))

(define (exp-it b n)
    (define (iter accum counter)
        (if (= counter 0)
            accum
            (iter (* accum b) (- counter 1))))
    (iter 1 n))
