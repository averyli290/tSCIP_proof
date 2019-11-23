(define (square-sum n)
    (if (= n 0)
        n 
        (+ (square-sum (- n 1)) (* n n))))

(define (square-sum-it n)
    (define (iter accum counter)
        (if (= counter 0)
            accum
            (iter (+ accum (* counter counter)) (- counter 1))))
    (iter 0 n))

(square-sum 3)
(square-sum-it 3)
