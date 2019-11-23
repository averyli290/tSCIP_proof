
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (incr x) (+ x 1))
(define (identity x) x)

(define (sum-rec a b term-fn)
    (if (> a b)
        0
        (+ (term-fn a) (sum-rec (+ a 1) b term-fn))))

(define (sum-it a b term-fn)
    (define (iter accum counter)
        (if (> counter b)
            accum
            (iter (+ accum (term-fn counter)) (+ counter 1))))
    (iter 0 a))
