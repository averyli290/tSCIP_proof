(define tolerance 0.00001)

(define (good-enough? guess x)
    (< (abs (- (* guess guess) x)) tolerance))

(define (next-guess guess x)
    (/ (+ guess (/ x guess)) 2))

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (next-guess guess x) x))
    )

(define (sqrt x)
    (sqrt-iter 2.0 x))
