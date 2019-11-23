(define (cube x) (* x x x))
(define (small-enough? x) (< (abs x) 0.00001))

(define (mystery x)
        (if (small-enough? x)
            x
            (let ((z (/ x 3.0)))
            (- (* 3 (mystery z)) (* 4 (cube (mystery z)))))))

(define (f1 c)
    (if (= c 0)
        (display "Done")
        (begin (display (asin (mystery c))) (newline) (f1 (- c 1)))
        )
    )

(f1 )
