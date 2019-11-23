
(define tolerance 0.00001)

(define (fixed-point f initial-guess)
    (if (< (abs (- (f initial-guess) initial-guess)) tolerance)
        (f initial-guess)
        (begin (display (f initial-guess)) (newline) (fixed-point f (f initial-guess)))
        )
    )


(fixed-point (lambda (x) (/ (+ x (/ 10 x)) 2))
             10.0)
