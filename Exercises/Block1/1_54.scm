; We have that x^2=n
; thefore our function is n/x=x


(define (fixed-point-sqrt x)
    (fixed-point (lambda (t) (/ t x))
                 2)
    )

;(fixed-point-sqrt 10.0)

;the value jumps around and doesn't get a correct value

