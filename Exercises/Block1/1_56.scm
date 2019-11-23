(define tolerance 0.00001)

(define (fixed-point f initial-guess)
    (if (< (abs (- (f initial-guess) initial-guess)) tolerance)
        (f initial-guess)
        (begin (display (f initial-guess)) (newline) (fixed-point f (f initial-guess)))
        )
    )


(fixed-point (lambda (x) (/ (log 1000) (log x))) 5)
; 27 iterations
(display "===break===")
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 5) 
; 7 iterations






