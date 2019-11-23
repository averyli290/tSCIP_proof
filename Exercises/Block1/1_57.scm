(define tolerance 0.00001)

(define (average-damping f)
    (lambda (x) (/ (+ x (f x)) 2)))


(define (fixed-point-from-transform f transform initial-guess)
    (if (< (abs (- ((transform f) initial-guess) initial-guess)) tolerance)
        ((transform f) initial-guess)
        (fixed-point-from-transform f transform ((transform f) initial-guess))
        
      )
    )

;(fixed-point-from-transform (lambda (w) (/ 100 (* w w w)))
;                            (lambda (f) (average-damping (average-damping f)))
;                            9.0)

