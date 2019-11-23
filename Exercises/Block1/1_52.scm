(define tolerance 0.00001)

(define (half-interval-method f a b)
    (let (
          (w (/ (+ a b) 2))
          )
        (cond ((< (abs (f w)) tolerance) (+ w 0.0))
              ((< (f w) 0) (half-interval-method f w b))
              (else (half-interval-method f a w))
              )
        )
    )


(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1 2)
