(define (make-rat x y)
    (cond ((and (< x 0) (> y 0)) (cons (/ (abs x) (gcd x y)) (* (/ y (gcd x y)) -1)))
          ((and (> x 0) (< y 0)) (cons (/ x (gcd x y)) (* (/ y (gcd x y)))))
          (else (cons (/ x (gcd x y)) (/ y (gcd x y))))
        )
    )
