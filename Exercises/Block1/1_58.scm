(define tolerance 0.00001)

(define (average-damping f)
    (lambda (x) (/ (+ x (f x)) 2.0)))


(define (fixed-point-from-transform f transform initial-guess)
    (if (< (abs (- ((transform f) initial-guess) initial-guess)) tolerance)
        ((transform f) initial-guess)
        (fixed-point-from-transform f transform ((transform f) initial-guess))
        
      )
    )

(define (fixed-point-from-transform f transform initial-guess)
    (if (good-enough? initial-guess ((transform f) initial-guess)) initial-guess
        (fixed-point-from-transform f transform ((transform f) initial-guess))))

;(fixed-point-from-transform (lambda (w) (/ 1000 (* w w))) average-damping 9.0)

;(fixed-point-from-transform (lambda (w) (/ 1000000 (* w w w w w w w))) (lambda (f) (average-damping (average-damping f))) 9.0)

(define (comp f g)
    (lambda (x) (f (g x))))

(define (nth-power f n)
    (if (= n 0)
        (lambda (x) x)
        (comp f (nth-power f (- n 1)))))

(define (nth-root n r)

    (fixed-point-from-transform (lambda (w) (/ n (exp w (- r 1))))
                                (nth-power average-damping (ceiling (/ (log r) (log 2))))
                                9)
    )


(nth-root 100 2)



