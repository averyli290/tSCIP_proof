(define (new-cons-2 x y)
    (lambda (m) (m x y)))

(define (new-car-2 p)
    (p (lambda (x y) x)))

(define (new-cdr-2 p)
    (p (lambda (x y) y)))

; (define a (new-cons-2 1 2))
; (define b (new-car-2 a))
; (define c (new-cdr-2 a))
; b
; c
