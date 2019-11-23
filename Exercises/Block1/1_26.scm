(define (new-cons x y)
    (define (dispatch m)
        (cond ((= m 0) x)
                ((= m 1) y)))
    dispatch)

(define (new-car p)
    (p 0))

(define (new-cdr p)
    (p 1))

; (define a (new-cons 5 6))
; (define b (new-car a))
; (define c (new-cdr a))

; b
; c
