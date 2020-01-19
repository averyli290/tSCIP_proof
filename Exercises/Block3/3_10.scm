(define x (cons 1 2))
(define z (cons x x))
(set-cdr! x 4)
z
; it does affect z... I guess it makes sense
