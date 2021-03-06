(define (set-tag tag object) (cons tag object))
(define (get-tag tagged-object) (car tagged-object))
(define (get-contents tagged-object) (cdr tagged-object))
(define (rectangular? tagged-object) (eq? 'rect (get-tag tagged-object)))
(define (polar? tagged-object) (eq? 'polar (get-tag tagged-object)))

(define (real-part-rect z) (car (get-contents z)))
(define (imag-part-rect z) (cdr (get-contents z)))
(define (real-part-polar z) (* (car (get-contents z)) (cos (cdr (get-contents z)))))
(define (imag-part-polar z) (* (car (get-contents z)) (sin (cdr (get-contents z)))))

(define (real-part z)
    (if (rectangular? z)
        (real-part-rect z)
        (real-part-polar z)))

(define (imag-part z)
    (if (rectangular? z)
        (imag-part-rect z)
        (imag-part-polar z)))

(define (make-from-real-imag a b)
    (set-tag 'rect (cons a b)))
