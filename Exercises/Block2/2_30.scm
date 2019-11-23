; Base functions
(define (set-tag tag object) (cons tag object))
(define (get-tag tagged-object) (car tagged-object))
(define (get-contents tagged-object) (cdr tagged-object))
(define (rectangular? tagged-object) (eq? 'rect (get-tag tagged-object)))
(define (polar? tagged-object) (eq? 'polar (get-tag tagged-object)))

; Specific functions (real and imag)
(define (real-part-rect z) (car (get-contents z)))
(define (imag-part-rect z) (cdr (get-contents z)))
(define (real-part-polar z) (* (car (get-contents z)) (cos (cdr (get-contents z)))))
(define (imag-part-polar z) (* (car (get-contents z)) (sin (cdr (get-contents z)))))

; General functions (real and imag)
(define (real-part z)
    (if (rectangular? z)
        (real-part-rect z)
        (real-part-polar z)))

(define (imag-part z)
    (if (rectangular? z)
        (imag-part-rect z)
        (imag-part-polar z)))

; Making rect form complex number
(define (make-from-real-imag a b)
    (set-tag 'rect (cons a b)))


; Specific functions (magnitude and angle)
(define (magnitude-rect z)
      (sqrt (+ (* (real-part-rect z) (real-part-rect z)) (* (imag-part-rect z) (imag-part-rect z)))))

(define (magnitude-polar z) (car (get-contents z)))
(define (angle-rect z)
    (atan (imag-part-rect z) (real-part-rect z)))
(define (angle-polar z) (cdr (get-contents z)))

; General functions (magnitude and angle)
(define (magnitude z)
    (if (rectangular? z)
        (magnitude-rect z)
        (magnitude-polar z)))

(define (angle z)
    (if (rectangular? z)
        (angle-rect z)
        (angle-polar z)))

; Making polar form complex number
(define (make-from-mag-ang a b)
    (set-tag 'polar (cons a b)))


(define test (make-from-real-imag 1 1))
(real-part test)
(imag-part test)
(magnitude test)
(angle test)


