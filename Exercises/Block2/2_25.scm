(define (make-from-real-imag a b) (cons a b))
(define (real-part z) (car z))
(define (imag-part z) (cdr z))

(define (make-from-mag-ang r theta)
    (make-from-real-imag (* r (cos theta)) (* r (sin theta))))

(define (magnitude z)
    (sqrt (+ (* (real-part z) (real-part z)) (* (imag-part z) (imag-part z)))))

(define (angle z) 
    (atan (real-part z) (imag-part z)))
