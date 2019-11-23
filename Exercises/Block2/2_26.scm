(define (make-from-real-imag a b) (cons a b))
(define (real-part z) (car z))
(define (imag-part z) (cdr z))

(define (make-from-mag-ang r theta)
    (make-from-real-imag (* r (cos theta)) (* r (sin theta))))

(define (magnitude z)
    (sqrt (+ (* (real-part z) (real-part z)) (* (imag-part z) (imag-part z)))))

(define (angle z) 
    (atan (real-part z) (imag-part z)))

(define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))
