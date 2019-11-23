(define (make-from-mag-ang a b) (cons a b))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (real-part z) (* (car z) (cos (cdr z))))
(define (imag-part z) (* (car z) (sin (cdr z))))
(define (make-from-real-imag a b) (make-from-mag-ang (sqrt (+ (* a a) (* b b))) (atan a b)))

(define (add-complex z1 z2)
      (make-from-real-imag (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
      (make-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
      (make-from-mag-ang (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
      (make-from-mag-ang (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))

;(define test1 (make-from-mag-ang 1 (/ 3.1415 2)))
;(define test2 (make-from-mag-ang 1 3.1415))

;(add-complex test1 test2)
;(sub-complex test1 test2)
;(mul-complex test1 test2)
;(div-complex test1 test2)
