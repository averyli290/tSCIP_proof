(define (make-empty-environment) '())
(define (top-frame env) (car env))
(define (enclosing-env env) (cdr env))
(define (frame-vars frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (extend-environment vars args base-env)
    (cons (cons vars args) base-env))
