(define (make-empty-environment) '())
(define (top-frame env) (car env))
(define (enclosing-env env) (cdr env))
(define (frame-vars frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (lookup-var-value var env)
    (define (var-in-frame vars vals) ; Iterates through frame and checks if var is in there, if so, returns value, otherwise, returns 'none
        (if (null? vars)
            'val-not-in-frame
            (if (eq? var (car vars))
                (car vals)
                (var-in-frame (cdr vars) (cdr vals)))))
    (if (null? env) ; return 'error is env is empty, otherwise checks top frame for var
        (error "Variable does not have a value in environment")
        (let ((val (var-in-frame (frame-vars (top-frame env)) (frame-values (top-frame env)))))
            (if (eq? 'val-not-in-frame val)
                (lookup-var-value var (enclosing-env env))
                val))))

(define (extend-environment vars args base-env))
