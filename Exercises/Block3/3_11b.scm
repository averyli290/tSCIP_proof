(define (make-empty-environment) '())
(define (top-frame env) (car env))
(define (enclosing-env env) (cdr env))
(define (frame-vars frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (helper-func var frame)
    (define (var-in-frame vars vals accum) ; Iterates through frame and checks if var is in there, if so, returns list of values looked through with val of var at the top, otherwise, returns '()
        (if (null? vars)
            '()
            (if (eq? var (car vars))
                (append (list (car vals)) accum)
                (var-in-frame (cdr vars) (cdr vals) (append (list (car vals)) accum)))))
    (var-in-frame (frame-vars frame) (frame-values frame) '()))

(define (replace-value-of-var var val frame) ; replaces value of var 
    (define f-vars (frame-vars frame))
    (define f-values (frame-values frame))
    (define (iter vars vals accum)
        (if (eq? (car vars) var)
            (append (reverse-list accum) (list val) (cdr vals))
            (iter (cdr vars) (cdr vals) (append (list (car vals)) accum))))
    (iter f-vars f-values '()))

(define (set-var-value! var value env)
     (if (null? env) ; return 'error is env is empty, otherwise attempts to set var in top frame
        (error "Variable does not have a value in environment")
        (let ((temp-val (helper-func var (top-frame env))))
            (if (null? temp-val)
                (set-var-value! var value (enclosing-env env))
                (begin (set-cdr! (top-frame env) (replace-value-of-var var value (top-frame env)))
                       (car temp-val))))))
