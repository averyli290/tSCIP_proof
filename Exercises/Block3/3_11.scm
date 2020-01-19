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

(define (lookup-var-value var env)
    (if (null? env) ; return 'error is env is empty, otherwise checks top frame for var
        (error "Variable does not have a value in environment")
        (let ((temp-val (helper-func var (top-frame env))))
            (if (eq? '() temp-val)
                (lookup-var-value var (enclosing-env env))
                (car temp-val)))))

(define (set-var-value! var value env)
     (if (null? env) ; return 'error is env is empty, otherwise attempts to set var in top frame
        (error "Variable does not have a value in environment")
        (let ((temp-val (helper-func var (top-frame env))))
            (if (eq? '() temp-val)
                (set-var-value! var value (enclosing-env env))
                (let ((first-val (car temp-val)))
                    (set! first-val value))))))

(define define-var! (var value env)
    (let ((temp-val (helper-func var (top-frame env))))
        (if (null? temp-val)
            (begin (append (list var) (frame-vars (top-frame env)))
                   (append (list value) (frame-values (top-frame env)))
                   'var)
            (set-car! temp-val value)))
    var)

(define (extend-environment vars args base-env)
    (cons (cons vars args) base-env))



(define G (make-empty-environment))
(set! G (extend-environment '(a b) (list 1 2) G))
(define-var! 'a 5 G)
G
