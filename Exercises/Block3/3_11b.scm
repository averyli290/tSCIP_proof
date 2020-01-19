(define (make-empty-environment) '())
(define (top-frame env) (car env))
(define (enclosing-env env) (cdr env))
(define (frame-vars frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (set-var-value! var value env)
     (define (set-var-in-frame vars vals)
        (if (null? vars) ; attempts to set var in top frame, if not, returns error 
            'val-not-in-frame
            (if (eq? var (car vars))
                (set-car! vals value)
                (set-var-in-frame (cdr vars) (cdr vals)))))
     (if (null? env) ; return 'error is env is empty, otherwise attempts to set var in top frame
        (error "Variable does not have a value in environment")
        (let ((returned (set-var-in-frame (frame-vars (top-frame env)) (frame-values (top-frame env)))))
            (if (eq? 'val-not-in-frame returned)
                 1
                 2))))
            ;    (set-var-in-frame var (enclosing-env env))))))


(define G (list (cons (list 'a 'b) (list 1 1))))
G
(set-var-value! 'c 2 G)
G
