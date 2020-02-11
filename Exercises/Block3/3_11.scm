(define (make-environment) (list (cons '() '())))
(define (top-frame env) (car env))
(define (enclosing-env env) (cdr env))
(define (frame-vars frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (helper-func var frame)
    (define (var-in-frame vars vals accum) ; Iterates through frame and checks if var is in there, if so, returns list of values looked through with val of var at the top, otherwise, returns a #f in first element of pair 
        (if (null? vars)
            (cons #f '())
            (if (eq? var (car vars))
                (cons #t (append (list (car vals)) accum))
                (var-in-frame (cdr vars) (cdr vals) (append (list (car vals)) accum)))))
    (var-in-frame (frame-vars frame) (frame-values frame) '()))

(define (reverse-list l)
    (define (iter toreverse accum)
        (if (null? toreverse)
            accum
            (iter (cdr toreverse) (cons (car toreverse) accum))))
    (iter l '()))

(define (replace-value-of-var var val frame) ; replaces value of var 
    (define f-vars (frame-vars frame))
    (define f-values (frame-values frame))
    (define (iter vars vals accum)
        (if (eq? (car vars) var)
            (append (reverse-list accum) (list val) (cdr vals))
            (iter (cdr vars) (cdr vals) (append (list (car vals)) accum))))
    (iter f-vars f-values '()))

(define (lookup-var-value var env)
    (if (null? env) ; return 'error is env is empty, otherwise checks top frame for var
        (error "Variable does not have a value in environment")
        (let ((temp-val (helper-func var (top-frame env))))
            (display temp-val)
            (if (not (car temp-val))
                (lookup-var-value var (enclosing-env env))
                (cadr temp-val)))))

(define (set-var-value! var value env)
     (if (null? env) ; return 'error is env is empty, otherwise attempts to set var in top frame
        (error "Variable does not have a value in environment")
        (let ((temp-val (helper-func var (top-frame env))))
            (if (not (car temp-val))
                (set-var-value! var value (enclosing-env env))
                (begin (set-cdr! (top-frame env) (replace-value-of-var var value (top-frame env)))
                       (cadr temp-val))))))

(define (define-var! var value env)
    (let ((temp-val (helper-func var (top-frame env))))
        (if (not (car temp-val))
            (let ((f-vars (car (cons (frame-vars (top-frame env)) '())))
                  (f-values (car (cons (frame-values (top-frame env)) '()))))
                (set-car! (top-frame env) (cons var f-vars))
                (set-cdr! (top-frame env) (cons value f-values)))
            (set-cdr! (top-frame env) (replace-value-of-var var value (top-frame env)))))
    var)

(define (extend-environment vars args base-env)
    (cons (cons vars args) base-env))



(define G (make-environment))
(set! G (extend-environment '(a b c d) (list 1 2 3 4) G))
(set-var-value! 'a 5 G)
(define-var! 'e '(4 20) G)
(lookup-var-value 'e G)
G

