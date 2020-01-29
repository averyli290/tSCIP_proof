
;;; BEGIN: 3.4

(define let
    (lambda (var1 var2 ... varn)
            (body1 body2 ... bodym)))

;;; END: 3.4

;;; BEGIN: 3.6

(define (make-account balance)
    (define (withdraw amount)
        (set! balance (- balance amount)))
    (define (deposit amount)
        (set! balance (+ balance amount)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'get-balance) balance)
            (else (error "unknown request!"))))
    dispatch)

;;; END: 3.6

;;; BEGIN: 3.7

(define (make-account balance password)
    (define (withdraw amount)
        (set! balance (- balance amount)))
    (define (deposit amount)
        (set! balance (+ balance amount)))
    (define (dispatch m password-input)
        (if (eq? password-input password)
            (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'get-balance) balance)
                (else (error "unknown request!")))
            (display "Incorrect password!")))
    dispatch)

;;; END: 3.7

;;; BEGIN: 3.8

(define (make-account balance password)
    (define password-wrong-inarow 0)
    (define (withdraw amount)
        (set! balance (- balance amount)))
    (define (deposit amount)
        (set! balance (+ balance amount)))
    (define (dispatch m password-input)
        (if (eq? password-input password)
            (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'get-balance) balance)
                (else (error "unknown request!")))
            (if (= (+ 1 password-wrong-inarow) 5)
                (begin (set! password-wrong-inarow 0) (lambda (x) (display "The authorities have been notified.")))
                (begin (set! password-wrong-inarow (+ 1 password-wrong-inarow)) (lambda (x) (display "Incorrect password."))))))
    dispatch)

;;; BEGIN 3.8

(define (make-account balance password)
    (define password-wrong-inarow 0)
    (define (withdraw amount)
        (set! balance (- balance amount)))
    (define (deposit amount)
        (set! balance (+ balance amount)))
    (define (dispatch m password-input)
        (if (eq? password-input password)
            (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'get-balance) balance)
                (else (error "unknown request!")))
            (if (= (+ 1 password-wrong-inarow) 5)
                (begin (set! password-wrong-inarow 0) (lambda (x) (display "The authorities have been notified.")))
                (begin (set! password-wrong-inarow (+ 1 password-wrong-inarow)) (lambda (x) (display "Incorrect password."))))))
    dispatch)

;;; END: 3.8

;;; BEGIN 3.10

;(define x (cons 1 2))
;(define z (cons x x))
;(set-cdr! x 4)
;z
; it does affect z... kinda uncool

;;; END: 3.10

;;; BEGIN: 3.11

;other functions
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

; Exercise 3.11a

(define (lookup-var-value var env)
    (if (null? env) ; return 'error is env is empty, otherwise checks top frame for var
        (error "Variable does not have a value in environment")
        (let ((temp-val (helper-func var (top-frame env))))
            (if (eq? '() temp-val)
                (lookup-var-value var (enclosing-env env))
                (car temp-val)))))

; Exercise 3.11b
(define (set-var-value! var value env)
     (if (null? env) ; return 'error is env is empty, otherwise attempts to set var in top frame
        (error "Variable does not have a value in environment")
        (let ((temp-val (helper-func var (top-frame env))))
            (if (null? temp-val)
                (set-var-value! var value (enclosing-env env))
                (begin (set-cdr! (top-frame env) (replace-value-of-var var value (top-frame env)))
                       (car temp-val))))))

; Exercise 3.11c
(define (define-var! var value env)
    (let ((temp-val (helper-func var (top-frame env))))
        (if (null? temp-val)
            (let ((f-vars (car (cons (frame-vars (top-frame env)) '())))
                  (f-values (car (cons (frame-values (top-frame env)) '()))))
                (set-car! (top-frame env) (cons var f-vars))
                (set-cdr! (top-frame env) (cons value f-values)))
            (set-cdr! (top-frame env) (replace-value-of-var var value (top-frame env)))))
    var)

; Exercise 3.11d
(define (extend-environment vars args base-env)
    (cons (cons vars args) base-env))

;;; END: 3.11


;;; BEGIN: 3.12

;;; -----------------------
;;; FRAMES AND ENVIRONMENTS 
;;; -----------------------

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


;;; ---------------
;;; TAGS AND TABLES
;;; ---------------

(define table '())
(define (set-table tag proc)
    (set! table (cons (cons tag proc) table))) ; Adds procedure to table

;ADD FOLD SAVING IN VIM

(define (set-tag tag contents)
    (cons tag contents))

(define (get-tag expr)
    (car expr))

(define (get-contents expr)
    (cdr expr))


(define (get-table tag)
    (define (inner my-table)
        (cond ((null? my-table) 'none)
              ((eq? (get-tag (car my-table)) tag) (get-contents (car my-table)))
              (else (inner (cdr my-table)))))
    (inner table))


;;; ----------
;;; EVALUATION
;;; ----------

(define (pre-eval expr)
    (if (or (number? expr) (string? expr))
        (list 'self-evaluating expr)
        (if (symbol? expr)
          (list 'variable expr)
          expr)))

(define (new-eval expr env)
    (define to-be-applied (pre-eval expr))
    ((get-table (get-tag to-be-applied)) to-be-applied env))


;;; --------------------
;;; PROCEDURES AND APPLY
;;; --------------------

(define (install-eval-package)
    (set-table 'self-evaluating (lambda (expr env) (car (get-contents expr))))
    (set-table 'variable (lambda (expr env) (lookup-var-value (car (get-contents expr)) env)))
    (set-table 'define (lambda (expr env) (define-var! (car (get-contents expr)) (new-eval (cadr (get-contents expr)) env) env)))
    (set-table 'set! (lambda (expr env) (set-var-value! (car (get-contents expr)) (new-eval (cadr (get-contents expr)) env) env))))

;;; END: 3.12
