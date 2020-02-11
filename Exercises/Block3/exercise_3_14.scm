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

;;; ---------------------------
;;; PROCEDURE OBJECTS AND APPLY 
;;; ---------------------------

; helper functions
(define (make-procedure params body env) (list 'procedure params body env))
(define (procedure-vars proc) (cadr proc))
(define (procedure-body proc) (caddr proc))
(define (procedure-env proc) (cadddr proc))
(define (make-primitive-procedure code) (list 'primitive code))
(define (primitive-procedure-code proc) (cadr proc))
(define (primitive? proc) (eq? (car proc) 'primitive))



;;; -----------
;;; EVALUTATION
;;; -----------

(define (pre-eval expr)
    (if (or (number? expr) (string? expr))
        (list 'self-evaluating expr)
        (if (symbol? expr)
          (list 'variable expr)
          expr)))

(define (new-eval expr env)
    (define to-be-applied (pre-eval expr))
    (if (eq? 'none (get-table (get-tag to-be-applied)))
        (new-eval (cons 'application expr) env)
        ((get-table (get-tag to-be-applied)) to-be-applied env)))

(define (eval-sequence exprs env)
    (define (iter to-be-new-evalued) ; evaluates all of the items and returns the evaluation of the last item.
        (if (null? (cdr to-be-new-evalued))
            (new-eval (car to-be-new-evalued) env)
            (begin (new-eval (car to-be-new-evalued) env) (iter (cdr to-be-new-evalued)))))
    (if (null? (get-contents exprs))
        'unspecified-return-value ; throws error if just "(begin)" with no arguments
        (iter (get-contents exprs))))

; should be part of PROCEDURE OBJECTS AND APPLY, but requires eval-sequence
(define (new-apply proc vals) ; Evaluates proc in a new frame with variables bound, checks if procedure is primitive
    (if (primitive? proc)
        (apply (primitive-procedure-code proc) vals)
        (eval-sequence (procedure-body proc) (extend-environment (procedure-vars proc) vals (procedure-env proc)))))

(define (install-eval-package)
    (define (eval-begin expr env) ; Adding this func to table instead of just writing entire thing in lambda for "begin" function
        (eval-sequence expr env))

    ; setting all the tags that can be evaluated in the table
    (set-table 'self-evaluating (lambda (expr env) (car (get-contents expr))))
    (set-table 'variable (lambda (expr env) (lookup-var-value (car (get-contents expr)) env)))
    (set-table 'define (lambda (expr env) (define-var! (car (get-contents expr)) (new-eval (cadr (get-contents expr)) env) env)))
    (set-table 'set! (lambda (expr env) (set-var-value! (car (get-contents expr)) (new-eval (cadr (get-contents expr)) env) env)))
    (set-table 'quote (lambda (expr env) (car (get-contents expr))))
    (set-table 'begin (lambda (expr env) (eval-begin expr env)))
        ; ^ returns 'true-object or 'false-object for true or false in lisp-1,
        ;where true? and false? functions return #t and #f iff the input are 'true-object and 'false-object respectievly

    (define (lambda-vars expr) (cadr expr)) ;lambda helper functions
    (define (lambda-body expr) (cdr expr))
    (set-table 'lambda (lambda (expr env) (make-procedure (lambda-vars expr) (lambda-body expr) env))) ;means eval-lambda
    (define (application-proc expr) (car (get-contents expr))) ;application helper functions
    (define (application-args expr) (cdr (get-contents expr)))
    (define (eval-application expr env) ; passes the application-procedure and the evaluated arguments into the new-apply function to apply the function to the evaluated values
        (new-apply (lookup-var-value (application-proc expr) env) (map (lambda (arg) (new-eval arg env)) (application-args expr))))
    (set-table 'application (lambda (expr env) (eval-application expr env)))

    (define (predicate expr) (car (get-contents expr))) ; helper functions for defining "if" function
    (define (consequent expr) (cadr (get-contents expr)))
    (define (has-alternate? expr) (not (null? (cddr (get-contents expr)))))
    (define (alternate expr) (caddr (get-contents expr)))
    (define (true? p) (if (eq? p 'true-object) #t))
    (define (false? p) (if (eq? p 'false-object) #f))
    (define (eval-if expr env)
        (if (eq? #t (true? (new-eval (predicate expr) env))) ; if not checking whether true? is #t, then #!unspecific will be returned for true and then run (new-eval (consequent expr) env)
            (new-eval (consequent expr) env)
            (if (has-alternate? expr)
                (new-eval (alternate expr) env)
                'unspecified-return-vaue)))

    (set-table 'if (lambda (expr env) (eval-if expr env)))
    )


(install-eval-package) ; installing eval package to add functions to table


;;; --------
;;; PRINTING
;;; --------

(define (sanitize-output exp)
    (if (and (pair? exp) (eq? 'procedure (get-tag exp)))
        (list 'procedure (procedure-vars exp) (procedure-body exp) 'enclosing-env)
        exp))

(define (print-environment env)
    (define (print-frame vars values)
        (if (null? vars)
            (begin (newline) (display "--------"))
            (begin (newline) (display (car vars)) (display ": ") (display (sanitize-output (car values)))
                (print-frame (cdr vars) (cdr values)))))
    (if (null? env)
        'done
        (begin (newline) (display "Next frame:") (newline)
            (print-frame (car (top-frame env)) (cdr (top-frame env)))
            (print-environment (enclosing-env env)))))

;;; -----
;;; SETUP
;;; -----

(define (setup-global-environment)
  ; returns a premade environment with primitives set and also calls install-eval-package
    (define primitives '())
    (define (add-primitive name code) (set! primitives (cons (cons name (make-primitive-procedure code)) primitives)))
    (define (add-primitive-predicate name code) (set! primitives (cons (cons name (make-primitive-procedure (lambda (x y) (if (code x y) 'true-object 'false-object)))) primitives))) ; modifies code to return 'true-object and 'false-object for 2 object comparisons
    (add-primitive '+ +)
    (add-primitive '- -)
    (add-primitive '* *)
    (add-primitive '/ /)
    (add-primitive-predicate '= =)

    (define global (make-environment))

    (define (bind-primitives env primitives-list)
        (if (null? primitives-list)
            env
            (begin 
                (define-var! (car (car primitives-list)) (cdar primitives-list) env)
                (bind-primitives env (cdr primitives-list)))))

    (bind-primitives global primitives)
    (install-eval-package)

    global)


;;; -------
;;; TESTING
;;; -------


(define global (setup-global-environment))
(new-eval '(define fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))) global)
(new-eval '(fact 6) global)
(print-environment global)
