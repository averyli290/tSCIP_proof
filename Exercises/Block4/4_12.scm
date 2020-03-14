;;; Implemented cond
;;; implemented cons, car, and cdr
;;; implemented <, >, and list
;;; implemented display using old display

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




;;; -----------------------
;;; FRAMES AND ENVIRONMENTS 
;;; -----------------------

(define frame '())
(define (get-var frame-pair) (car frame-pair))
(define (get-val frame-pair) (cdr frame-pair))

(define (var-in-frame? var)
    (define (iter f)
        (if (null? f)
            #f
            (if (eq? var (get-var (car f)))
                #t
                (iter (cdr f)))))
    (iter frame))


(define (define-var var value)
    (set! frame (cons (cons var value) frame)))

(define (set-var-value! var value)
    (define (iter f) ; assumes that the var is already in the frame, returns previous value set for var
        (if (eq? var (get-var (car f)))
            (set-car! f (cons var value))
            (set-cdr! f (iter (cdr f)))))
    (if (not (var-in-frame? var))
        (error "Variable does not have a value" )
        (iter frame)))

(define (get-var-value var)
    (define (iter f)
        (if (null? f)
            (error "Variable does not have a value")
            (if (eq? var (get-var (car f)))
                (get-val (car f))
                (iter (cdr f)))))
    (iter frame))

(define-var 'test 1)
(var-in-frame? 'test)
(set-var-value! 'test 2)
(get-var-value 'test)


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

;;; ----------
;;; OPERATIONS
;;; ----------

(define (op-name op) (car op))
(define (op-func op) (cdr op))

;;; ----------
;;; EVALUATION
;;; ----------

(define (pre-eval expr)
    (if (symbol? expr)
        (set-tag 'label expr)
        expr))

(define (rml-eval expr)
    (let ((to-be-evaled (pre-eval expr))) ; pre-evals the expr to see if label first, then looks up tag in table and applies corresponding function
        ((get-table (get-tag to-be-evaled)) to-be-evaled)))

(define (add-lisp-func-two-arg name function) ; adds a function that already exists in list that takes in two arguments
    (set-table name (lambda (expr) (function (rml-eval (car (get-contents expr))) (rml-eval (cadr (get-contents expr)))))))

(define (install-rml-eval-package)

    (set-table 'const (lambda (expr) (car (get-contents expr))))
    (add-lisp-func-two-arg '+ +)
    (add-lisp-func-two-arg '- +)
    'done)

(install-rml-eval-package)

(define (make-machine reg-names op-list instructions)

    (define (setup) ; adds all registers to the frame and operations to the table
        (define (iter-reg registers)
            (if (null? registers)
                'none ; dummy return variable
                (begin (define-var (car registers) 0) (iter-reg (cdr registers)))))
        (iter-reg reg-names)
        (define (iter-op operations)
            (if (null? operations)
                'none ; dummy return variable
                (begin (add-lisp-func-two-arg (op-name (car op-list)) (op-func (car op-list))))))
        (iter-op op-list))

    (list setup instructions)) ; returns setup function and instructions at end

(make-machine '(a b) (list (cons '= =)) '())

(define (run machine)
    ())

;(define (set-register-contents machine reg-name value))

;(define (get-register-contents machine reg-name))


;(rml-eval '(const 1))
(rml-eval '(+ (+ (const 1) (const 2)) (const 100)))
(rml-eval '(= (const 1) (const 2)))



