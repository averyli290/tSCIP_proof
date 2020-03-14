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

(define (make-rml-frame) '())
(define (get-var frame-pair) (car frame-pair))
(define (get-val frame-pair) (cdr frame-pair))

(define (var-in-frame? var rml-frame)
    (define (iter f)
        (if (null? f)
            #f
            (if (eq? var (get-var (car f)))
                #t
                (iter (cdr f)))))
    (iter rml-frame))


(define (define-var var value rml-frame)
    (set! rml-frame (cons (cons var value) rml-frame)))

(define (set-var-value! var value)
    (define (iter f) ; assumes that the var is already in the frame, returns previous value set for var
        (if (eq? var (get-var (car f)))
            (set-car! f (cons var value))
            (set-cdr! f (iter (cdr f)))))
    (if (not (var-in-frame? var))
        (begin (display var) (error "Variable does not have a value"))
        (iter rml-frame)))

(define (get-var-value var rml-frame)
    (define (iter f)
        (if (null? f)
            (begin (display var) (error "Variable does not have a value"))
            (if (eq? var (get-var (car f)))
                (get-val (car f))
                (iter (cdr f)))))
    (iter rml-frame))


;;; ---------------
;;; TAGS AND TABLES
;;; ---------------

(define (make-rml-table) '())
(define (set-table tag proc table)
    (set! table (cons (cons tag proc) table))) ; Adds procedure to table

(define (set-tag tag contents)
    (cons tag contents))

(define (get-tag expr)
    (car expr))

(define (get-contents expr)
    (cdr expr))

(define (get-table tag table)
    (define (inner my-table)
        (cond ((null? my-table) (begin (display tag) 'not-in-table)) ; returns 'not-in-table if tag is not in table
              ((eq? (get-tag (car my-table)) tag) (get-contents (car my-table)))
              (else (inner (cdr my-table)))))
    (inner table))

(define testtable (make-rml-table))
;(set! testtable (cons (cons 'test (lambda () 1)) testtable))
(set-table 'test (lambda () 1) testtable)
testtable

(define (test l)
    (set! l (list 1)))
(define l1 (list 1 2))
(display (test l1))
l1

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
        (set-tag 'label expr) ;(begin (newline) (display expr) (set-tag 'label expr))
        expr))

(define (rml-eval expr table)
    (let ((to-be-evaled (pre-eval expr))) ; pre-evals the expr to see if label first, then looks up tag in table and applies corresponding function
        (newline)
        (display "Tobeevaled: ")
        (display expr)
        ((get-table (get-tag to-be-evaled) table) to-be-evaled)))

(define (add-lisp-func-two-arg name function table) ; adds a function that already exists in list that takes in two arguments
    (set-table name (lambda (expr) (function (rml-eval (car (get-contents expr)) table) (rml-eval (cadr (get-contents expr)) table))) table))

(define (install-rml-eval-package table)

    (set-table 'const (lambda (expr) (car (get-contents expr))) table)
    (set-table 'reg (lambda (expr) (car (get-contents expr))) table)
    (set-table 'assign (lambda (expr) (set-var-value! (rml-eval (car (get-contents expr)) table) (rml-eval (cadr (get-contents expr)) table))) table)
    (add-lisp-func-two-arg '+ + table)
    (add-lisp-func-two-arg '- + table)
    (newline)
    (display table)
    'done)


;(define (run-setup machine) (display machine) (begin ((caar machine) (cdar machine)) ((cadr machine) (cddr machine))))

(define (make-machine reg-names op-list instructions)
    (define machine-table (make-rml-table))
    (define machine-frame (make-rml-frame))

    (install-rml-eval-package machine-table)

    (define (setup) ; adds all registers to the frame and operations to the table
        (define (iter-reg registers)
            (if (null? registers)
                'none ; dummy return variable
                (begin (define-var (car registers) 0 machine-frame) (iter-reg (cdr registers)))))

        (define (iter-op operations)
            (if (null? operations)
                'none ; dummy return variable
                (begin (add-lisp-func-two-arg (op-name (car op-list)) (op-func (car op-list)) machine-table))))

        (begin (iter-reg reg-names) (iter-op op-list))) ; returns begin statement that runs the iter functions to set up the registers and operations

    (list setup instructions machine-table machine-frame)) ; returns setup function and instructions as a CONS PAIR at end

(define (run-setup machine) ((car machine)))
(define (get-machine-table machine) (caddr machine))

(define (run machine)
    (run-setup machine) ; runs setup first
    (let ((table (get-machine-table machine)))
    
    (define (iter-run instructions)
        (if (null? instructions) ; if instructions is empty, returns done
            'done
            (begin (display (car instructions)) (rml-eval (car instructions) table) (iter-run (cdr instructions)))))
     
    (iter-run (cadr machine)))
    )

;(define test (make-machine '(a b) (list (cons '= =))
;                           '(
;                             (assign (reg a) (const 1))
;                             )
;                           ))


;(run test)


;(define (set-register-contents machine reg-name value))

;(define (get-register-contents machine reg-name) ())

