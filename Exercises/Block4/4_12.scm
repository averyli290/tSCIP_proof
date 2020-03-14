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
            (set! f (cons f (iter (cdr f))))))
    (if (not (var-in-frame? var))
        (begin (display "set-var-value: ") (display var) (error "Variable does not have a value"))
        ;(error "Variable does not have a value")
        (iter frame)))

(define (get-var-value var)
    (define (iter f)
        (if (null? f)
            (begin (display var) (error "Variable does not have a value"))
            (if (eq? var (get-var (car f)))
                (get-val (car f))
                (iter (cdr f)))))
    (iter frame))


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
        (cond ((null? my-table) 'none) ; returns 'none if tag is not in table
              ((eq? (get-tag (car my-table)) tag) (get-contents (car my-table)))
              (else (inner (cdr my-table)))))
    (inner table))

;;; ----------
;;; OPERATIONS
;;; ----------

(define (op-name op) (car op))
(define (op-func op) (cdr op))

;;; -----
;;; STACK
;;; -----



;;; ----------
;;; EVALUATION
;;; ----------

(define (pre-eval expr)
    (if (symbol? expr)
        (set-tag 'label expr) ; if the expression is just a symbol, then it is a label
        expr))

(define (rml-eval expr)
    (let ((to-be-evaled (pre-eval expr))) ; pre-evals the expr to see if label first, then looks up tag in table and applies corresponding function
        (let ((table-value (get-table (get-tag to-be-evaled))))
            (newline)
            (newline)
            (display "to-be-evaled: ")
            (display to-be-evaled) (newline) (display "table-value: ")
            (display table-value)
            ;(display (map (lambda (expr) (rml-eval expr)) (cdr to-be-evaled)))
            (if (eq? table-value 'none)
                (if (eq? (caar expr) 'op) ; if not in table, the expression is in the form of ((op operation) ...) or ((non-op expression)) which should be evaluated by evaluating the car of it (non-op expression)
                    (apply (rml-eval (car expr)) (map (lambda (expr) (rml-eval expr)) (cdr to-be-evaled)))
                    (rml-eval (car to-be-evaled)))
                (table-value to-be-evaled))
            )))

(define (add-lisp-func name function) ; adds a function that already exists in list that takes in two arguments
    (set-table name function))

(define (install-rml-eval-package)

    (set-table 'const (lambda (expr) (car (get-contents expr))))
    (set-table 'reg (lambda (expr) (get-var-value (car (get-contents expr)))))
    (set-table 'label (lambda (expr) (get-var-value (car (get-contents expr)))))
    (set-table 'assign (lambda (expr) (set-var-value! (cadar (get-contents expr)) (rml-eval (cdr (get-contents expr))))))
    (set-table 'op (lambda (expr) (get-table (car (get-contents expr)))))
    (add-lisp-func '+ +)
    (add-lisp-func '- +)
    (add-lisp-func '* *)
    (add-lisp-func '/ /)
    'done)

(install-rml-eval-package)

(define (make-machine reg-names op-list instructions)
    ; returns setup function and instructions as a CONS PAIR at end
    (define (setup) ; adds all registers to the frame and operations to the table
        ; returns a begin statement that runs the iter functions to set up the registers and operations
        (define (iter-reg registers)
            (if (null? registers)
                'none ; dummy return variable
                (begin (define-var (car registers) 0) (iter-reg (cdr registers)))))

        (define (iter-op operations)
            (if (null? operations)
                'none ; dummy return variable
                (begin (newline) (display (op-name (car operations))) (display ": ") (display (op-func (car operations)))
                                                           (add-lisp-func (op-name (car operations)) (op-func (car operations))) (iter-op (cdr operations)))))

        (begin (iter-reg reg-names) (iter-op op-list)))

    (cons setup instructions)) ; returns setup function and instructions as a CONS PAIR at end

(define test (make-machine '(a b c d) (list (cons 'rem modulo) (cons '= =))
                           '(

                             )
                           ))

(define (run-setup machine) ((car machine)))

(define (run machine)
    (run-setup machine) ; runs setup first

    (define (iter-run instructions)
        (if (null? instructions) ; if instructions is empty, returns done
            'done
            (begin (display (car instructions)) (rml-eval (car instructions)) (iter-run (cdr instructions)))))

    (iter-run (cdr machine))
    'done)

;(define (set-register-contents machine reg-name value))

(define (get-register-contents machine reg-name) (get-var-value reg-name))
(run test)
(get-register-contents test 'b)
table



