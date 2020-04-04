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
        (begin (newline) (display "set-var-value: ") (display var) (error "Variable does not have a value"))
        ;(error "Variable does not have a value")
        (iter frame)))

(define (get-var-value var)
    (define (iter f)
        (if (null? f)
            (begin (newline) (display "get-var-value: ") (display var) (error "Variable does not have a value"))
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

(define stack '())
(define (push val) (set! stack (cons val stack)))
(define (pop) (let ((toreturn (car stack))) (set! stack (cdr stack)) toreturn))

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
            
            (if (eq? table-value 'none)
                (if (eq? (caar expr) 'op) ; if not in table, the expression is in the form of ((op operation) ...) or ((non-op expression)), both of which should be evaluated by evaluating the car of it (non-op expression)
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
    (set-table 'assign (lambda (expr) (set-var-value! (cadar (get-contents expr)) (rml-eval (cdr (get-contents expr)))))) ; assign sets the value of var, which is (cadar (get-contents expr)), where expr is (assign (reg a) (other thing))
    (set-table 'op (lambda (expr) (get-table (car (get-contents expr)))))
    (add-lisp-func '+ +)
    (add-lisp-func '- +)
    (add-lisp-func '* *)
    (add-lisp-func '/ /)

    ; functions that need instructions
    (set-table 'goto (lambda (instructions expr) (cons (cadr expr) (rml-eval (cadr expr))))) ; need to return ('label (get-var-value label)) because the instructions iteration needs to run (get-var-value label) and it uses cdr 
    (set-table 'branch (lambda (instructions expr) ((get-table 'goto) instructions expr))) ; branch is the same as goto
    (set-table 'test (lambda (instructions expr) (if (rml-eval (cdar instructions))
                                                     instructions
                                                     (cdr instructions))))

    ; stack functions 
    (set-table 'push (lambda (expr) (push (rml-eval (get-contents expr)))))
    (set-table 'pop (lambda (expr) (set-var-value! (cadar (get-contents expr)) (pop)))) ; accesses the reg symbol and binds it to (pop), the top value of the stackj

    ; perform (for writing or running other lisp functions)
    (set-table 'perform (lambda (expr) (apply (rml-eval (cadr expr)) (map (lambda (element) (rml-eval element)) (cddr expr))))) ; evaluates (op <operation-name>) and applies it to the elements that come after after evaluating the elements with a map

    'done)

(install-rml-eval-package)

(define (get-breakpoints machine) (cadr machine))
(define (get-instructions machine) (car machine))

(define (make-machine reg-names op-list instructions)
    (define (setup) ; adds all registers to the frame and operations to the table
        ; returns a begin statement that runs the iter functions to set up the registers, operations, and find all locations for label and sets thim in the table
        (define (iter-reg registers)
            (if (null? registers)
                'dummy ; dummy return variable
                (begin (define-var (car registers) 0) (iter-reg (cdr registers)))))

        (define (iter-op operations)
            (if (null? operations)
                'dummy ; dummy return variable
                (begin (newline) (display (op-name (car operations))) (display ": ") (display (op-func (car operations)))
                                   (add-lisp-func (op-name (car operations)) (op-func (car operations))) (iter-op (cdr operations)))))

        (define (iter-label code)
            (if (null? code)
                'dummy
                (if (symbol? (car code)) ; tests to see if there is a label
                    (begin (define-var (car code) (cdr code)) (iter-label (cdr code))) ; adds the label-code pair to the table
                    (iter-label (cdr code)))))

        (begin (iter-reg reg-names) (iter-op op-list) (iter-label instructions)))

    (setup) ; runs the setup

    (define (print-variables)
        (define (iter vars)
            (if (null? vars)
                (display " ")
                (begin (newline) (display (car vars)) (display ": ") (display (get-var-value (car vars))) (iter (cdr vars)))))
        (display "Variables: ")
        (iter reg-names))

    (define breakpoints '())
    (list instructions breakpoints print-variables)) ; returns instructions and breakpoints (empty list for now) as a CONS PAIR at end


(define (run machine)
    
    (define state 'none)
    (define (set-state! newstate) (set! state newstate)) ; sets the state of the machine

    ; Monitoring
    (define (print-vars) ((caddr machine)))
    (define (print-stack) (begin (display "Stack: ") (display stack)))
    (define (print-current-line instructions) (begin (display "Current line: ") (display (car instructions))))
    (define (print-instructions instructions) (begin (display "Instructions") (display instructions)))

    (define (master-print instructions)
        (print-current-line instructions)
        (newline)
        (print-vars)
        (newline)
        (print-stack)
        (newline)
        )

    (define (is-breakpoint? code)
        (define (iter-check breakpoints)
            (if (null? breakpoints)
                #f
                (if (equal? (car breakpoints) code)
                    #t
                    (iter-check (cdr breakpoints)))))
        (iter-check (get-breakpoints machine)))

    (define (needs-instructions? expr) ; checks to see if the expression needing to be evaluated needs to take instructions (except for label)
        (define needs-instructions (list 'test 'goto 'branch)) ; creates a list whose elements are the types of expressions that needs instructions
        (define (check l) ; function to go through list and check if the type of expression expr is is in the list
            (cond ((null? l) #f)
                  ((eq? (car l) (car expr)) #t)
                  (else (check (cdr l)))))
        (if (symbol? expr) ; checking for labels, they cannot be checking by check function because (car 'label) returns an error
            #f
            (check needs-instructions)))

    (define (iter-run instructions)
        (newline)
        (cond ((null? instructions) (set! state 'done))
              ((and (is-breakpoint? instructions) (not (eq? state 'first-line))) (begin (set! state 'breakpoint) (set-car! machine instructions)))
              ((needs-instructions? (car instructions)) (begin (master-print instructions) (set! instructions ((get-table (get-tag (car instructions))) instructions (car instructions)))))
              ((symbol? (car instructions)) (begin (display (car instructions)) 'label-line))
              (else (begin (master-print instructions) (rml-eval (car instructions))))
              )

        (if (eq? state 'first-line) ; once the program has gone past the first line, sets the state to 'running
            (set-state! 'running))

        (if (null? instructions)
            'done
            (if (not (eq? state 'running))
                state
                (iter-run (cdr instructions))))
        )

    (set-state! 'first-line) ; the state of the machine either running, breakpoint, or done
    (iter-run (get-instructions machine))
    state)

(define (set-register-contents machine reg-name value) (set-var-value! reg-name value))

(define (get-register-contents machine reg-name) (get-var-value reg-name))

;;; -----------
;;; BREAKPOINTS
;;; -----------



(define (set-breakpoint machine label n)

    (define (post-iter-label code counter) ; goes to x-1 lines of code after the label
        (if (= counter 0)
            code
            (post-iter-label (cdr code) (- counter 1))))

    (define (iter-label code) ; find the labels and runs post-iter-label on the code if found
        (if (null? code)
            '()
            (if (eq? (car code) label)
                (post-iter-label code n)
                (begin (newline) (display code) (iter-label (cdr code))))))

    (let ((value (iter-label (car machine))))
         (if (null? value)
            'no-breakpoint-set
            (set-cdr! machine (cons value (cdr machine))))
         ))

(define (cancel-breakpoint machine label n)

    (define (post-iter-label code counter) ; goes to x-1 lines of code after the label
        (if (= counter 0)
            code
            (post-iter-label (cdr code) (- counter 1))))

    (define (iter-label code) ; find the labels and runs post-iter-label on the code if found
        (if (null? code)
            '()
            (if (eq? (car code) label)
                (post-iter-label code n)
                (begin (newline) (display code) (iter-label (cdr code))))))

    (define (remove-breakpoint to-remove breakpoints) ; searches for breakpoint and if it is found, returns the list without the breakpoint in it
        (if (null? breakpoints)
            '()
            (if (eq? to-remove (car breakpoints))
                (cdr breakpoints) ; skips the element when it is found to remove it
                (cons (car breakpoints) (remove-breakpoint to-remove (cdr breakpoints))))))

    (let ((value (iter-label (car machine))))
         (if (null? value)
            'no-breakpoint-found
            (let ((breakpoints (get-breakpoints machine)))
                (
                 (set! breakpoints (remove-breakpoint (value (get-breakpoints machine))))
                 )))
            ;(set-cdr! machine (remove-breakpoint value (cdr machine))))
         ))


(define gcd-machine (make-machine
                      '(a b t)
                      (list (cons 'rem modulo) (cons '= =))
                      '(test-label
                         (test (op =) (reg b) (const 0))
                         (branch (label done-label))
                         (assign (reg t) (op rem) (reg a) (reg b))
                         (assign (reg a) (reg b))
                         (assign (reg b) (reg t))
                         (goto (label test-label))
                         done-label)))


(set-register-contents gcd-machine 'a 511)
(set-register-contents gcd-machine 'b 371)
(run gcd-machine) ; returns 'done


