(load "RM_simulator.scm")
(load "helper-functions.scm")


;#####################
;# VECTORS & REGISTERS
;#####################

(define MAX_MEMORY_SIZE 15)

;##########
;# POINTERS
;##########

(define (pointer? obj) (pair? obj))
(define (address pointer) (cdr pointer))
(define (make-pointer address) (cons 'p address))
(define (make-pointer obj) (cons 'p obj))

;####################
;# MODIFIED FRAME API
;####################

;(define (top-frame env) (vector-ref glob-mac-cars (address env)))
(define (top-frame env) (vector-ref glob-mac-cars (address env)))
(define (enclosing-env env) (vector-ref glob-mac-cdrs (address env)))
(define (frame-vars frame) (vector-ref glob-mac-cars (address frame)))
(define (frame-values frame) (vector-ref glob-mac-cdrs (address frame)))

(define glob-mac-cars (make-vector MAX_MEMORY_SIZE 'undef))
(define glob-mac-cdrs (make-vector MAX_MEMORY_SIZE 'undef))

;;; frame procedures
(define (make-environment)
    (let ((f (address (get-register-contents glob-mac 'free))))
        (vector-set! glob-mac-cars f (make-pointer (+ 1 f))) ; setting up f to be a list (constructed in RML frame api) that points to pairs representing frames
        (vector-set! glob-mac-cdrs f '()) ; signalling the end of the environment list
        (vector-set! glob-mac-cars (+ 1 f) '()) ; the null objects signal the end of a list, so here represents an empty list
        (vector-set! glob-mac-cdrs (+ 1 f) '())
        (set-register-contents glob-mac 'free (make-pointer (+ 2 f))) ; updating free
        (make-pointer f) ; returning the environment
    )
    )


;; Helper function. Scans a table of vars and values, looking for an instance of var. Returns the corresponding sublist of values, with the desired value on top. Returns #f if var isn't found. Note that if the variable is present, this won't return #f, even if the value of that variable is #f!
(define (scan-frame var vars values)
    (cond ((null? vars)
            #f)
          ((eq? var (vector-ref glob-mac-cars (address vars)))
            values)
          (else (scan-frame var (vector-ref glob-mac-cdrs (address vars)) (vector-ref glob-mac-cdrs (address values))))))

;; Helper function. Adds a var and value to a frame. Any pointers to the frame will still be valid, but pointers to the "head" of the var and value lists will be stale.
(define (add-binding var value frame)
    (let ((f (address (get-register-contents glob-mac 'free))))
    (vector-set! glob-mac-cars f var)
    (vector-set! glob-mac-cdrs f (frame-vars frame))
    (vector-set! glob-mac-cars (+ 1 f) value)
    (vector-set! glob-mac-cdrs (+ 1 f) (frame-values frame))
    (vector-set! glob-mac-cars (address frame) (make-pointer f))
    (vector-set! glob-mac-cdrs (address frame) (make-pointer (+ 1 f)))
    (set-register-contents glob-mac 'free (make-pointer (+ 2 f))))
    var)

;; Our standard API.
(define (lookup-var-value var env)
    (if (null? env)
        (begin (newline) (display var) (error "Variable not found by lookup-var-value!"))
        (let ((value-list (scan-frame var (frame-vars (top-frame env))
                                     (frame-values (top-frame env)))))
            (if value-list
                (vector-ref glob-mac-cars (address value-list))
                (lookup-var-value var (enclosing-env env))))))


(define (set-var-value! var value env)
    (if (null? env)
        (error "Variable not found by set-var-value!")
        (let ((value-list (scan-frame var (frame-vars (top-frame env))
                                     (frame-values (top-frame env)))))
            (if value-list
                (let ((old-value (vector-ref glob-mac-cars (address value-list))))
                       (vector-set! glob-mac-cars (address value-list) value)
                                            old-value)
                (set-var-value! var value (enclosing-env env))))))

(define (define-var! var value env) 
      (let ((value-list (scan-frame var (frame-vars (top-frame env))
            (frame-values (top-frame env)))))
            (if value-list
              (vector-set! glob-mac-cars (address value-list) value)
                  (add-binding var value (top-frame env)))))


(define (extend-environment vars args base-env)
    ; used to be a one-liner: (cons (cons vars args) base-env)
    (let ((f (address (get-register-contents glob-mac 'free))))
        (set-register-contents glob-mac 'free (make-pointer (+ f 2)))
            (vector-set! glob-mac-cars f vars)
                (vector-set! glob-mac-cdrs f args)
                    (vector-set! glob-mac-cars (+ f 1) (make-pointer f))
                        (vector-set! glob-mac-cdrs (+ f 1) base-env)
                            (make-pointer (+ f 1))))


;####################################
;# RML MACHINE OPS & HELPER FUNCTIONS
;####################################

; operations for RML machine
(define machine-make-environment (cons 'make-environment make-environment))
(define machine-define-var! (cons 'define-var! define-var!))
(define machine-lookup-var-value (cons 'lookup-var-value lookup-var-value))
(define machine-make-pointer (cons 'make-pointer make-pointer))
(define machine-vector-ref (cons 'vector-ref (lambda (vec pointer) (vector-ref vec (address pointer)))))
(define machine-vector-set! (cons 'vector-set! (lambda (vec pointer value) (vector-set! vec (address pointer) value))))
(define make-null (cons 'make-null (lambda () '())))
(define increment-pointer (cons 'increment-pointer (lambda (pointer) (make-pointer (+ (address pointer) 1)))))
(define other-ops (list (cons 'eq? eq?) (cons 'null? null?) (cons 'display display) (cons '- -) (cons '+ +) (cons '= =) (cons 'symbol? symbol?) (cons 'number? number?) (cons 'integer? integer?)))
; labels are just symbol in my RML simulator


(define (get-op-list) (append (list machine-lookup-var-value machine-make-environment machine-define-var! machine-make-pointer machine-vector-ref machine-vector-set! make-null increment-pointer) other-ops)) ; returns a list of all ops to be used in special machine



;; RML code for constructing '(10 20 30):

(define (get-RML)
  '(
    (assign (reg env) (op make-environment))

    (assign (reg temp-var) (op make-null)) ; making ((define x (+ 1 3) '()) x '())
    (assign (reg program) CONS (const 3) (reg temp-var))
    (assign (reg program) CONS (const 1) (reg program))
    (assign (reg program) CONS (const +) (reg program))
    (assign (reg program) CONS (reg program) (reg temp-var))
    (assign (reg program) CONS (const x) (reg program))
    (assign (reg program) CONS (const define) (reg program))
    (assign (reg temp-var) CONS (const x) (reg temp-var))
    (assign (reg program) CONS (reg program) (reg temp-var))


    ;(assign (reg temp-var) (op make-null))
    ;(assign (reg program) CONS (const 4) (reg temp-var)) ; creating ((define x 4))
    ;(assign (reg program) CONS (const x) (reg program))
    ;(assign (reg program) CONS (const define) (reg program))

    ;(assign (reg temp-var) CONS (const x) (reg temp-var))
    ;(assign (reg program) CONS (reg program) (reg temp-var))


    eval-loop
        (test (op null?) (reg program)) ; tests if program is null
        (branch (label done)) ; if so, goes to label done
        (assign (reg continue) (label eval-loop))
        (assign (reg exp) CAR (reg program))
        (assign (reg program) CDR (reg program))
        (goto (label eval))

    eval ; testing to see what to use to evaluate exp (expression)
        (test (op number?) (reg exp))
        (branch (label ev-self-evalutating))
        (test (op symbol?) (reg exp))
        (branch (label ev-variable))
        (assign (reg temp-var) CAR (reg exp))
        (test (op eq?) (const define) (reg temp-var))
        (branch (label ev-define))

        (assign (reg temp-1) CADR (reg exp)) ; TEMPORARY for evaluating ((define x (+ 1 3) '()) x '())
        (assign (reg temp-2) CADDR (reg exp))
        (assign (reg val) (op +) (reg temp-1) (reg temp-2))
        (goto (reg continue))

    ev-self-evalutating
        (assign (reg val) (reg exp))
        (goto (reg continue)) ; goes to the label stored in continue after any evaluation

    ev-variable
        (assign (reg val) (perform (op lookup-var-value) (reg exp) (reg env))) ; looking up the value of the variable in the environment
        (goto (reg continue)) ; goes to the label stored in continue after any evaluation

    ev-define
        (push (reg exp)) ; saving the state of exp and continue
        (push (reg continue))
        (assign (reg exp) CADDR (reg exp)) ; getting the sub expression part of (define x sub-expr) !!! Has to be CADDR
        (assign (reg continue) (label ev-define-2)) ; setting continue
        (goto (reg eval)) ; evaluating the sub expression
    ev-define-2 ; second define label so it can assign the first argument to the result of evaluating second argument
        (pop (reg continue)) ; getting continue back
        (pop (reg exp))
        (assign (reg temp-var) CADR (reg exp)) ; getting the to-define part of (define to-define sub-expr)
        (perform (op define-var!) (reg temp-var) (reg val) (reg env)) ; defining the variable with the result of the evaluated sub expression
        (goto (reg continue)) ; finally goes to continue


    cons  ; inputs: p-1, p-2
        (perform (op vector-set!) (reg the-cars) (reg free) (reg p-1))
        (perform (op vector-set!) (reg the-cdrs) (reg free) (reg p-2))
        (assign (reg p-val) (reg free)) ; assignes p-val to free, allows user to get the pointer that points to the cons pair
        (assign (reg free) ((op increment-pointer) (reg free))) ; increments free
        (goto (label p-cont))

    car  ; input: p-1
        (assign (reg p-val) (perform (op vector-ref) (reg the-cars) (reg p-1)))
        (perform (op vector-ref) (reg the-cars) (reg p-1))
        (goto (reg p-cont))

    cdr ; input: p-1
        (assign (reg p-val) (perform (op vector-ref) (reg the-cdrs) (reg p-1)))
        (perform (op vector-ref) (reg the-cdrs) (reg p-1))
        (goto (reg p-cont))

    done

    ))



(define (post-process machine)
    ; function that sets the machine up for the special machine
    (set-register-contents machine 'the-cars glob-mac-cars)
    (set-register-contents machine 'the-cdrs glob-mac-cdrs)
    (set-register-contents machine 'free (make-pointer 0))
    (set-register-contents machine 'p-cont '(label done))

    machine ; have to return modified machine back

    )

(define (make-special-machine) (post-process (make-machine '(temp-1 temp-2 program temp-var continue val env exp argl the-cars the-cdrs free p-1 p-2 p-val p-cont) (get-op-list) (precompile-RML (get-RML)))))


(define glob-mac (make-special-machine))
(run glob-mac)

(get-register-contents glob-mac 'exp)
(get-register-contents glob-mac 'val)
glob-mac-cars
glob-mac-cdrs

