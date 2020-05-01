(load "RM_simulator.scm")



;#####################
;# VECTORS & REGISTERS
;#####################

(define MAX_MEMORY_SIZE 20)

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

(define (top-frame env) (vector-ref glob-mac-cars (address env)))
(define (enclosing-env env) (vector-ref glob-mac-cdrs (address env)))
(define (frame-vars frame) (vector-ref glob-mac-cars (address frame)))
(define (frame-values frame) (vector-ref glob-mac-cdrs (address frame)))


;;; frame procedures
;(define (make-environment)
;    (let ((f (address (get-register-contents glob-mac 'free))))
;    (vector-set! glob-mac-cars ))
;    )


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
        (error "Variable not found by lookup-var-value!")
        (let ((value-list (scan-frame var (frame-vars (top-frame env))
                                     (frame-values (top-frame env)))))
            (if value-list
                (vector-ref glob-mac-cars (address value-list))
                (lookup-var-value var (enclosing-env env))))))


;(define (set-var-value! var value env)
;    (if (null? env)
;        (error "Variable not found by set-var-value!")
;        (let ((value-list (scan-frame var (frame-vars (top-frame env))
;                                     (frame-values (top-frame env)))))
;            (if value-list
;                (let ((old-value (vector-ref glob-mac-cars (address value-list))))
;                       (vector-set! glob-mac-cars (address value-list) value)
;                                            old-value)
;                (set-var-value! var value (enclosing-env env))))))
;



;####################################
;# RML MACHINE OPS & HELPER FUNCTIONS
;####################################

; operations for RML machine
(define machine-make-pointer (cons 'make-pointer make-pointer))
(define machine-vector-ref (cons 'vector-ref (lambda (vec pointer) (vector-ref vec (address pointer)))))
(define machine-vector-set! (cons 'vector-set! (lambda (vec pointer value) (vector-set! vec (address pointer) value))))
(define make-null (cons 'make-null (lambda () '())))
(define increment-pointer (cons 'increment-pointer (lambda (pointer) (make-pointer (+ (address pointer) 1)))))
(define other-ops (list (cons 'eq? eq?) (cons '- -) (cons '+ +) (cons '= =) (cons 'integer? integer?)))
; labels are just symbol in my RML simulator


(define (get-op-list) (append (list machine-make-pointer machine-vector-ref machine-vector-set! make-null increment-pointer) other-ops)) ; returns a list of all ops to be used in special machine


;; RML code for constructing '(10 20 30):

(define (get-RML) '(
    start
        (assign (reg p-1) (const 30))
        (assign (reg p-2) (op make-null))
        (assign (reg p-cont) (label start-a))
        (goto (label cons))
        start-a

        (assign (reg p-1) (const 20))
        (assign (reg p-2) (reg p-val)) 
        (assign (reg p-cont) (label start-b))
        (goto (label cons))
        start-b

        (assign (reg p-1) (const 10))
        (assign (reg p-2) (reg p-val))
        (assign (reg p-cont) (label start-c))
        (goto (label cons))
        start-c

        (assign (reg a) (reg p-val))
        
    pre-list-length
    (assign (reg c) (label done)) ; acts as continue label
    (push (reg a)) ; pushing a to the stack so we can use a as a variable knowing that the value of a is saved
    (test (op eq?) (perform (op vector-ref) (reg the-cars) (reg a)) (perform (op make-null))) ; checks if c is just empty list 
    (branch (label done))

    list-copy
        (test (op eq?) (perform (op vector-ref) (reg the-cdrs) (reg a)) (perform (op make-null)))
        (branch (label base-case))
        (push (reg c)) ; pushing the continue label to the stack first, then the pointer
        (push (reg a))
        (assign (reg c) (label post-rec-copy))
        (assign (reg a) (perform (op vector-ref) (reg the-cdrs) (reg a))) ; set a to be the next pointer in the list
        (goto (label list-copy))

    post-rec-copy
        (pop (reg a)) ; gets the pointer at the top of the stack that it expects to point to the right values
        (assign (reg p-1) (perform (op vector-ref) (reg the-cars) (reg a))) ; getting the value to cons by looking in the-cars 
        (assign (reg p-2) (reg b))
        (assign (reg p-cont) (label post-rec-copy-1)) ; just regular cons things
        (goto (label cons))

    post-rec-copy-1
        (assign (reg b) (reg p-val)) ; sets the value of b to the new cons list
        (pop (reg c)) ; gets the value of c to go to the next label 
        (goto (reg c))

    base-case
        (push (reg c)) ; pushing c and a to the stack in order to set up the stack for the recursive construction steps
        (push (reg a))
        (assign (reg b) (op make-null)) ; b starts at being an empty list
        (goto (reg c))


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
        (pop (reg a)) ; getting the value of a backj


))


(define (post-process machine)
    ; function that sets the machine up for the special machine
    (define glob-mac-cars (make-vector MAX_MEMORY_SIZE 'undef))
    (define glob-mac-cdrs (make-vector MAX_MEMORY_SIZE 'undef))
    ;(set-register-contents machine 'the-cars (make-vector MAX_MEMORY_SIZE 'undef))
    ;(set-register-contents machine 'the-cdrs (make-vector MAX_MEMORY_SIZE 'undef))
    (set-register-contents machine 'the-cars glob-mac-cars)
    (set-register-contents machine 'the-cdrs glob-mac-cdrs)
    (set-register-contents machine 'free (make-pointer 0))

    (set! glob-mac-cdrs machine)

    machine ; have to return modified machine back

    )

(define (make-special-machine) (post-process (make-machine '(a b c the-cars the-cdrs free p-1 p-2 p-val p-cont) (get-op-list) (get-RML))))

(define test (make-special-machine))
(run test)

;(get-register-contents test 'the-cars)
;(get-register-contents test 'the-cdrs)
;(get-register-contents test 'free)
;(get-register-contents test 'a)
;(get-register-contents test 'b)
;(get-register-contents test 'c)


