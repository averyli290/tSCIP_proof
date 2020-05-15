(load "RM_simulator.scm")
(load "helper-functions.scm")
(load "5_13_RML.scm")


;#####################
;# VECTORS & REGISTERS
;#####################

(define MAX_MEMORY_SIZE 25)

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



;; Convert a nested list of list of... into vector format.
;; Input f is the current free index.
;; Returns the next free index.
(define (lisp-to-vectors obj f v-cars v-cdrs)
    (define (setup-cars obj f)
        (cond ((not (pair? (car obj)))
                (vector-set! v-cars f (car obj))
                (+ 1 f))
              (else
                (vector-set! v-cars f (make-pointer (+ 1 f)))
                (lisp-to-vectors (car obj) (+ 1 f) v-cars v-cdrs))))

    (cond ((not (pair? (cdr obj)))
            (vector-set! v-cdrs f (cdr obj))
            (setup-cars obj f))
          (else
            (let ((new-free (setup-cars obj f))) ; do cars before cdrs
            (vector-set! v-cdrs f (make-pointer new-free))
            (lisp-to-vectors (cdr obj) new-free v-cars v-cdrs)))))



(define (post-process machine)
    ; loading the program in
    ;(define free-index 0) ; making value for storing free index
    (define free-index (lisp-to-vectors (the-program) 0 glob-mac-cars glob-mac-cdrs))
    (set-register-contents machine 'program (make-pointer 0))

    ; function that sets the machine up for the special machine
    (set-register-contents machine 'the-cars glob-mac-cars)
    (set-register-contents machine 'the-cdrs glob-mac-cdrs)
    (set-register-contents machine 'free (make-pointer free-index))
    (set-register-contents machine 'p-cont '(label done))
    (set-register-contents machine 'null '())

    machine ; have to return modified machine back

    )

(define (get-registers) '(the-cars the-cdrs free temp-1 temp-2 temp-var temp-val null exp env proc val unev argl continue program p-1 p-2 p-val p-cont))
;(define (make-special-machine) (post-process (make-machine '(null temp-1 temp-2 program temp-var continue val env exp argl the-cars the-cdrs free p-1 p-2 p-val p-cont) (get-op-list) (precompile-RML (get-RML)))))
(define (make-special-machine) (post-process (make-machine (get-registers) (get-op-list) (precompile-RML (get-RML)))))


(define glob-mac (make-special-machine))
(run glob-mac)

glob-mac-cars
glob-mac-cdrs
(get-register-contents glob-mac 'val)


