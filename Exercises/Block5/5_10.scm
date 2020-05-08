(load "RM_simulator_5_10.scm")
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
        (error "Variable not found by lookup-var-value!")
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
    ;(newline)
    ;(display (scan-frame var (frame-vars (top-frame env)) (frame-values (top-frame env))))
    ;(display (frame-vars (top-frame env)))
    ;(display "env: ")
    ;(display (top-frame env))
    ;(newline)
    ;(define env (top-frame env-pointer))
    ;(newline)
    ;(display (frame-vars (top-frame env)))
    ;(define env (top-frame env-pointer))
    ;(frame-vars (top-frame env))
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
(define other-ops (list (cons 'eq? eq?) (cons '- -) (cons '+ +) (cons '= =) (cons 'integer? integer?)))
; labels are just symbol in my RML simulator


(define (get-op-list) (append (list machine-lookup-var-value machine-make-environment machine-define-var! machine-make-pointer machine-vector-ref machine-vector-set! make-null increment-pointer) other-ops)) ; returns a list of all ops to be used in special machine



;; RML code for constructing '(10 20 30):

(define (get-RML)
  '(
    (assign (reg env) (op make-environment))
    (perform (op define-var!) (const r) (const 13) (reg env))
    (perform (op define-var!) (const s) (const 14) (reg env))
    (assign (reg val) (op lookup-var-value) (const s) (reg env))
    (assign (reg exp) CONS (const x) (op make-null))
    (assign (reg exp) CONS (const y) (reg exp))
    (assign (reg argl) CONS (const 17) (op make-null))
    (assign (reg argl) CONS (const 18) (reg argl))
    (goto (label done))
    (assign (reg env) (op extend-environment) (reg exp) (reg argl) (reg env))
    ;done

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

(define (make-special-machine) (post-process (make-machine '(env val exp argl a b c the-cars the-cdrs free p-1 p-2 p-val p-cont) (get-op-list) (precompile-RML (get-RML)))))


(define glob-mac (make-special-machine))
(run glob-mac)

;(get-register-contents glob-mac 'p-cont)
glob-mac-cars
glob-mac-cdrs
;(get-register-contents test 'the-cdrs)
;(get-register-contents test 'free)
;(get-register-contents test 'a)
;(get-register-contents test 'b)
;(get-register-contents test 'c)

(define (iter l)
    (if (null? l)
        'dummy
        (begin (newline) (display (car l)) (iter (cdr l)))))

;(iter (car glob-mac))

