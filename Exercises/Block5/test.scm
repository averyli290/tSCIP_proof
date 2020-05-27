(load "RM_simulator.scm")
(load "helper-functions.scm")
(load "test_RML.scm")


;#####################
;# VECTORS & REGISTERS
;#####################

(define MAX_MEMORY_SIZE 400)

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

;############
;# PRIMITIVES
;############

(define primitive-table '()) ; the same table setup, but renamed for primitives
(define (set-primitive-table prim-name prim-proc)
    (set! primitive-table (cons (cons prim-name prim-proc) primitive-table))) ; Adds procedure to table
(define (set-prim-name prim-name prim-proc)
    (cons name prim-proc))
(define (get-prim-name expr)
    (car expr))
(define (get-contents expr)
    (cdr expr))
(define (get-primitive-table prim-name)
    (define (inner my-table)
        (cond ((null? my-table) 'none)
              ((eq? (get-prim-name (car my-table)) prim-name) (get-contents (car my-table)))
              (else (inner (cdr my-table)))))
    (inner primitive-table))


(define (primitive-apply primitive-name arglist) ; exposed in RML code as level 0 proc
    (define (convert-to-lisp-list l pointer)
      ; converts a pointer that points to an address in glob-mac-cars and glob-mac-cdrs (reg the-cars) and (reg the-cdrs) in the RML representation to the lisp version of the list
        (if (null? pointer)
            l
            (convert-to-lisp-list (cons (vector-ref glob-mac-cars (address pointer)) l) (vector-ref glob-mac-cdrs (address pointer)))))
    (define (reverse-list l) ; simple reverse function for a list
        (define (iter original result)
            (if (null? original)
                result
                (iter (cdr original) (cons (car original) result))))
        (iter l '())
        )
    (let ((lisp-arglist (reverse-list (convert-to-lisp-list '() arglist))))
      ; converts the pointer that points to the head of the list and then applies the primitive to it
        (apply (get-primitive-table primitive-name) lisp-arglist))
    )


;# Adding primitives to the table
(set-primitive-table '+ +)
(set-primitive-table '* *)
(set-primitive-table '- -)
(set-primitive-table '/ /)
(set-primitive-table '= (lambda (a b) (if (= a b) 1 0))) ; = in level 2 lisp will return 1 and 0 instead of true and false


;####################################
;# RML MACHINE OPS & HELPER FUNCTIONS
;####################################

; operations for RML machine
(define machine-make-environment (cons 'make-environment make-environment))
(define machine-define-var! (cons 'define-var! define-var!))
(define machine-lookup-var-value (cons 'lookup-var-value lookup-var-value))
(define machine-extend-environment (cons 'extend-environment extend-environment))
(define machine-make-pointer (cons 'make-pointer make-pointer))
(define machine-vector-ref (cons 'vector-ref (lambda (vec pointer) (vector-ref vec (address pointer)))))
(define machine-vector-set! (cons 'vector-set! (lambda (vec pointer value) (vector-set! vec (address pointer) value))))
(define machine-primitive-apply (cons 'primitive-apply primitive-apply))
(define make-null (cons 'make-null (lambda () '())))
(define increment-pointer (cons 'increment-pointer (lambda (pointer) (make-pointer (+ (address pointer) 1)))))
(define other-ops (list (cons 'eq? eq?) (cons 'null? null?) (cons 'display display) (cons '- -) (cons '+ +) (cons '= =) (cons 'symbol? symbol?) (cons 'number? number?) (cons 'integer? integer?)))
; labels are just symbol in my RML simulator


(define (get-op-list) (append (list machine-lookup-var-value machine-make-environment machine-define-var! machine-extend-environment machine-make-pointer machine-vector-ref machine-vector-set! machine-primitive-apply make-null increment-pointer) other-ops)) ; returns a list of all ops to be used in special machine



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

(define (get-registers) '(the-cars the-cdrs free temp-1 temp-2 temp-var temp-val null exp env proc val unev argl continue program p-1 p-2 p-val p-cont global-env))
(define (make-special-machine) (post-process (make-machine (get-registers) (get-op-list) (precompile-RML (get-RML)))))


(define glob-mac (make-special-machine))
(run glob-mac)

glob-mac-cars
glob-mac-cdrs

(get-register-contents glob-mac 'val)
;(cadr stack)
