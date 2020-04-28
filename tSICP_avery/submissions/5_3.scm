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

; operations for RML machine
(define machine-make-pointer (cons 'make-pointer make-pointer))
(define machine-vector-ref (cons 'vector-ref (lambda (vec pointer) (vector-ref vec (address pointer)))))
(define machine-vector-set! (cons 'vector-set! (lambda (vec pointer value) (vector-set! vec (address pointer) value))))


(define (get-op-list) (list machine-make-pointer machine-vector-ref machine-vector-set!)) ; returns a list of all ops to be used in special machine
(define (get-RML) '(test-label))
(define (post-process machine)

    (set-register-contents machine 'the-cars (make-vector MAX_MEMORY_SIZE 'undef))
    (set-register-contents machine 'the-cdrs (make-vector MAX_MEMORY_SIZE 'undef))
    (set-register-contents machine 'free (make-pointer 0))

    machine ; return the machine when done

    )

(define (make-special-machine) (post-process (make-machine '(a b c the-cars the-cdrs free) (list machine-make-pointer machine-vector-ref machine-vector-set!) (get-RML))))

; test 1: With trivial RML code:
(define (get-RML) '())

; you should be able to run:


(define m (make-special-machine))
(run m)
(get-register-contents m 'free) ; returns (p . 0)
(vector-ref (get-register-contents m 'the-cars) 3)  ; returns whatever default vector value you used, assuming your MAX-MEMORY-SIZE > 3


; **********************************
; test 2: With RML code:

(define (get-RML) '(
    (perform (op vector-set!) (reg the-cars) (reg free) (const 5))
    (assign (reg a) (op vector-ref) (reg the-cars) (reg free))

    (assign (reg b) (const 2))
    (assign (reg b) (op make-pointer) (reg b))
    (perform (op vector-set!) (reg the-cdrs) (reg b) (const 5)
    ))
)

; you should be able to run:
(define m (make-special-machine))
(run m)
(get-register-contents m 'a) ; returns 5
(get-register-contents m 'the-cdrs) ; returns #(undefined undefined 5 undefined undefined undefined...)

