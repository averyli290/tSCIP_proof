(load "RM_simulator.scm")


;#####################
;# VECTORS & REGISTERS
;#####################

(define MAX_MEMORY_SIZE 20)

(define (vector-ref vec index) ; gets the value of an element at a specified index
    (cond ((null? vec) (error "index referenced not in range of vec"))
          ((= 0 index) (car vec))
          (else (vector-ref (cdr vec) (- index 1)))))

(define (vector-set! vec index value) ; sets the value of an element at a specified index
    (cond ((null? vec) (error "index referenced not in range of vec"))
          ((= 0 index) (set-car! vec value))
          (else (vector-set! (cdr vec) (- index 1) value))))

(define (make-vector size default-value); creates a vector with length size with all the entries set to default-value
    (if (= size 0)
        '()
        (cons default-value (make-vector (- size 1) default-value))))


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
;(define machine-vector-set! (cons 'vector-set! (lambda (vec pointer value) (display 0))))
(define make-null (cons 'make-null (lambda () '())))
(define increment-pointer (lambda (pointer) (make-pointer (+ (address pointer) 1))))
;(define increment-pointer (lambda (pointer) (set! pointer (make-pointer (+ (address pointer) 1)))))
(define other-ops (list (cons '= =) (cons 'integer? integer?) (cons 'increment-pointer increment-pointer)))
; labels are just symbol in my RML simulator


(define (get-op-list) (append (list machine-make-pointer machine-vector-ref machine-vector-set! make-null) other-ops)) ; returns a list of all ops to be used in special machine

(define (get-RML)
  '(
    cons
    (perform (op vector-set!) (reg the-cars) (reg free) (reg p-1))
    (perform (op vector-set!) (reg the-cdrs) (reg free) (reg p-2))
    (test (op integer?) (reg p-cont)) ; we don't want to execute (goto (reg p-cont)) on the first pass through, when (reg p-cont) is (const 0)
    (branch (label cons-end))
    (goto (reg p-cont))
    cons-end
    (assign (reg p-val) (reg free))
    (assign (reg free) ((op increment-pointer) (reg free)))

    car
    (test (op integer?) (reg p-1))
    (branch (label car-end)) ; same as above branch
    (assign (reg p-val) (perform (op vector-ref) (reg the-cars) (reg p-1)))
    (perform (op vector-ref) (reg the-cars) (reg p-1))
    (goto (reg p-cont))
    car-end

    cdr
    (test (op integer?) (reg p-1))
    (branch (label cdr-end)) ; same as above branch
    (assign (reg p-val) (perform (op vector-ref) (reg the-cdrs) (reg p-1)))
    (goto (reg p-cont))
    cdr-end
    
    start
    (assign (reg p-1) (const 1))
    (assign (reg p-2) (const 2))
    (assign (reg p-cont) (label start-1))
    (goto (label cons))
    (perform (op vector-set!) (reg the-cars) (reg free) (reg p-1))
    (perform (op vector-set!) (reg the-cdrs) (reg free) (reg p-2))
    (goto (reg p-cont))
    start-1
    (assign (reg a) (reg p-val))

    car-test
    (assign (reg p-1) (reg a))
    (assign (reg p-cont) (label car-test-1))
    (goto (label cdr))
    car-test-1
    (assign (reg bool-reg) (reg p-val))
    
    )
  )


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

        (goto (label done))

    cons  ; inputs: p-1, p-2
        ;; YOUR CODE HERE
        (perform (op vector-set!) (reg the-cars) (reg free) (reg p-1))
        (perform (op vector-set!) (reg the-cdrs) (reg free) (reg p-2))
        (test (op integer?) (reg p-cont)) ; we don't want to execute (goto (reg p-cont)) on the first pass through, when (reg p-cont) is (const 0), skips goto
        (branch (label cons-end))
        (assign (reg p-val) (reg free)) ; assignes p-val to free, allows user to get the pointer that points to the cons pair
        (assign (reg free) ((op increment-pointer) (reg free))) ; increments free
        (goto (label p-cont))
        cons-end



    car  ; input: p-1
        ;; YOUR CODE HERE
        (test (op integer?) (reg p-cont))
        (branch (label car-end)) ; same as above branch
        (assign (reg p-val) (perform (op vector-ref) (reg the-cars) (reg p-1)))
        (perform (op vector-ref) (reg the-cars) (reg p-1))
        (goto (reg p-cont))
        car-end

    cdr ; input: p-1
        ;; YOUR CODE HERE
        (test (op integer?) (reg p-cont))
        (branch (label cdr-end)) ; same as above branch
        (assign (reg p-val) (perform (op vector-ref) (reg the-cdrs) (reg p-1)))
        (perform (op vector-ref) (reg the-cdrs) (reg p-1))
        (goto (reg p-cont))
        cdr-end

    done



))



(define (post-process machine)

    (set-register-contents machine 'the-cars (make-vector MAX_MEMORY_SIZE 'undef))
    (set-register-contents machine 'the-cdrs (make-vector MAX_MEMORY_SIZE 'undef))
    (set-register-contents machine 'free (make-pointer 0))
    (set-register-contents machine 'p-cont 0)
    (set-register-contents machine 'p-1 0)
    machine ; have to return modified machine back
    )

(define (make-special-machine) (post-process (make-machine '(a b c the-cars the-cdrs free p-1 p-2 p-val p-cont bool-reg) (get-op-list) (get-RML))))

(define test (make-special-machine))
;
;(get-register-contents test 'p-1)
;(get-register-contents test 'p-2)
;(get-register-contents test 'p-val)
;(get-register-contents test 'p-cont)
;(get-register-contents test 'the-cars)
;(get-register-contents test 'the-cdrs)
;(get-register-contents test 'free)
;(get-register-contents test 'bool-reg)
;
(run test)
;(get-register-contents test 'p-val)

;(get-register-contents test 'p-1)
;(get-register-contents test 'p-2)
;(get-register-contents test 'p-val)
;(get-register-contents test 'p-cont)
(get-register-contents test 'the-cars)
(get-register-contents test 'the-cdrs)
(get-register-contents test 'free)
(get-register-contents test 'a)
(get-register-contents test 'b)
(get-register-contents test 'c)



;; RML code for constructing '(10 20 30):

