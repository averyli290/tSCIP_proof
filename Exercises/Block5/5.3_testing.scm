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

