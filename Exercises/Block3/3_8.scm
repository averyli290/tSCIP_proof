(define (make-account balance password)
    (define password-wrong-inarow 0)
    (define (withdraw amount)
        (set! balance (- balance amount)))
    (define (deposit amount)
        (set! balance (+ balance amount)))
    (define (dispatch m password-input)
        (if (eq? password-input password)
            (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'get-balance) balance)
                (else (error "unknown request!")))
            (if (= (+ 1 password-wrong-inarow) 5)
                (begin (set! password-wrong-inarow 0) (lambda (x) (display "The authorities have been notified.")))
                (begin (set! password-wrong-inarow (+ 1 password-wrong-inarow)) (lambda (x) (display "Incorrect password."))))))
    dispatch)
