(define (make-account balance password)
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
            (display "Incorrect password!")))
    dispatch)
