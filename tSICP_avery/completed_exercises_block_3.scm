;;; BEGIN: 3.4

(define let
    (lambda (var1 var2 ... varn)
            (body1 body2 ... bodym)))

;;; END: 3.4

;;; BEGIN: 3.6

(define (make-account balance)
    (define (withdraw amount)
        (set! balance (- balance amount)))
    (define (deposit amount)
        (set! balance (+ balance amount)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'get-balance) balance)
            (else (error "unknown request!"))))
    dispatch)

;;; END: 3.6

;;; BEGIN: 3.7

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

;;; END: 3.7

;;; BEGIN: 3.8

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

;;; BEGIN 3.8
