(define (make-reg)
    (let ((contents 'undefined))

        (define (set-contents value)
            (set! contents value))

        (define (get-contents)
            contents)

        (define (dispatch m)
            (cond ((eq? 'set-contents m) set-contents)
                  ((eq? 'get-contents m) get-contents)))

        dispatch))

