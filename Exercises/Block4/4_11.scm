(define (make-stack)
    (let ((stack '()))

        (define (push value)
            (set! stack (cons value stack)))

        (define (pop)
            (let ((temp stack))
                (set! stack (cdr temp))
                (car temp)))

        (define (empty?)
            (eq? stack '()))

        (define (dispatch m)
            (cond ((eq? 'push m) push)
                  ((eq? 'pop m) pop)
                  ((eq? 'empty? m) empty?)))

        dispatch))
