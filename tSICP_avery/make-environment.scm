(define (make-environment)
    (let ((f (address (get-register-contents glob-mac 'free))))
        (vector-set! glob-mac-cdrs f 'null)
        (vector-set! glob-mac-cdrs (+ 1 f) 'null)
        (vector-set! glob-mac-cars (+ 2 f) (make-pointer f))
        (vector-set! glob-mac-cdrs (+ 2 f) (make-pointer (+ 1 f)))
        (set-register-contents glob-mac 'free (make-pointer (+ 3 f))) ; updating free
        (list (make-pointer (+ 2 f))) ; returning the environment
        ;(list (cons (make-pointer f) (make-pointer (+ 1 f)))) ; returning the environment
    )
    )