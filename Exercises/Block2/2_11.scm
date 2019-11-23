(define (flatmap proc seq)
    (fold-right append '()
        (map proc seq)))

(define (elt-of-set? obj set)
    (if (null? set)
        #f
        (if (= (car set) obj)
            #t
            (elt-of-set? obj (cdr set))))
    )

(define (add-to-set obj set)
    (if (not (elt-of-set? obj set))
        (cons obj set)
        set
        )
    )

(define (union set-1 set-2)
    (fold-right (lambda (elt lst) (if (not (elt-of-set? elt lst))
                                    (append lst (list elt))
                                    lst))
                set-1
                set-2))


(define (intersection set-1 set-2)
    (filter (lambda (elt1) 
                (fold-right
                        (lambda (arg1 arg2) (or arg1 arg2))
                        #f
                        (map (lambda (elt2) (= elt1 elt2))
                             set-2)))
            set-1))


