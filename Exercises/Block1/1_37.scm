(define (accumulate start-value combine-fn l)
    (if (null? (cdr l))
        (combine-fn (car l) start-value)
        (combine-fn (car l) (accumulate start-value combine-fn (cdr l)))
        ) 
    )

; (accumulate 0 + (list 1 2 3))
; (accumulate '() cons (list 1 2 3))
