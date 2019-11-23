;(fold-right (lambda (x y) (+ (* 2 x) (* 3 y))) 1 '(1 2 4))

(define (accumulate start-value combine-fn l)
    (if (null? (cdr l))
        (combine-fn (car l) start-value)
        (combine-fn (car l) (accumulate start-value combine-fn (cdr l)))
        ) 
    )

(define (count-leaves tree)
    (accumulate 0 + (map (lambda (x) (if (not (pair? x))
                                        1 
                                        (+ (count-leaves (list (car x))) (count-leaves (cdr x)))
                                      )
                        )
                       tree
                    ) 
                )
    )

;(define test '(1 2 (3 4 5) (6 7)))
;(count-leaves test)
